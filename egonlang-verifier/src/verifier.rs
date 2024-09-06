use egonlang_core::prelude::*;
use egonlang_errors::{EgonErrorS, EgonResultMultiSpannedErr, EgonTypeError};
use egonlang_types::{
    egon_unknown,
    type_env::{TypeEnv, TypeEnvValue},
    EgonType, T,
};
use span::Span;
use tracelog::{log_expr, log_identifier, log_stmt, log_type};

use crate::rules::{core::*, Rule};

/// Verify an AST [`Module`](egonlang_core::ast::Module) using the registered
/// [`Rule`](crate::rules::Rule) set
///
/// ```
/// use egonlang_core::prelude::*;
/// use egonlang_verifier::prelude::*;
///
/// // Verifier with the default rule set
/// let verifier = Verifier::default();
///
/// let mut module = parse("let a = 123;", 0).expect("Unable to parse");
///
/// let result = verify_module(&mut module);
///
/// matches!(result, Ok(()));
/// ```
pub struct Verifier<'a> {
    rules: Vec<Box<dyn Rule<'a>>>,
    type_env: TypeEnv,
}

impl Default for Verifier<'_> {
    /// Create a [`Verifier`] with the [`Rule`] set
    fn default() -> Self {
        Self::new().with_default_rules()
    }
}

impl<'a> Verifier<'a> {
    /// Create a [`Verifier`] with no default [`Rule`](crate::rules::Rule) set
    pub fn new() -> Self {
        Verifier {
            rules: Default::default(),
            type_env: TypeEnv::new(),
        }
    }

    /// Register a [`Rule`](crate::rules::Rule)
    ///
    /// ```ignore
    /// use egonlang_verifier::prelude::*;
    ///
    /// let verifier = Verifier::new().with_rule(TypeMismatchInfixRule);
    /// ```
    pub fn add_rule<R: Rule<'a> + 'static>(&mut self, rule: R) {
        self.rules.push(Box::new(rule));
    }

    /// Register the core language [`Rule`](crate::rules::Rule) set
    ///
    /// ```
    /// use egonlang_verifier::prelude::*;
    ///
    /// // These are the same
    /// let verifier = Verifier::new().with_default_rules();
    /// let verifier = Verifier::default();
    /// ```
    pub fn with_default_rules(mut self) -> Self {
        self.add_rule(TypeMismatchInfixRule);
        self.add_rule(TypeMismatchPrefixRule);
        self.add_rule(TypeMisMatchListItemsRule);
        self.add_rule(TypeMismatchOnDeclarationsRule);
        self.add_rule(DeclareConstWithoutValueRule);
        self.add_rule(ReassigningConstValueRule);
        self.add_rule(ReferencingUndefinedIdentifierRule);
        self.add_rule(DivideByZeroRule);
        self.add_rule(TypeMismatchFnReturnExprRule);
        self.add_rule(TypeMismatchIfCondExprRule);
        self.add_rule(TypeMismatchReassigningLetValuesRule);
        self.add_rule(TypeMismatchIfthenElseExprRule);
        self.add_rule(InvalidTypeAliasNameRule);
        self.add_rule(AssertTypeRule);
        self.add_rule(NoReturnOutsideBlockRule);
        self.add_rule(NoStmtsAfterReturnStmtRule);
        self.add_rule(NoNonCallableCalledRule);
        self.add_rule(TypeMismatchArgsInCallExprRule);
        self.add_rule(WrongNumberOfArgsCallingFnRule);

        self
    }

    /// Verify an AST [`Module`](egonlang_core::ast::Module) using the registered [`Rule`](crate::rules::Rule) set
    pub fn verify(&mut self, module: &mut ast::Module) -> EgonResultMultiSpannedErr<()> {
        let mut all_errs: Vec<EgonErrorS> = vec![];

        tracelog::tracelog!(
            label = verifier,verify;
            "Verifying module..."
        );

        for (stmt, stmt_span) in &mut module.stmts {
            if let Err(stmt_errs) = self.visit_stmt(stmt, stmt_span) {
                all_errs.extend(stmt_errs);
            }
        }

        if !all_errs.is_empty() {
            return Err(all_errs);
        }

        Ok(())
    }
}

impl<'a> Verifier<'a> {
    /// Resolves an identifier through all type environment scopes
    fn resolve_identifier(
        &self,
        identifier: &str,
        span: &Span,
    ) -> EgonResultMultiSpannedErr<TypeEnvValue> {
        tracelog::tracelog!(
            label = verifier, resolve_identifier;
            "(level: {}) Resolving type for identifier: {}",
            self.current_type_env_level(),
            log_identifier(&identifier)
        );

        let resolved_to = self.type_env.get(identifier);

        match &resolved_to {
            Some(resolved_to) => {
                tracelog::tracelog!(
                    label = verifier, resolve_identifier;
                    "(level: {}) Resolved identifier: {} to type: {}",
                    self.current_type_env_level(),
                    log_identifier(&identifier),
                    log_type(&resolved_to.of_type)
                );
            }
            None => {
                tracelog::tracelog!(
                    label = verifier, resolve_identifier;
                    "(level: {}) Unable to resolve identifier: {}. Generating undefined error.",
                    self.current_type_env_level(),
                    log_identifier(&identifier)
                );
            }
        }

        resolved_to.ok_or(vec![(
            EgonTypeError::Undefined(identifier.to_string()).into(),
            span.clone(),
        )])
    }

    /// Resolve an expression's type recursively.
    ///
    /// This will resolve value identifiers types as well.
    ///
    /// Example
    ///
    /// let a = 123;
    ///
    /// let b = false;
    ///
    /// (a, b,); // This expression's type resolves to (number, bool,)
    fn resolve_expr_type(
        &self,
        expr: &ast::Expr,
        span: &Span,
    ) -> EgonResultMultiSpannedErr<TypeEnvValue> {
        tracelog::tracelog!(
            label = verifier,resolve_expr_type;
            "(level: {}) Resolving type for expression {}",
            self.current_type_env_level(),
            log_expr(expr)
        );

        let resolved_type = match expr {
            ast::Expr::Call(expr_call) => {
                tracelog::tracelog!(
                    label = verifier,resolve_expr_type,expr_call;
                    "(level: {}) {} is a call expression",
                    self.current_type_env_level(),
                    log_expr(expr)
                );

                match self.resolve_expr_type(&expr_call.callee.0, span) {
                    Ok(callee_type) => {
                        if callee_type.of_type.is_function() {
                            tracelog::tracelog!(
                                label = verifier,resolve_expr_type,expr_call,resolve_callee;
                                "(level: {}) callee {} in call expression {} is a function",
                                self.current_type_env_level(),
                                log_expr(&expr_call.callee.0),
                                log_expr(expr)
                            );

                            Ok(callee_type.of_type.get_function_return().into())
                        } else {
                            tracelog::tracelog!(
                                label = verifier,resolve_expr_type,expr_call,resolve_callee;
                                "(level: {}) callee {} in call expression {} is not a function",
                                self.current_type_env_level(),
                                log_expr(&expr_call.callee.0),
                                log_expr(expr)
                            );

                            Ok(egon_unknown!().into())
                        }
                    }
                    Err(errs) => Err(errs),
                }
            }
            ast::Expr::Identifier(ident_expr) => {
                tracelog::tracelog!(
                    label = verifier,resolve_expr_type,expr_identifier;
                    "(level: {}) {} is an identifier expression",
                    self.current_type_env_level(),
                    log_expr(expr)
                );

                self.resolve_identifier(&ident_expr.identifier.name, span)
            }
            ast::Expr::List(list_expr) => {
                tracelog::tracelog!(
                    label = verifier,resolve_expr_type,expr_list;
                    "(level: {}) {} is a list expression",
                    self.current_type_env_level(),
                    log_expr(expr)
                );

                if list_expr.items.is_empty() {
                    return Ok(Type::unknown_list().into());
                }

                let (first_item_expr, _) = list_expr.items.first().unwrap().clone();
                let first_item_type = self.resolve_expr_type(&first_item_expr, span)?.of_type;

                Ok(Type::list(first_item_type).into())
            }
            ast::Expr::Tuple(tuple_expr) => {
                tracelog::tracelog!(
                    label = verifier,resolve_expr_type,expr_tuple;
                    "(level: {}) {} is a tuple expression",
                    self.current_type_env_level(),
                    log_expr(expr)
                );

                if tuple_expr.items.is_empty() {
                    return Ok(Type::tuple(vec![]).into());
                }

                let item_types: Vec<Type> = tuple_expr
                    .items
                    .clone()
                    .into_iter()
                    .map(|(x_expr, _)| self.resolve_expr_type(&x_expr, span).unwrap().of_type)
                    .collect();

                Ok(Type::tuple(item_types).into())
            }
            ast::Expr::Block(block_expr) => {
                tracelog::tracelog!(
                    label = verifier,resolve_expr_type,expr_block;
                    "(level: {}) {} is a block expression",
                    self.current_type_env_level(),
                    log_expr(expr)
                );

                if let Some(block_typeref) = &block_expr.typeref {
                    return Ok(TypeEnvValue {
                        of_type: block_typeref.clone(),
                        is_const: true,
                    });
                }

                if let Some((block_expr_return_expr, _)) = &block_expr.return_expr {
                    self.resolve_expr_type(block_expr_return_expr, span)
                } else {
                    let resolved_type = Type::unit();

                    tracelog::tracelog!(
                        label = verifier,resolve_expr_type,expr_identifier;
                        "(level: {}) Resolved block expression {} to type {}",
                        self.current_type_env_level(),
                        log_expr(block_expr),
                        log_type(&resolved_type)
                    );

                    return Ok(resolved_type.into());
                }
            }
            ast::Expr::Type(type_expr) => {
                tracelog::tracelog!(
                    label = verifier,resolve_expr_type,expr_type;
                    "(level: {}) {} is a type expression",
                    self.current_type_env_level(),
                    log_expr(expr)
                );

                if type_expr.0.is_builtin() {
                    tracelog::tracelog!(
                        label = verifier,resolve_expr_type,expr_type;
                        "(level: {}) Resolved type expression {} to builtin type {}",
                        self.current_type_env_level(),
                        log_expr(type_expr),
                        log_type(type_expr)
                    );

                    return Ok(type_expr.0.clone().into());
                }

                match self.resolve_identifier(type_expr.0.to_string().as_str(), span) {
                    Ok(type_env_value) => {
                        tracelog::tracelog!(
                            label = verifier,resolve_expr_type,expr_type;
                            "(level: {}) Resolved type expression {} to type {}",
                            self.current_type_env_level(),
                            log_expr(type_expr),
                            log_type(&type_env_value.of_type)
                        );

                        Ok(type_env_value.clone())
                    }
                    Err(errs) => Err(errs),
                }
            }
            ast::Expr::Assign(assign_expr) => {
                tracelog::tracelog!(
                    label = verifier,resolve_expr_type,expr_assign;
                    "(level: {}) {} is an assign expression",
                    self.current_type_env_level(),
                    log_expr(expr)
                );

                let name = &assign_expr.identifier.0.name;

                self.resolve_identifier(name, span)
            }
            ast::Expr::If(if_expr) => {
                tracelog::tracelog!(
                    label = verifier,resolve_expr_type,expr_if;
                    "(level: {}) {} is an if expression",
                    self.current_type_env_level(),
                    log_expr(expr)
                );

                let (then_expr, _) = &if_expr.then;
                let then_typeref = self.resolve_expr_type(then_expr, span)?;

                Ok(then_typeref)
            }
            ast::Expr::Unit => {
                tracelog::tracelog!(
                    label = verifier,resolve_expr_type,expr_unit;
                    "(level: {}) {} is an unit expression",
                    self.current_type_env_level(),
                    log_expr(expr)
                );

                Ok(Type::unit().into())
            }
            ast::Expr::Literal(literal) => {
                tracelog::tracelog!(
                    label = verifier,resolve_expr_type,expr_literal;
                    "(level: {}) {} is a literal expression",
                    self.current_type_env_level(),
                    log_expr(expr)
                );

                Ok(match literal {
                    ast::ExprLiteral::Bool(_) => Type::bool(),
                    ast::ExprLiteral::Number(_) => Type::number(),
                    ast::ExprLiteral::String(_) => Type::string(),
                }
                .into())
            }
            ast::Expr::Infix(infix) => {
                tracelog::tracelog!(
                    label = verifier,resolve_expr_type,expr_infix;
                    "(level: {}) {} is an infix expression",
                    self.current_type_env_level(),
                    log_expr(expr)
                );

                Ok(match infix.op {
                    ast::OpInfix::Add => Type::number(),
                    ast::OpInfix::Subtract => Type::number(),
                    ast::OpInfix::Multiply => Type::number(),
                    ast::OpInfix::Divide => Type::number(),
                    ast::OpInfix::Modulus => Type::number(),
                    ast::OpInfix::Less => Type::bool(),
                    ast::OpInfix::LessEqual => Type::bool(),
                    ast::OpInfix::Greater => Type::bool(),
                    ast::OpInfix::GreaterEqual => Type::bool(),
                    ast::OpInfix::Equal => Type::bool(),
                    ast::OpInfix::NotEqual => Type::bool(),
                    ast::OpInfix::LogicAnd => Type::bool(),
                    ast::OpInfix::LogicOr => Type::bool(),
                }
                .into())
            }
            ast::Expr::Prefix(prefix) => {
                tracelog::tracelog!(
                    label = verifier,resolve_expr_type,expr_prefix;
                    "(level: {}) {} is an prefix expression",
                    self.current_type_env_level(),
                    log_expr(expr)
                );

                Ok(match prefix.op {
                    ast::OpPrefix::Negate => Type::number(),
                    ast::OpPrefix::Not => Type::bool(),
                }
                .into())
            }
            ast::Expr::Fn(fn_) => {
                tracelog::tracelog!(
                    label = verifier,resolve_expr_type,expr_fn;
                    "(level: {}) {} is a function expression",
                    self.current_type_env_level(),
                    log_expr(expr)
                );

                Ok(Type::function(
                    Type::tuple(
                        fn_.params
                            .clone()
                            .into_iter()
                            .map(|((_, typeref), _)| typeref)
                            .collect(),
                    ),
                    fn_.return_type.clone().0,
                )
                .into())
            }
            ast::Expr::Range(_) => {
                tracelog::tracelog!(
                    label = verifier,resolve_expr_type,expr_range;
                    "(level: {}) {} is a range expression",
                    self.current_type_env_level(),
                    log_expr(expr)
                );

                Ok(Type::range().into())
            }
        };

        if let Ok(resolved_type) = &resolved_type {
            tracelog::tracelog!(
                label = verifier,resolve_expr_type;
                "(level: {}) Resolved expression {} to the type {}",
                self.current_type_env_level(),
                log_expr(expr),
                log_type(&resolved_type.of_type)
            );
        };

        resolved_type
    }

    fn visit_stmt(&mut self, stmt: &mut ast::Stmt, span: &Span) -> EgonResultMultiSpannedErr<()> {
        let mut errs: Vec<EgonErrorS> = vec![];

        let stmt_string = log_stmt(stmt);

        tracelog::tracelog!(label = verifier,visit_stmt; "{}", stmt_string);

        let result = match stmt {
            ast::Stmt::Expr(stmt_expr) => {
                tracelog::tracelog!(label = verifier,visit_stmt,stmt_expr; "{} is an expression statement", stmt_string);

                let (expr, expr_span) = &mut stmt_expr.expr;

                tracelog::tracelog!(
                    label = verifier,visit_stmt,stmt_expr;
                    "Checking expression {} from stmt {}",
                    log_expr(expr),
                    stmt_string
                );

                let expr_errs = self.visit_expr(expr, expr_span).err().unwrap_or_default();

                tracelog::tracelog!(
                    label = verifier,visit_stmt,stmt_expr,error;
                    "Expression {} from statement {} had {} errors",
                    log_expr(expr),
                    stmt_string,
                    expr_errs.len()
                );

                errs.extend(expr_errs);

                if !errs.is_empty() {
                    return Err(errs);
                }

                Ok(())
            }
            ast::Stmt::Assign(stmt_assign) => {
                tracelog::tracelog!(
                    label = verifier,visit_stmt,stmt_assign;
                    "{} is an assignment statement",
                    stmt_string
                );

                match (&mut stmt_assign.type_expr, &mut stmt_assign.value) {
                    (None, None) => {}
                    (None, Some((value_expr, value_span))) => {
                        let value_typeref = self.visit_expr(value_expr, value_span)?;

                        self.type_env.set(
                            &stmt_assign.identifier.0.name,
                            TypeEnvValue {
                                of_type: value_typeref.of_type,
                                is_const: stmt_assign.is_const,
                            },
                        );
                    }
                    (Some((type_expr, type_span)), None) => {
                        match self.visit_expr(type_expr, type_span) {
                            Ok(type_typeref) => {
                                self.type_env.set(
                                    &stmt_assign.identifier.0.name,
                                    TypeEnvValue {
                                        of_type: type_typeref.of_type,
                                        is_const: stmt_assign.is_const,
                                    },
                                );
                            }
                            Err(type_errs) => {
                                errs.extend(type_errs);
                            }
                        }
                    }
                    (Some((type_expr, type_span)), Some((value_expr, value_span))) => {
                        match self.visit_expr(type_expr, type_span) {
                            Ok(type_typeref) => {
                                self.visit_expr(value_expr, value_span)?;

                                self.type_env.set(
                                    &stmt_assign.identifier.0.name,
                                    TypeEnvValue {
                                        of_type: type_typeref.of_type,
                                        is_const: stmt_assign.is_const,
                                    },
                                );
                            }
                            Err(type_errs) => {
                                errs.extend(type_errs);
                            }
                        }
                    }
                };

                Ok(())
            }
            ast::Stmt::TypeAlias(stmt_type_alias) => {
                tracelog::tracelog!(
                    label = verifier,visit_stmt,stmt_type_alias;
                    "{} is a type alias statement",
                    stmt_string
                );

                let alias = &stmt_type_alias.alias;
                let value = self
                    .resolve_identifier(
                        &stmt_type_alias.value.0.to_string(),
                        &stmt_type_alias.value.1.clone(),
                    )
                    .unwrap_or(stmt_type_alias.value.0.clone().into());

                self.type_env.set(&alias.0.name, value);

                Ok(())
            }
            ast::Stmt::Fn(stmt_fn) => {
                tracelog::tracelog!(
                    label = verifier,visit_stmt,stmt_function;
                    "{} is an function statement",
                    stmt_string
                );

                let (fn_expr, fn_expr_span) = &mut stmt_fn.fn_expr;

                tracelog::tracelog!(
                    label = verifier,visit_stmt,stmt_function;
                    "Checking function statement's function expression {}",
                    log_expr(fn_expr)
                );

                match self.visit_expr(fn_expr, fn_expr_span) {
                    Ok(fn_expr_type) => {
                        self.type_env.set(&stmt_fn.name.0.name, fn_expr_type);

                        Ok(())
                    }
                    Err(fn_expr_errs) => {
                        tracelog::tracelog!(
                            label = verifier,visit_stmt,stmt_function,error;
                            "Function expression {} had {} errors",
                            log_expr(fn_expr),
                            fn_expr_errs.len()
                        );

                        Err(fn_expr_errs)
                    }
                }
            }
            ast::Stmt::AssertType(stmt_assert_type) => {
                let mut errs: Vec<EgonErrorS> = vec![];

                let (value_expr, value_span) = &mut stmt_assert_type.value;
                if let Err(x) = self.visit_expr(value_expr, value_span) {
                    errs.extend(x);
                }

                let (expected_type_expr, expected_type_span) = &mut stmt_assert_type.expected_type;
                if let Err(x) = self.visit_expr(expected_type_expr, expected_type_span) {
                    errs.extend(x);
                }

                let rule_errs = self.run_stmt_rules(stmt, span);
                errs.extend(rule_errs);

                if !errs.is_empty() {
                    return Err(errs);
                }

                Ok(())
            }
            ast::Stmt::Return(stmt_return) => {
                let mut errs: Vec<EgonErrorS> = vec![];

                let (value_expr, value_span) = &mut stmt_return.value;

                if let Err(x) = self.visit_expr(value_expr, value_span) {
                    errs.extend(x);
                }

                if !errs.is_empty() {
                    return Err(errs);
                }

                //
                Ok(())
            }
            ast::Stmt::Error => {
                tracelog::tracelog!(
                    label = verifier,visit_stmt,error_stmt;
                    "{} is an error statement",
                    stmt_string
                );

                Ok(())
            }
        };

        tracelog::tracelog!(
            label = verifier,visit_stmt;
            "Running rules for statement {}",
            stmt_string
        );

        let rule_errs = self.run_stmt_rules(stmt, span);
        errs.extend(rule_errs);

        tracelog::tracelog!(
            label = verifier,visit_stmt;
            "There were a total of {} rule errors with statement {}",
            errs.len(),
            stmt_string
        );

        tracelog::tracelog!(
            label = verifier,visit_stmt,exit_stmt;
            "{}",
            stmt_string
        );

        if !errs.is_empty() {
            return Err(errs);
        }

        result
    }

    fn run_stmt_rules(&self, stmt: &ast::Stmt, span: &Span) -> Vec<EgonErrorS> {
        tracelog::tracelog!(
            label = verifier,run_stmt_rules;
            "Run rules against statement {}",
            log_stmt(stmt)
        );

        let mut errs: Vec<EgonErrorS> = vec![];

        for rule in &self.rules {
            let rule_string = rule.to_string();

            let rule_errs = rule
                .visit_stmt(
                    stmt,
                    span,
                    &|id: &str, span: &Span| self.resolve_identifier(id, span),
                    &|expr: &ast::Expr, span: &Span| self.resolve_expr_type(expr, span),
                )
                .err()
                .unwrap_or_default();

            if !rule_errs.is_empty() {
                tracelog::tracelog!(
                    label = verifier,run_stmt_rules;
                    "{} generated {} errors against statement {}",
                    rule_string.italic(),
                    rule_errs.len(),
                    log_stmt(stmt)
                );
            }

            errs.extend(rule_errs);
        }

        errs
    }

    fn run_expr_rules(&self, expr: &ast::Expr, span: &Span) -> Vec<EgonErrorS> {
        let mut errs: Vec<EgonErrorS> = vec![];

        tracelog::tracelog!(
            label = verifier,run_expr_rules;
            "Run rules against expression {}",
            log_expr(expr)
        );

        for rule in &self.rules {
            let rule_string = rule.to_string();

            let rule_errs = rule
                .visit_expr(
                    expr,
                    span,
                    &|id: &str, span: &Span| self.resolve_identifier(id, span),
                    &|expr: &ast::Expr, span: &Span| self.resolve_expr_type(expr, span),
                )
                .err()
                .unwrap_or_default();

            if !rule_errs.is_empty() {
                tracelog::tracelog!(
                    label = verifier,run_expr_rules;
                    "{} generated {} errors against expr {}",
                    rule_string.italic(),
                    rule_errs.len(),
                    log_expr(expr)
                );
            }

            errs.extend(rule_errs);
        }

        errs
    }

    fn visit_expr(
        &mut self,
        expr: &mut ast::Expr,
        span: &Span,
    ) -> EgonResultMultiSpannedErr<TypeEnvValue> {
        let expr_string = log_expr(expr);
        let expr_clone = expr.clone();

        tracelog::tracelog!(label = verifier,visit_expr; "{}", expr_string);

        match expr {
            ast::Expr::Block(block_expr) => {
                tracelog::tracelog!(label = verifier,visit_expr,expr_block; "{} is a block expression", expr_string);

                let mut errs: Vec<EgonErrorS> = vec![];

                self.start_new_type_env();

                let mut return_stmt_type: Option<Type> = None;

                let rule_errs = self.run_expr_rules(&expr_clone, span);
                errs.extend(rule_errs);

                for (stmt, stmt_span) in &mut block_expr.stmts {
                    if let ast::Stmt::Return(stmt_return) = stmt {
                        tracelog::tracelog!(label = verifier,visit_expr,expr_block,return_stmt; "{} has a return statement", expr_string);

                        stmt_return.set_used_in_block();

                        match self.resolve_expr_type(&stmt_return.value.0, &stmt_return.value.1) {
                            Ok(v) => {
                                tracelog::tracelog!(
                                    label = verifier,visit_expr,expr_block,return_stmt;
                                    "caching return value: {}",
                                    log_type(&v.of_type)
                                );

                                return_stmt_type = Some(v.of_type);
                            }
                            Err(stmt_return_errs) => {
                                errs.extend(stmt_return_errs);
                            }
                        }
                    }

                    let stmt_errs = self.visit_stmt(stmt, stmt_span).err().unwrap_or_default();

                    errs.extend(stmt_errs);
                }

                match &mut block_expr.return_expr {
                    Some((return_expr, return_span)) => {
                        tracelog::tracelog!(
                            label = verifier,visit_expr,expr_block;
                            "{} has a return expression {}",
                            expr_string,
                            log_expr(return_expr)
                        );

                        match self.visit_expr(return_expr, return_span) {
                            Ok(return_type) => {
                                let rule_errs = self.run_expr_rules(return_expr, return_span);
                                errs.extend(rule_errs);

                                self.end_current_type_env();

                                if !errs.is_empty() {
                                    return Err(errs);
                                }

                                tracelog::tracelog!(
                                    label = verifier,visit_expr,expr_block;
                                    "{} resolved to type of {}",
                                    expr_string,
                                    log_type(&return_type.of_type)
                                );

                                // Record the block's returning expression
                                // This is important for identifier expressions
                                //
                                // ```egon
                                // let a = {
                                //   let b = 123;
                                //
                                //   b
                                // };
                                // ```
                                block_expr.typeref = Some(return_type.of_type.clone());

                                Ok(return_type)
                            }
                            Err(return_errs) => {
                                errs.extend(return_errs);

                                self.end_current_type_env();

                                Err(errs)
                            }
                        }
                    }
                    None => {
                        self.end_current_type_env();

                        if let Some(stmt_return_type) = return_stmt_type {
                            block_expr.typeref = Some(stmt_return_type);
                        }

                        if !errs.is_empty() {
                            return Err(errs);
                        }

                        if let Some(block_return_type) = &block_expr.typeref {
                            return Ok(block_return_type.clone().into());
                        }

                        Ok(Type::unit().into())
                    }
                }
            }
            ast::Expr::Assign(assign_expr) => {
                tracelog::tracelog!(
                    label = visit_expr,expr_assign;
                    "{} is an assign expression",
                    expr_string
                );

                let mut errs: Vec<EgonErrorS> = vec![];

                let ident_errs = self
                    .resolve_identifier(&assign_expr.identifier.0.name, &assign_expr.identifier.1)
                    .err()
                    .unwrap_or_default();

                errs.extend(ident_errs);

                let (value_expr, value_span) = &mut assign_expr.value;
                let value_type = self.visit_expr(value_expr, value_span);

                match value_type {
                    Ok(value_type) => {
                        let rule_errs = self.run_expr_rules(value_expr, value_span);
                        errs.extend(rule_errs);

                        let rule_errs = self.run_expr_rules(expr, span);
                        errs.extend(rule_errs);

                        if !errs.is_empty() {
                            return Err(errs);
                        }

                        Ok(value_type)
                    }
                    Err(value_errs) => {
                        errs.extend(value_errs);

                        let rule_errs = self.run_expr_rules(expr, span);
                        errs.extend(rule_errs);

                        Err(errs)
                    }
                }
            }
            ast::Expr::Infix(infix_expr) => {
                tracelog::tracelog!(
                    label = visit_expr,expr_infix;
                    "{} is an infix expression",
                    expr_string
                );

                let mut errs: Vec<EgonErrorS> = vec![];

                let (lt_expr, lt_span) = &mut infix_expr.lt;

                let lt_errs = self.visit_expr(lt_expr, lt_span).err().unwrap_or_default();

                errs.extend(lt_errs);

                let (rt_expr, rt_span) = &mut infix_expr.rt;

                let rt_errs = self.visit_expr(rt_expr, rt_span).err().unwrap_or_default();

                errs.extend(rt_errs);

                let rule_errs = self.run_expr_rules(&expr_clone, span);
                errs.extend(rule_errs);

                if !errs.is_empty() {
                    return Err(errs);
                }

                Ok(match infix_expr.op {
                    ast::OpInfix::Add => Type::number().into(),
                    ast::OpInfix::Subtract => Type::number().into(),
                    ast::OpInfix::Multiply => Type::number().into(),
                    ast::OpInfix::Divide => Type::number().into(),
                    ast::OpInfix::Modulus => Type::number().into(),
                    ast::OpInfix::Less => Type::bool().into(),
                    ast::OpInfix::LessEqual => Type::bool().into(),
                    ast::OpInfix::Greater => Type::bool().into(),
                    ast::OpInfix::GreaterEqual => Type::bool().into(),
                    ast::OpInfix::Equal => Type::bool().into(),
                    ast::OpInfix::NotEqual => Type::bool().into(),
                    ast::OpInfix::LogicAnd => Type::bool().into(),
                    ast::OpInfix::LogicOr => Type::bool().into(),
                })
            }
            ast::Expr::Prefix(prefix_expr) => {
                tracelog::tracelog!(
                    label = visit_expr,expr_infix;
                    "{} is an prefix expression",
                    expr_string
                );

                let (rt_expr, rt_span) = &mut prefix_expr.rt;

                match self.visit_expr(rt_expr, rt_span) {
                    Ok(_) => {
                        let mut errs: Vec<EgonErrorS> = vec![];

                        let rule_errs = self.run_expr_rules(&expr_clone, span);
                        errs.extend(rule_errs);

                        if !errs.is_empty() {
                            return Err(errs);
                        }

                        Ok(self.resolve_expr_type(expr, span).unwrap())
                    }
                    Err(rt_errs) => Err(rt_errs),
                }
            }
            ast::Expr::Fn(fn_expr) => {
                tracelog::tracelog!(
                    label = verifier,visit_expr,expr_function;
                    "{} is a function expression",
                    expr_string
                );

                self.start_new_type_env();

                let mut param_types: Vec<Type> = vec![];

                for ((ident, type_ref), _) in &fn_expr.params {
                    let name = &ident.name;

                    param_types.push(type_ref.clone());

                    self.type_env.set(
                        name,
                        TypeEnvValue {
                            of_type: type_ref.clone(),
                            is_const: true,
                        },
                    );
                }

                let (body_expr, body_span) = &mut fn_expr.body;

                match self.visit_expr(body_expr, body_span) {
                    Ok(body_type) => {
                        let mut errs: Vec<EgonErrorS> = vec![];

                        let rule_errs = self.run_expr_rules(expr, span);
                        errs.extend(rule_errs);

                        self.end_current_type_env();

                        if !errs.is_empty() {
                            tracelog::tracelog!(
                                label = visit_expr,expr_function,body;
                                "Expression function {} had {} errors",
                                expr_string,
                                errs.len()
                            );

                            return Err(errs);
                        }

                        Ok(Type::function(Type::tuple(param_types), body_type.of_type).into())
                    }
                    Err(body_errs) => {
                        let mut errs: Vec<EgonErrorS> = vec![];

                        errs.extend(body_errs);

                        let rule_errs = self.run_expr_rules(expr, span);
                        errs.extend(rule_errs);

                        self.end_current_type_env();

                        tracelog::tracelog!(
                            label = verifier,visit_expr,expr_function,body;
                            "Expression function {} had {} errors",
                            expr_string,
                            errs.len()
                        );

                        Err(errs)
                    }
                }
            }
            ast::Expr::If(if_expr) => {
                tracelog::tracelog!(
                    label = visit_expr,expr_if;
                    "{} is an if expression",
                    expr_string
                );

                let (cond_expr, cond_span) = &mut if_expr.cond;

                match self.visit_expr(cond_expr, cond_span) {
                    Ok(_) => {
                        let (then_expr, then_span) = &mut if_expr.then;

                        match self.visit_expr(then_expr, then_span) {
                            Ok(then_type) => {
                                let mut errs: Vec<EgonErrorS> = vec![];

                                let rule_errs = self.run_expr_rules(&expr_clone, span);
                                errs.extend(rule_errs);

                                match &mut if_expr.else_ {
                                    Some((else_expr, else_span)) => {
                                        match self.visit_expr(else_expr, else_span) {
                                            Ok(_) => {
                                                if !errs.is_empty() {
                                                    return Err(errs);
                                                }

                                                Ok(then_type.clone())
                                            }
                                            Err(else_errs) => {
                                                errs.extend(else_errs);

                                                Err(errs)
                                            }
                                        }
                                    }
                                    None => {
                                        if !errs.is_empty() {
                                            return Err(errs);
                                        }

                                        Ok(then_type.clone())
                                    }
                                }
                            }
                            Err(then_errs) => Err(then_errs),
                        }
                    }
                    Err(cond_errs) => Err(cond_errs),
                }
            }
            ast::Expr::Unit => Ok(Type::unit().into()),
            ast::Expr::Literal(literal_expr) => Ok(match literal_expr {
                ast::ExprLiteral::Bool(_) => Type::bool().into(),
                ast::ExprLiteral::Number(_) => Type::number().into(),
                ast::ExprLiteral::String(_) => Type::string().into(),
            }),
            ast::Expr::Identifier(_ident_expr) => self.resolve_expr_type(&expr_clone, span),
            ast::Expr::List(list_expr) => {
                tracelog::tracelog!(
                    label = visit_expr,expr_infix;
                    "{} is an list expression",
                    expr_string
                );

                if list_expr.items.is_empty() {
                    return Ok(Type::list(Type::unknown()).into());
                }

                let mut errs: Vec<EgonErrorS> = vec![];
                let mut first_item_type = Type::unknown().into();

                for (i, (item_expr, item_span)) in list_expr.items.iter_mut().enumerate() {
                    match self.visit_expr(item_expr, item_span) {
                        Err(item_errs) => {
                            errs.extend(item_errs);
                        }
                        Ok(item_type) => {
                            if i == 0 {
                                first_item_type = item_type;
                            }
                        }
                    }
                }

                let rule_errs = self.run_expr_rules(&expr_clone, span);
                errs.extend(rule_errs);

                // errs.dedup();

                if !errs.is_empty() {
                    return Err(errs);
                }

                let t = Type::list(first_item_type.of_type).into();

                Ok(t)
            }
            ast::Expr::Tuple(tuple_expr) => {
                let mut item_types: Vec<Type> = vec![];
                let mut errs: Vec<EgonErrorS> = vec![];

                for (item_expr, item_span) in &mut tuple_expr.items {
                    match self.visit_expr(item_expr, item_span) {
                        Ok(item_type) => {
                            item_types.push(item_type.of_type.clone());
                        }
                        Err(item_errs) => {
                            item_types.push(Type::unknown());
                            errs.extend(item_errs);
                        }
                    };
                }

                let rule_errs = self.run_expr_rules(&expr_clone, span);
                errs.extend(rule_errs);

                if !errs.is_empty() {
                    return Err(errs);
                }

                Ok(Type::tuple(item_types.clone()).into())
            }
            ast::Expr::Range(_) => {
                let mut errs: Vec<EgonErrorS> = vec![];

                let rule_errs = self.run_expr_rules(&expr_clone, span);
                errs.extend(rule_errs);

                if !errs.is_empty() {
                    return Err(errs);
                }

                Ok(Type::range().into())
            }
            ast::Expr::Type(type_expr) => {
                tracelog::tracelog!(
                    label = verifier,visit_expr,expr_type;
                    "{} is an type expression",
                    expr_string
                );

                let mut errs: Vec<EgonErrorS> = vec![];

                let typeref = &type_expr.0;
                let typeref_ident = &typeref.to_string();

                let type_type = self.resolve_identifier(typeref_ident, span);

                match type_type {
                    Ok(t) => {
                        let rule_errs = self.run_expr_rules(&expr_clone, span);
                        errs.extend(rule_errs);

                        if !errs.is_empty() {
                            return Err(errs);
                        }

                        Ok(t)
                    }
                    Err(e) => {
                        if typeref.is_builtin() {
                            Ok(typeref.clone().into())
                        } else {
                            Err(e)
                        }
                    }
                }
            }
            ast::Expr::Call(expr_call) => {
                let mut errs: Vec<EgonErrorS> = vec![];

                let (callee_expr, callee_span) = &mut expr_call.callee;
                let callee_type = self.visit_expr(callee_expr, callee_span);

                if let Err(callee_errs) = &callee_type {
                    errs.extend(callee_errs.clone());
                };

                for (arg_expr, arg_span) in &mut expr_call.args {
                    let arg_errs = self
                        .visit_expr(arg_expr, arg_span)
                        .err()
                        .unwrap_or_default();

                    errs.extend(arg_errs);
                }

                let rule_errs = self.run_expr_rules(&expr_clone, span);
                errs.extend(rule_errs);

                if !errs.is_empty() {
                    return Err(errs);
                }

                Ok(callee_type.unwrap())
            }
        }
    }

    /// Start new type environment scope
    fn start_new_type_env(&mut self) {
        tracelog::tracelog!(
            label = verifier,scope_start;
            "Starting new scope"
        );

        self.type_env.start_scope();
    }

    /// End tne current type environment scope
    fn end_current_type_env(&mut self) {
        let level = self.current_type_env_level();

        tracelog::tracelog!(
            label = verifier,scope_end;
            "Ending current scope (current level: {level}, new level: {})",
            level - 1
        );

        let _ = self.type_env.end_scope();
    }

    fn current_type_env_level(&self) -> usize {
        self.type_env.get_scope_depth()
    }
}

#[cfg(test)]
mod verifier_tests {
    use crate::prelude::*;
    use ast::{ExprBlock, Module, StmtExpr};
    use egonlang_core::prelude::*;
    use egonlang_errors::{EgonSyntaxError, EgonTypeError};
    use egonlang_types::Type;
    use pretty_assertions::assert_eq;

    use super::Verifier;

    macro_rules! verifier_test {
        ($test_name:ident, $input:expr, $expected:expr) => {
            #[test]
            fn $test_name() {
                let result = parse($input, 0).and_then(|mut module| verify_module(&mut module));

                assert_eq!($expected, result);
            }
        };

        ($test_name:ident, $input:expr, $expected:expr, $ignore_reason:expr) => {
            #[test]
            #[ignore = $ignore_reason]
            fn $test_name() {
                let result = parse($input, 0).and_then(|module| Verifier::new().verify(&module));

                assert_eq!($expected, result);
            }
        };
    }

    verifier_test!(works_with_empty_module, "", Ok(()));

    verifier_test!(validate_number_expr_stmt, "123;", Ok(()));

    verifier_test!(
        validate_const_declaration_errors_without_value,
        "const a;",
        Err(vec![
            (EgonTypeError::UnknownType.into(), 0..8),
            (
                EgonSyntaxError::UninitializedConst {
                    name: "a".to_string()
                }
                .into(),
                0..8
            )
        ])
    );

    verifier_test!(
        validate_const_declaration_requires_value_with_value,
        "const a = 123;",
        Ok(())
    );

    verifier_test!(
        validate_let_declaration_requires_type_or_value,
        "let a;",
        Err(vec![(EgonTypeError::UnknownType.into(), 0..6)])
    );

    verifier_test!(
        validate_let_declaration_requires_type_or_value_with_value,
        "let a = 123;",
        Ok(())
    );

    verifier_test!(
        validate_let_declaration_requires_type_or_value_with_type,
        "let a: number;",
        Ok(())
    );

    verifier_test!(
        validate_let_declarations_with_value_and_type_must_match,
        "let a: number = \"foo\";",
        Err(vec![(
            EgonTypeError::MismatchType {
                expected: "number".to_string(),
                actual: "string".to_string()
            }
            .into(),
            16..21
        )])
    );

    verifier_test!(
        validate_let_decl_typed_as_number_with_range_value_type,
        "let a: number = 0..10;",
        Err(vec![(
            EgonTypeError::MismatchType {
                expected: "number".to_string(),
                actual: "range".to_string()
            }
            .into(),
            16..21
        )])
    );

    verifier_test!(
        validate_let_decl_typed_as_number_with_list_value_type,
        "let a: number = [1, 2, 3];",
        Err(vec![(
            EgonTypeError::MismatchType {
                expected: "number".to_string(),
                actual: "list<number>".to_string()
            }
            .into(),
            16..25
        )])
    );

    verifier_test!(
        validate_let_decl_typed_as_list_number_with_list_value_type,
        "let a: list<number> = [1, 2, 3];",
        Ok(())
    );

    verifier_test!(
        validate_let_decl_typed_as_tuple_with_tuple_value_type,
        "let a: tuple<number, number, number> = (1, 2, 3,);",
        Ok(())
    );

    verifier_test!(
        validate_let_decl_with_value_as_block_with_returning_expr,
        "let a: number = { 123 };",
        Ok(())
    );

    verifier_test!(
        validate_let_decl_with_value_as_block_with_returning_expr_mismatch_types,
        "let a: number = { \"foo\" };",
        Err(vec![(
            EgonTypeError::MismatchType {
                expected: "number".to_string(),
                actual: "string".to_string()
            }
            .into(),
            16..25
        )])
    );

    verifier_test!(
        validate_let_decl_with_value_as_block_without_returning_expr,
        "let a: () = { 123; };",
        Ok(())
    );

    verifier_test!(validate_list_items_with_same_type, "[1, 2, 3];", Ok(()));

    verifier_test!(
        validate_list_items_with_mixed_types,
        "[
            1,
            2,
            \"foo\",
            { () },
            0..10,
            false,
            if (true) { \"red\" } else { \"blue\" }
        ];",
        Err(vec![
            (
                EgonTypeError::MismatchType {
                    expected: "number".to_string(),
                    actual: "string".to_string()
                }
                .into(),
                44..49
            ),
            (
                EgonTypeError::MismatchType {
                    expected: "number".to_string(),
                    actual: "()".to_string()
                }
                .into(),
                63..69
            ),
            (
                EgonTypeError::MismatchType {
                    expected: "number".to_string(),
                    actual: "range".to_string()
                }
                .into(),
                83..88
            ),
            (
                EgonTypeError::MismatchType {
                    expected: "number".to_string(),
                    actual: "bool".to_string()
                }
                .into(),
                102..107
            ),
            (
                EgonTypeError::MismatchType {
                    expected: "number".to_string(),
                    actual: "string".to_string()
                }
                .into(),
                121..156
            )
        ])
    );

    verifier_test!(
        validate_assign_mismatch_types_block_returning_string,
        "let a: () = { \"foo\" };",
        Err(vec![(
            EgonTypeError::MismatchType {
                expected: "()".to_string(),
                actual: "string".to_string()
            }
            .into(),
            12..21
        )])
    );

    verifier_test!(
        validate_assign_mismatch_types_block_returning_number,
        "let a: () = { 123 };",
        Err(vec![(
            EgonTypeError::MismatchType {
                expected: "()".to_string(),
                actual: "number".to_string()
            }
            .into(),
            12..19
        )])
    );

    verifier_test!(
        validate_assign_mismatch_types_block_returning_unit,
        "let a: () = { () };",
        Ok(())
    );

    verifier_test!(
        validate_assign_mismatch_types_block_returning_list,
        "let a: () = { [1, 2, 3] };",
        Err(vec![(
            EgonTypeError::MismatchType {
                expected: "()".to_string(),
                actual: "list<number>".to_string()
            }
            .into(),
            12..25
        )])
    );

    verifier_test!(
        validate_assign_mismatch_types_block_returning_tuple,
        "let a: () = { (1, 2, 3,) };",
        Err(vec![(
            EgonTypeError::MismatchType {
                expected: "()".to_string(),
                actual: "tuple<number, number, number>".to_string()
            }
            .into(),
            12..26
        )])
    );

    verifier_test!(
        validate_assign_mismatch_types_block_returning_bool,
        "let a: () = { false };",
        Err(vec![(
            EgonTypeError::MismatchType {
                expected: "()".to_string(),
                actual: "bool".to_string()
            }
            .into(),
            12..21
        )])
    );

    verifier_test!(
        validate_assign_mismatch_types_nested_block_returning_number,
        "let a: () = { { 123 } };",
        Err(vec![(
            EgonTypeError::MismatchType {
                expected: "()".to_string(),
                actual: "number".to_string()
            }
            .into(),
            12..23
        )])
    );

    verifier_test!(
        validate_assign_chain_mismatched_types,
        "let a: () = b = 123;",
        Err(vec![(
            EgonTypeError::Undefined("b".to_string()).into(),
            12..13
        )])
    );

    verifier_test!(
        validate_assign_chain_matching_types,
        "let a: number = b = 123;",
        Err(vec![(
            EgonTypeError::Undefined("b".to_string()).into(),
            16..17
        )])
    );

    verifier_test!(
        validate_if_else_mismatched_types,
        "if (true) { 123; } else { 123 };",
        Err(vec![(
            EgonTypeError::MismatchType {
                expected: "()".to_string(),
                actual: "number".to_string()
            }
            .into(),
            24..31
        )])
    );

    verifier_test!(
        validate_if_cond_is_bool_mismatch_number,
        "if (123) {} else {};",
        Err(vec![(
            EgonTypeError::MismatchType {
                expected: "bool".to_string(),
                actual: "number".to_string()
            }
            .into(),
            3..8
        )])
    );

    verifier_test!(
        validate_if_cond_is_bool_mismatch_number_plus_number,
        "if (123 + 456) {} else {};",
        Err(vec![(
            EgonTypeError::MismatchType {
                expected: "bool".to_string(),
                actual: "number".to_string()
            }
            .into(),
            3..14
        )])
    );

    verifier_test!(
        validate_if_cond_is_bool_match_number_lt_number,
        "if (123 < 456) {} else {};",
        Ok(())
    );

    verifier_test!(
        validate_if_cond_is_bool_match_number_gt_number,
        "if (123 > 456) {} else {};",
        Ok(())
    );

    verifier_test!(
        validate_if_cond_is_bool_match_number_gte_number,
        "if (123 >= 456) {} else {};",
        Ok(())
    );

    verifier_test!(
        validate_if_cond_is_bool_match_number_lte_number,
        "if (123 <= 456) {} else {};",
        Ok(())
    );

    verifier_test!(
        validate_if_cond_is_bool_neq,
        "if (123 != 456) {} else {};",
        Ok(())
    );

    verifier_test!(
        validate_if_cond_is_bool_eq,
        "if (123 == 456) {} else {};",
        Ok(())
    );

    verifier_test!(validate_infix_types_plus_type_match, "1 + 1;", Ok(()));

    verifier_test!(
        validate_infix_types_plus_type_mismatch_string,
        "1 + \"foo\";",
        Err(vec![(
            EgonTypeError::MismatchType {
                expected: "number".to_string(),
                actual: "string".to_string()
            }
            .into(),
            4..9
        )])
    );

    verifier_test!(
        validate_infix_types_plus_type_mismatch_string_flipped,
        "\"foo\" + 1;",
        Err(vec![(
            EgonTypeError::MismatchType {
                expected: "number".to_string(),
                actual: "string".to_string()
            }
            .into(),
            0..5
        )])
    );

    verifier_test!(
        testtt,
        "if (true and 1) { (); } else { (); };",
        Err(vec![(
            EgonTypeError::MismatchType {
                expected: "bool".to_string(),
                actual: "number".to_string()
            }
            .into(),
            13..14
        )])
    );

    verifier_test!(
        validate_assign_empty_list_to_typed_list,
        "let a: list<number> = [];",
        Ok(())
    );

    verifier_test!(
        validate_assign_empty_list_to_untyped_list,
        "let a = [];",
        Err(vec![(EgonTypeError::UknownListType.into(), 8..10)])
    );

    verifier_test!(
        validate_let_decl_empty_list_to_unknown_list,
        "let a: list<unknown> = [];",
        Err(vec![(EgonTypeError::UknownListType.into(), 0..26)])
    );

    verifier_test!(
        validate_const_decl_empty_list_to_unknown_list,
        "const a: list<unknown> = [];",
        Err(vec![(EgonTypeError::UknownListType.into(), 0..28)])
    );

    verifier_test!(
        validate_assign_mixed_type_list,
        "a = [1, \"a\"];",
        Err(vec![
            (EgonTypeError::Undefined("a".to_string()).into(), 0..1),
            (
                EgonTypeError::MismatchType {
                    expected: "number".to_string(),
                    actual: "string".to_string()
                }
                .into(),
                8..11
            )
        ])
    );

    verifier_test!(
        validate_let_decl_unknown_type,
        "let a: unknown;",
        Err(vec![(EgonTypeError::UnknownType.into(), 7..14)])
    );

    verifier_test!(
        validate_let_decl_untyped_sets_env_type_reassign_type_mismatch,
        "
        let a = 123;
        a = \"foo\";
        ",
        Err(vec![(
            EgonTypeError::MismatchType {
                expected: Type::number().to_string(),
                actual: Type::string().to_string()
            }
            .into(),
            34..39
        )])
    );

    verifier_test!(
        validate_let_decl_untyped_assign_from_identifier_type_mismatch,
        "
        let a = 123;
        let b: string = a;
        ",
        Err(vec![(
            EgonTypeError::MismatchType {
                expected: Type::string().to_string(),
                actual: Type::number().to_string()
            }
            .into(),
            46..47
        )])
    );

    verifier_test!(
        validate_const_decl_untyped_sets_env_type_reassign_type_mismatch,
        "
        const a = 123;
        a = \"foo\";
        ",
        Err(vec![(
            EgonSyntaxError::ReassigningConst {
                name: "a".to_string()
            }
            .into(),
            32..41
        )])
    );

    verifier_test!(
        list_expression_with_mismatch_type_identifier_value,
        "
        let a = \"foo\";
        let b = [1, 2, a];
        ",
        Err(vec![(
            EgonTypeError::MismatchType {
                expected: Type::number().to_string(),
                actual: Type::string().to_string()
            }
            .into(),
            47..48
        )])
    );

    verifier_test!(
        list_expression_with_typed_identifier_first_item_and_mismatch_type_item,
        "
        let a = \"foo\";
        let b = [a, \"bar\", 3];
        ",
        Err(vec![(
            EgonTypeError::MismatchType {
                expected: Type::string().to_string(),
                actual: Type::number().to_string()
            }
            .into(),
            51..52
        )])
    );

    verifier_test!(
        validate_fn_expr_sum,
        "(a: number, b: number): number => { a + b };",
        Ok(())
    );

    verifier_test!(
        validate_fn_expr_type_mismatch_in_body_identifier,
        "(a: string): number => { a };",
        Err(vec![(
            EgonTypeError::MismatchType {
                expected: "number".to_string(),
                actual: "string".to_string()
            }
            .into(),
            23..28
        )])
    );

    verifier_test!(
        validate_fn_expr_type_mismatch_in_body_infix_bang,
        "(a: string): bool => { !a };",
        Err(vec![(
            EgonTypeError::MismatchType {
                expected: "bool".to_string(),
                actual: "string".to_string()
            }
            .into(),
            24..25
        )])
    );

    verifier_test!(
        validate_fn_expr_type_mismatch_in_body_infix_bang_and_fn_return_type_mismatch,
        "(a: bool): number => { !a };",
        Err(vec![(
            EgonTypeError::MismatchType {
                expected: "number".to_string(),
                actual: "bool".to_string()
            }
            .into(),
            21..27
        )])
    );

    verifier_test!(
        validate_fn_expr_type_mismatch_in_body_infix_bang_and_fn_return_type_mismatch_b,
        "(a: string): number => { !a };",
        Err(vec![
            (
                EgonTypeError::MismatchType {
                    expected: "bool".to_string(),
                    actual: "string".to_string()
                }
                .into(),
                26..27
            ),
            (
                EgonTypeError::MismatchType {
                    expected: "number".to_string(),
                    actual: "bool".to_string()
                }
                .into(),
                23..29
            )
        ])
    );

    verifier_test!(
        validate_let_decl_typed_assigning_bang_prefixed_identifier,
        "
        let a = false;
        let b: number = { !a };
        ",
        Err(vec![(
            EgonTypeError::MismatchType {
                expected: "number".to_string(),
                actual: "bool".to_string()
            }
            .into(),
            48..54
        )])
    );

    verifier_test!(
        validate_fn_expr_sum_type_mismatch,
        "(a: string, b: ()): number => { a + b };",
        Err(vec![
            (
                EgonTypeError::MismatchType {
                    expected: "number".to_string(),
                    actual: "string".to_string()
                }
                .into(),
                32..33
            ),
            (
                EgonTypeError::MismatchType {
                    expected: "number".to_string(),
                    actual: "()".to_string()
                }
                .into(),
                36..37
            )
        ])
    );

    verifier_test!(
        validate_type_alias,
        "
        type NumberList = list<number>;

        let a: NumberList = [1, 2, 3];
        ",
        Ok(())
    );

    verifier_test!(
        validate_type_alias_nested,
        "
        type NumberList = list<number>;
        type Alias = NumberList;

        let a: Alias = [1, 2, 3];
        ",
        Ok(())
    );

    verifier_test!(
        validate_type_alias_pascal_case,
        "type int = number;",
        Err(vec![(
            EgonSyntaxError::InvalidTypeAlias {
                name: "int".to_string()
            }
            .into(),
            0..18
        )])
    );

    verifier_test!(
        validate_type_alias_pascal_case2,
        "type Number_List = list<number>;",
        Err(vec![(
            EgonSyntaxError::InvalidTypeAlias {
                name: "Number_List".to_string()
            }
            .into(),
            0..32
        )])
    );

    // verifier_test!(
    //     validate_fn_expr_mismatch_return_identifier_declared_in_body,
    //     "
    //     (): () => {
    //         type Int = number;
    //         let a: number = 5;
    //         let b: Int = a + 10;
    //         b
    //     };
    //     ",
    //     Err(vec![(
    //         EgonTypeError::MismatchType {
    //             expected: "()".to_string(),
    //             actual: "number".to_string()
    //         }
    //         .into(),
    //         128..129
    //     )])
    // );

    verifier_test!(
        validate_divide_by_zero,
        "0 / 1;",
        Err(vec![(EgonSyntaxError::DivideByZero.into(), 0..1)])
    );

    verifier_test!(
        validate_divide_by_zero_2,
        "1 / 0;",
        Err(vec![(EgonSyntaxError::DivideByZero.into(), 4..5)])
    );

    verifier_test!(
        validate_divide_by_zero_3,
        "0 / 0;",
        Err(vec![
            (EgonSyntaxError::DivideByZero.into(), 0..1),
            (EgonSyntaxError::DivideByZero.into(), 4..5)
        ])
    );

    verifier_test!(
        validate_mismatch_type_when_let_decl_with_block_value,
        r#"
        let a: number = {
            let a: string = "foo";

            a
        };
        "#,
        Err(vec![(
            EgonTypeError::MismatchType {
                expected: Type::number().to_string(),
                actual: Type::string().to_string()
            }
            .into(),
            25..86
        )])
    );

    verifier_test!(
        validate_reassign_const_value_3,
        r#"
        const a: number = 123;

        {
            a = 456;
        };
        "#,
        Err(vec![(
            EgonSyntaxError::ReassigningConst {
                name: "a".to_string()
            }
            .into(),
            55..62
        )])
    );

    verifier_test!(
        validate_type_alias_block_scoped,
        r#"
        type Int = number;

        let a: Int = 5;

        {
            type Integer = Int;
        };
        
        let c: Integer = a + 20;
        "#,
        Err(vec![(
            EgonTypeError::Undefined("Integer".to_string()).into(),
            131..138
        )])
    );

    verifier_test!(
        validate_type_alias_block_scoped_b,
        r#"
        type Int = number;

        {
            type Integer = Int;
        };
        
        let c: Integer = 5 + 20;
        "#,
        Err(vec![(
            EgonTypeError::Undefined("Integer".to_string()).into(),
            106..113
        )])
    );

    verifier_test!(
        validate_type_alias_block_scoped_c,
        r#"
        type Int = number;

        {
            type Integer = Int;
        };
        
        let c: Integer = 5 + 20;
        "#,
        Err(vec![(
            EgonTypeError::Undefined("Integer".to_string()).into(),
            106..113
        )])
    );

    verifier_test!(
        validate_assert_type,
        r#"
        assert_type 123, number;
        assert_type -1, number;
        assert_type true, bool;
        assert_type false, bool;
        assert_type true and false, bool;
        assert_type true or false, bool;
        assert_type !false, bool;
        assert_type (), ();
        assert_type "testing", string;
        assert_type [1, 2, 3], list<number>;
        assert_type [], list<unknown>;
        assert_type (false, 100,), tuple<bool, number>;
        assert_type (a: number, b: number): number => { a + b }, function<tuple<number, number>, number>;
        assert_type 0..100, range;
        assert_type 1 + 2, number;
        assert_type 1 - 2, number;
        assert_type 1 / 2, number;
        assert_type 1 * 2, number;
        assert_type 1 % 2, number;
        assert_type 1 > 2, bool;
        assert_type 1 >= 2, bool;
        assert_type 1 < 2, bool;
        assert_type 1 <= 2, bool;
        assert_type 1 == 2, bool;
        "#,
        Ok(())
    );

    verifier_test!(
        validate_assert_type_with_value_in_block,
        r#"
        assert_type { 123 }, number;
        "#,
        Ok(())
    );

    verifier_test!(
        validate_assign_to_undefined_identifier,
        r#"let b: number = 123;

a = b;"#,
        Err(vec![(
            EgonTypeError::Undefined("a".to_string()).into(),
            22..23
        )])
    );

    #[test]
    fn errors_when_referencing_undefined_identifier() {
        let mut verifier = Verifier::default();

        let mut module = ast::Module::from(vec![(
            ast::StmtExpr {
                expr: (
                    ast::Identifier {
                        name: "a".to_string(),
                    }
                    .into(),
                    1..2,
                ),
            }
            .into(),
            0..3,
        )]);

        let results = verifier.verify(&mut module);

        assert_eq!(
            Err(vec![(
                EgonTypeError::Undefined("a".to_string()).into(),
                1..2
            )]),
            results
        );
    }

    #[test]
    fn errors_when_referencing_multiple_undefined_identifiers() {
        let mut verifier = Verifier::default();

        let mut module = ast::Module::from(vec![
            (
                ast::StmtExpr {
                    expr: (
                        ast::Identifier {
                            name: "a".to_string(),
                        }
                        .into(),
                        1..2,
                    ),
                }
                .into(),
                0..0,
            ),
            (
                ast::StmtExpr {
                    expr: (
                        ast::Identifier {
                            name: "b".to_string(),
                        }
                        .into(),
                        5..6,
                    ),
                }
                .into(),
                0..0,
            ),
        ]);

        let results = verifier.verify(&mut module);

        assert_eq!(
            Err(vec![
                (EgonTypeError::Undefined("a".to_string()).into(), 1..2),
                (EgonTypeError::Undefined("b".to_string()).into(), 5..6)
            ]),
            results
        );
    }

    #[test]
    fn errors_when_declaring_let_using_list_with_type_mismatched_items() {
        let results =
            parse("let a = [10, false];", 0).and_then(|mut module| verify_module(&mut module));

        assert_eq!(
            Err(vec![(
                EgonTypeError::MismatchType {
                    expected: Type::number().to_string(),
                    actual: Type::bool().to_string()
                }
                .into(),
                13..18
            )]),
            results
        );
    }

    #[test]
    fn errors_when_assigning_using_list_with_type_mismatched_items() {
        let results = parse("let a: list<number>; a = [10, false];", 0)
            .and_then(|mut module| verify_module(&mut module));

        assert_eq!(
            Err(vec![(
                EgonTypeError::MismatchType {
                    expected: Type::number().to_string(),
                    actual: Type::bool().to_string()
                }
                .into(),
                30..35
            )]),
            results
        );
    }

    #[test]
    fn errors_when_reassigning_to_undefined_ident_using_list_with_type_mismatched_items() {
        let results =
            parse("a = [10, false];", 0).and_then(|mut module| verify_module(&mut module));

        assert_eq!(
            Err(vec![
                (EgonTypeError::Undefined("a".to_string()).into(), 0..1),
                (
                    EgonTypeError::MismatchType {
                        expected: Type::number().to_string(),
                        actual: Type::bool().to_string()
                    }
                    .into(),
                    9..14
                ),
            ]),
            results
        );
    }

    #[test]
    fn no_errors() {
        let results = parse("(a: number): number => { a };", 0)
            .and_then(|mut module| verify_module(&mut module));

        assert_eq!(Ok(()), results);
    }

    #[test]
    fn validate_applies_types_to_block_exprs() {
        let mut module = parse("{ 123 };", 0).unwrap();
        let _ = verify_module(&mut module);

        assert_eq!(
            Module {
                stmts: vec![(
                    StmtExpr {
                        expr: (
                            ExprBlock {
                                stmts: vec![],
                                return_expr: Some((123f64.into(), 2..5)),
                                typeref: Some(Type::number())
                            }
                            .into(),
                            0..7
                        )
                    }
                    .into(),
                    0..8
                )]
            },
            module
        );
    }

    #[test]
    fn errors_when_calling_number() {
        let results = parse("123456();", 0).and_then(|mut module| verify_module(&mut module));

        assert_eq!(
            Err(vec![(
                EgonTypeError::NotCallable("number".to_string()).into(),
                0..6
            )]),
            results
        );
    }
}
