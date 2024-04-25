use regex::Regex;

use crate::{
    ast::{
        Expr, ExprIdentifier, ExprInfix, ExprList, ExprPrefix, ExprS, ExprTuple, Identifier,
        Module, OpInfix, OpPrefix, Stmt, TypeRef,
    },
    errors::{
        Error, ErrorS, SyntaxError,
        TypeError::{self, UknownListType, UnknownType},
    },
    span::{Span, Spanned},
};

use std::collections::HashMap;

#[derive(Default)]
pub struct Validator {}

#[derive(Debug, PartialEq, Clone)]
pub struct TypeEnvValue {
    pub typeref: TypeRef,
    pub is_const: bool,
}

impl TypeEnvValue {
    fn map_typeref(&self, typeref: TypeRef) -> TypeEnvValue {
        let mut env_var = self.clone();
        env_var.typeref = typeref;

        env_var
    }
}

#[derive(Default)]
pub struct TypeEnvironment<'a> {
    root: Option<&'a TypeEnvironment<'a>>,
    env: HashMap<String, TypeEnvValue>,
}

impl<'a> TypeEnvironment<'a> {
    pub fn new() -> TypeEnvironment<'a> {
        TypeEnvironment {
            root: None,
            env: HashMap::new(),
        }
    }

    fn extend(&self) -> TypeEnvironment {
        TypeEnvironment {
            root: Some(self),
            env: HashMap::new(),
        }
    }

    /// Attempt to resolve an identifier `name` to an [`EnvVar`].
    ///
    /// If the `name` resolve to [`None`], then the `root` [`Env`] will be searched.
    pub fn get(&self, name: &str) -> Option<TypeEnvValue> {
        let result = match self.env.get(name) {
            Some(result) => Some(result.clone()),
            None => match self.root {
                Some(root) => root.get(name),
                _ => None,
            },
        };

        if result.is_some() {
            let result = result.clone().unwrap();

            if result.typeref.0 == *"type" {
                return Some(result.map_typeref(result.typeref.1.first().unwrap().clone()));
            }
        }

        result
    }

    /// Set an identifier name to an [`TypeRef`] and an is_const [`bool`].
    ///
    /// If the [`TypeRef`] is a `type<T>`, the boxed type will be unwrapped when stored.
    /// This helps resolve type aliases.
    ///
    /// If the env did not have this name present, [`None`] is returned.
    pub fn set(&mut self, name: &str, value: TypeEnvValue) -> Option<TypeEnvValue> {
        // Is this a type alias?
        if value.typeref.0 == *"type" {
            let new_typeref = value.typeref.1.first().unwrap().clone();

            return match self.get(&new_typeref.to_string()) {
                Some(r) => self.env.insert(name.to_string(), r),
                None => self
                    .env
                    .insert(name.to_string(), value.map_typeref(new_typeref)),
            };
        }

        self.env.insert(name.to_string(), value)
    }

    /// Resolve an expression's type recursively.
    /// This will resolve value identifiers types as well.
    ///
    /// Example
    ///
    /// let a = 123;
    ///
    /// let b = false;
    ///
    /// (a, b,); // This expression's type resolves to (number, bool,)
    pub fn resolve_expr_type(&self, expr: &ExprS) -> Result<TypeRef, Vec<ErrorS>> {
        let (expr, span) = expr;

        let resolved_type = match expr {
            Expr::Identifier(ident_expr) => self.resolve_identifier_type(ident_expr, span),
            Expr::List(list_expr) => self.resolve_list_type(list_expr),
            Expr::Tuple(tuple_expr) => self.resolve_tuple_type(tuple_expr),
            _ => Ok(expr.clone().get_type_expr()),
        };

        println!("Resolving type for expr: {expr:?} to {resolved_type:?}");

        resolved_type
    }

    fn resolve_identifier_type(
        &self,
        ident_expr: &ExprIdentifier,
        span: &Span,
    ) -> Result<TypeRef, Vec<ErrorS>> {
        let name = &ident_expr.identifier.name;

        match self.get(name) {
            Some(env_var) => Ok(env_var.typeref),
            None => Err(vec![(
                Error::TypeError(TypeError::Undefined(name.to_string())),
                span.clone(),
            )]),
        }
    }

    fn resolve_list_type(&self, list_expr: &ExprList) -> Result<TypeRef, Vec<ErrorS>> {
        if list_expr.items.is_empty() {
            return Ok(TypeRef::list(TypeRef::unknown()));
        }

        let first_item = list_expr.items.first().unwrap().clone();
        let first_item_type = self.resolve_expr_type(&first_item)?;

        let remaining_items: Vec<ExprS> = list_expr.items.clone().into_iter().skip(1).collect();

        let mut errs = vec![];

        for item in remaining_items {
            let (_, item_span) = &item;
            let item_type = self.resolve_expr_type(&item)?;

            if item_type != first_item_type {
                errs.push((
                    Error::TypeError(TypeError::MismatchType {
                        expected: first_item_type.to_string(),
                        actual: item_type.to_string(),
                    }),
                    item_span.clone(),
                ));
            }
        }

        if !errs.is_empty() {
            return Err(errs);
        }

        Ok(TypeRef::list(first_item_type))
    }

    fn resolve_tuple_type(&self, tuple_expr: &ExprTuple) -> Result<TypeRef, Vec<ErrorS>> {
        if tuple_expr.items.is_empty() {
            return Ok(TypeRef::list(TypeRef::unknown()));
        }

        let item_types = tuple_expr
            .items
            .clone()
            .into_iter()
            .map(|x| self.resolve_expr_type(&x).unwrap())
            .collect();

        Ok(TypeRef::tuple(item_types))
    }
}

impl Validator {
    /// Validate a [`Module`]'s AST
    pub fn validate(
        &mut self,
        module: &Module,
        env: &mut TypeEnvironment,
    ) -> Result<(), Vec<ErrorS>> {
        let mut all_errs: Vec<ErrorS> = vec![];

        for stmt in &module.stmts {
            if let Err(errs) = self.visit_stmt(stmt, env) {
                all_errs.extend(errs);
            }
        }

        if !all_errs.is_empty() {
            return Err(all_errs);
        }

        Ok(())
    }

    #[allow(dead_code)]
    fn visit_ident(&mut self, _ident: &Spanned<Identifier>) -> Result<(), Vec<ErrorS>> {
        Ok(())
    }

    fn visit_stmt(
        &mut self,
        stmt: &Spanned<Stmt>,
        env: &mut TypeEnvironment,
    ) -> Result<(), Vec<ErrorS>> {
        let (stmt, span) = stmt;

        match stmt {
            Stmt::Expr(stmt_expr) => self.visit_expr(&stmt_expr.expr, env),
            Stmt::Assign(stmt_assign) => {
                let pattern = Regex::new("^[A-Z][A-Za-z0-9]*$").unwrap();

                let mut errs = vec![];

                let name = stmt_assign.identifier.name.clone();

                // const declarations
                if stmt_assign.is_const {
                    if stmt_assign.value.is_none() {
                        errs.push((
                            Error::SyntaxError(SyntaxError::UninitializedConst {
                                name: name.clone(),
                            }),
                            span.clone(),
                        ));

                        if stmt_assign.type_expr.is_none() {
                            errs.push((Error::TypeError(TypeError::UnknownType), span.clone()));
                        }
                    }
                // let declarations
                } else if stmt_assign.type_expr.is_none() && stmt_assign.value.is_none() {
                    errs.push((Error::TypeError(TypeError::UnknownType), span.clone()));
                }

                if stmt_assign.type_expr.is_some() {
                    if stmt_assign.value.is_some() {
                        let (type_expr, _) = stmt_assign.type_expr.clone().unwrap();
                        let mut type_identifier = type_expr.get_type_expr();
                        let value = stmt_assign.value.clone().unwrap();

                        let value_expr = value.0;
                        let value_type = value_expr.clone().get_type_expr();

                        if value_type.0 == *"type" && !pattern.is_match(&name) {
                            errs.push((
                                Error::SyntaxError(SyntaxError::InvalidTypeAlias {
                                    name: name.to_string(),
                                }),
                                span.clone(),
                            ));
                        }

                        // Resolve any type identifier type aliases
                        if let Some(type_identifier_aliased_to) =
                            env.get(&type_identifier.to_string())
                        {
                            type_identifier = type_identifier_aliased_to.typeref.clone();
                        }

                        if type_identifier != value_type {
                            // This checks for empty lists being assigned to a list<T>
                            // e.g. let a: list<number> = [];
                            if type_identifier.0 == TypeRef::list(TypeRef::unknown()).0
                                && value_type == TypeRef::list(TypeRef::unknown())
                            {
                                env.set(
                                    &name,
                                    TypeEnvValue {
                                        typeref: type_identifier,
                                        is_const: stmt_assign.is_const,
                                    },
                                );
                            // If the mismatched type is an identifier, the identifier's
                            // must be resolved first
                            } else if value_type.0 == *"identifier" {
                                if let Expr::Identifier(identifier) = &value_expr {
                                    let value_identifier = &identifier.identifier.name;

                                    if let Some(value_type) = env.get(value_identifier) {
                                        if type_identifier != value_type.typeref {
                                            errs.push((
                                                Error::TypeError(TypeError::MismatchType {
                                                    expected: type_identifier.to_string(),
                                                    actual: value_type.typeref.to_string(),
                                                }),
                                                value.1.clone(),
                                            ));
                                        }
                                    }
                                }
                            // All other cases are a type mismatch
                            } else {
                                errs.push((
                                    Error::TypeError(TypeError::MismatchType {
                                        expected: type_identifier.to_string(),
                                        actual: value_type.to_string(),
                                    }),
                                    span.clone(),
                                ));
                            };
                        } else {
                            // This checks for empty lists being assigned to a list<unknown>
                            // e.g. let a: list<unknown> = [];
                            if type_identifier == TypeRef::list(TypeRef::unknown())
                                && value_type == TypeRef::list(TypeRef::unknown())
                            {
                                errs.push((Error::TypeError(UknownListType {}), span.clone()));
                            } else {
                                env.set(
                                    &name,
                                    TypeEnvValue {
                                        typeref: type_identifier,
                                        is_const: stmt_assign.is_const,
                                    },
                                );
                            };
                        }
                    // Assignment value was not defined
                    } else {
                        let (type_expr, _) = stmt_assign.type_expr.clone().unwrap();
                        let type_identifier = type_expr.get_type_expr();

                        if type_identifier == TypeRef::unknown() {
                            errs.push((Error::TypeError(UnknownType {}), span.clone()));
                        } else {
                            env.set(
                                &name,
                                TypeEnvValue {
                                    typeref: type_identifier,
                                    is_const: stmt_assign.is_const,
                                },
                            );
                        }
                    }
                // Assignment type expression was not defined
                } else {
                    // This checks for empty lists being assigned to a untyped list<unknown>
                    // e.g. let a = [];
                    if stmt_assign.value.is_some() {
                        let value = stmt_assign.value.clone().unwrap();

                        if let Err(e) = self.visit_expr(&value, env) {
                            errs.extend(e);
                        } else {
                            let value_type = value.0.get_type_expr();

                            // This checks for empty lists being assigned to a list<T>
                            // e.g. let a = [];
                            if value_type == TypeRef::list(TypeRef::unknown()) {
                                errs.push((Error::TypeError(UknownListType {}), span.clone()));
                            } else {
                                env.set(
                                    &name,
                                    TypeEnvValue {
                                        typeref: value_type,
                                        is_const: stmt_assign.is_const,
                                    },
                                );
                            }
                        }
                    }
                }

                if !errs.is_empty() {
                    return Err(errs);
                }

                Ok(())
            }
            Stmt::Fn(_) => Ok(()),
            Stmt::Error => todo!(),
        }
    }

    fn visit_expr(
        &mut self,
        expr: &Spanned<Expr>,
        env: &mut TypeEnvironment,
    ) -> Result<(), Vec<ErrorS>> {
        let (expr, expr_span) = expr;

        match expr {
            Expr::List(list) => {
                if list.items.is_empty() {
                    return Ok(());
                }

                let first_item = list.items.first().unwrap().clone();

                let first_item_type_ident = env.resolve_expr_type(&first_item)?;

                let remaining_items: Vec<ExprS> = list.items.clone().into_iter().skip(1).collect();

                let mut errs = vec![];

                for item in remaining_items {
                    let (_, item_span) = &item;
                    let item_type = env.resolve_expr_type(&item)?;

                    if item_type != first_item_type_ident {
                        errs.push((
                            Error::TypeError(TypeError::MismatchType {
                                expected: first_item_type_ident.to_string(),
                                actual: item_type.to_string(),
                            }),
                            item_span.clone(),
                        ));
                    }
                }

                if !errs.is_empty() {
                    return Err(errs);
                }

                Ok(())
            }
            Expr::If(if_) => {
                let mut errs = vec![];

                if let Err(cond_errs) = self.visit_expr(&if_.cond, env) {
                    errs.extend(cond_errs);
                }

                if let Err(then_errs) = self.visit_expr(&if_.then, env) {
                    errs.extend(then_errs);
                }

                if let Some(else_errs) = &if_.else_ {
                    if let Err(else_errs) = self.visit_expr(else_errs, env) {
                        errs.extend(else_errs);
                    }
                }

                let cond_expr = if_.cond.0.clone().get_type_expr();

                if cond_expr.0 != *"bool" {
                    return Err(vec![(
                        Error::TypeError(TypeError::MismatchType {
                            expected: "bool".to_string(),
                            actual: cond_expr.to_string(),
                        }),
                        if_.cond.1.clone(),
                    )]);
                }

                let then_type_ref = if_.then.0.clone().get_type_expr();

                if if_.else_.is_some() {
                    let (else_expr, else_span) = if_.else_.clone().unwrap();
                    let else_type_ref = else_expr.get_type_expr();

                    if else_type_ref != then_type_ref {
                        return Err(vec![(
                            Error::TypeError(TypeError::MismatchType {
                                expected: then_type_ref.to_string(),
                                actual: else_type_ref.to_string(),
                            }),
                            else_span.clone(),
                        )]);
                    }
                }

                if !errs.is_empty() {
                    return Err(errs);
                }

                Ok(())
            }
            Expr::Infix(infix) => {
                let number_typeref = TypeRef::number();
                let bool_typeref = TypeRef::bool();

                match infix.op {
                    OpInfix::Add => self.validate_infix_types(infix, number_typeref, env),
                    OpInfix::Subtract => self.validate_infix_types(infix, number_typeref, env),
                    OpInfix::Multiply => self.validate_infix_types(infix, number_typeref, env),
                    OpInfix::Divide => self.validate_infix_types(infix, number_typeref, env),
                    OpInfix::Modulus => self.validate_infix_types(infix, number_typeref, env),
                    OpInfix::Less => self.validate_infix_types(infix, number_typeref, env),
                    OpInfix::LessEqual => self.validate_infix_types(infix, number_typeref, env),
                    OpInfix::Greater => self.validate_infix_types(infix, number_typeref, env),
                    OpInfix::GreaterEqual => self.validate_infix_types(infix, number_typeref, env),
                    OpInfix::LogicAnd => self.validate_infix_types(infix, bool_typeref, env),
                    OpInfix::LogicOr => self.validate_infix_types(infix, bool_typeref, env),
                    _ => Ok(()),
                }
            }
            Expr::Prefix(prefix) => {
                let number_typeref = TypeRef::number();
                let bool_typeref = TypeRef::bool();

                match prefix.op {
                    OpPrefix::Negate => self.validate_prefix_types(prefix, number_typeref, env),
                    OpPrefix::Not => self.validate_prefix_types(prefix, bool_typeref, env),
                }
            }
            Expr::Block(block) => {
                let mut errs: Vec<ErrorS> = vec![];

                let mut block_env = env.extend();

                for stmt in &block.stmts {
                    if let Err(stmt_errs) = self.visit_stmt(stmt, &mut block_env) {
                        errs.extend(stmt_errs);
                    }
                }

                if let Some(returning_expr) = &block.return_expr {
                    if let Err(expr_errs) = self.visit_expr(returning_expr, &mut block_env) {
                        errs.extend(expr_errs);
                    }
                }

                if !errs.is_empty() {
                    return Err(errs);
                }

                Ok(())
            }
            Expr::Assign(assign) => {
                let mut errs: Vec<ErrorS> = vec![];

                if let Err(expr_errs) = self.visit_expr(&assign.value, env) {
                    errs.extend(expr_errs);
                }

                let value_type = assign.value.0.clone().get_type_expr();

                if let Some(env_var) = env.get(&assign.identifier.name) {
                    if env_var.is_const {
                        errs.push((
                            Error::SyntaxError(SyntaxError::ReassigningConst {
                                name: assign.identifier.name.clone(),
                            }),
                            expr_span.clone(),
                        ));
                    } else {
                        let env_var_type = env_var.typeref.clone();

                        if env_var_type != value_type {
                            errs.push((
                                Error::TypeError(TypeError::MismatchType {
                                    expected: env_var_type.to_string(),
                                    actual: value_type.to_string(),
                                }),
                                assign.value.1.clone(),
                            ));
                        }
                    }
                }

                if !errs.is_empty() {
                    return Err(errs);
                }

                Ok(())
            }
            Expr::Fn(fn_) => {
                let mut errs: Vec<ErrorS> = vec![];

                let mut fn_env = env.extend();

                for ((ident, type_ref), _) in &fn_.params {
                    let name = &ident.name;

                    fn_env.set(
                        name,
                        TypeEnvValue {
                            typeref: type_ref.clone(),
                            is_const: true,
                        },
                    );
                }

                if let Err(body_errs) = self.visit_expr(&fn_.body, &mut fn_env) {
                    errs.extend(body_errs);
                }

                let fn_return_type = fn_.return_type.0.clone();

                if let Expr::Block(block) = &fn_.body.0 {
                    for stmt in &block.stmts {
                        if let Err(stmt_errs) = self.visit_stmt(stmt, &mut fn_env) {
                            errs.extend(stmt_errs);
                        }
                    }

                    if let Some(block_return) = &block.return_expr {
                        let block_return_type = fn_env.resolve_expr_type(block_return)?;
                        let (_, returning_expr_span) = block_return;

                        if fn_return_type != block_return_type {
                            errs.push((
                                Error::TypeError(TypeError::MismatchType {
                                    expected: fn_return_type.to_string(),
                                    actual: block_return_type.to_string(),
                                }),
                                returning_expr_span.clone(),
                            ));
                        }
                    }
                }

                if !errs.is_empty() {
                    return Err(errs);
                }

                Ok(())
            }
            _ => Ok(()),
        }
    }

    fn validate_infix_types(
        &mut self,
        infix: &ExprInfix,
        expected_type: TypeRef,
        env: &mut TypeEnvironment<'_>,
    ) -> Result<(), Vec<ErrorS>> {
        let mut errs = vec![];

        let lt_errs = self.visit_expr(&infix.lt, env).err().unwrap_or_default();
        errs.extend(lt_errs);

        let rt_errs = self.visit_expr(&infix.rt, env).err().unwrap_or_default();
        errs.extend(rt_errs);

        let lt_type = env.resolve_expr_type(&infix.lt)?;
        let (_, lt_span) = infix.lt.clone();

        let rt_type = env.resolve_expr_type(&infix.rt)?;
        let (_, rt_span) = infix.rt.clone();

        if lt_type != expected_type {
            errs.push((
                Error::TypeError(TypeError::MismatchType {
                    expected: expected_type.to_string(),
                    actual: lt_type.to_string(),
                }),
                lt_span,
            ));
        }
        if rt_type != expected_type {
            errs.push((
                Error::TypeError(TypeError::MismatchType {
                    expected: expected_type.to_string(),
                    actual: rt_type.to_string(),
                }),
                rt_span,
            ));
        }

        if !errs.is_empty() {
            return Err(errs);
        }

        Ok(())
    }

    fn validate_prefix_types(
        &mut self,
        prefix: &ExprPrefix,
        expected_type: TypeRef,
        env: &mut TypeEnvironment<'_>,
    ) -> Result<(), Vec<ErrorS>> {
        let mut errs = vec![];

        let rt_errs = self.visit_expr(&prefix.rt, env).err().unwrap_or_default();
        errs.extend(rt_errs);

        let rt_type = env.resolve_expr_type(&prefix.rt)?;
        let (_, rt_span) = prefix.rt.clone();

        if rt_type != expected_type {
            errs.push((
                Error::TypeError(TypeError::MismatchType {
                    expected: expected_type.to_string(),
                    actual: rt_type.to_string(),
                }),
                rt_span,
            ));
        }

        if !errs.is_empty() {
            return Err(errs);
        }

        Ok(())
    }
}

#[cfg(test)]
mod validator_tests {
    use pretty_assertions::assert_eq;

    use crate::{
        ast::TypeRef,
        errors::{Error, SyntaxError, TypeError},
        parser::parse,
        validator::TypeEnvValue,
    };

    use super::{TypeEnvironment, Validator};

    macro_rules! validator_test {
        ($test_name:ident, $input:expr, $expected:expr) => {
            #[test]
            fn $test_name() {
                let mut env = TypeEnvironment::new();

                let result = parse($input, 0)
                    .and_then(|module| Validator::default().validate(&module, &mut env));

                assert_eq!($expected, result);
            }
        };
    }

    validator_test!(validate_number_expr_stmt, "123;", Ok(()));

    validator_test!(
        validate_const_declaration_requires_value,
        "const a;",
        Err(vec![
            (
                Error::SyntaxError(SyntaxError::UninitializedConst {
                    name: "a".to_string()
                }),
                0..8
            ),
            (Error::TypeError(TypeError::UnknownType {}), 0..8)
        ])
    );

    validator_test!(
        validate_const_declaration_requires_value_with_value,
        "const a = 123;",
        Ok(())
    );

    validator_test!(
        validate_let_declaration_requires_type_or_value,
        "let a;",
        Err(vec![(Error::TypeError(TypeError::UnknownType), 0..6)])
    );

    validator_test!(
        validate_let_declaration_requires_type_or_value_with_value,
        "let a = 123;",
        Ok(())
    );

    validator_test!(
        validate_let_declaration_requires_type_or_value_with_type,
        "let a: number;",
        Ok(())
    );

    validator_test!(
        validate_let_declarations_with_value_and_type_must_match,
        "let a: number = \"foo\";",
        Err(vec![(
            Error::TypeError(TypeError::MismatchType {
                expected: "number".to_string(),
                actual: "string".to_string()
            }),
            0..22
        )])
    );

    validator_test!(
        validate_let_decl_typed_as_number_with_range_value_type,
        "let a: number = 0..10;",
        Err(vec![(
            Error::TypeError(TypeError::MismatchType {
                expected: "number".to_string(),
                actual: "range".to_string()
            }),
            0..22
        )])
    );

    validator_test!(
        validate_let_decl_typed_as_number_with_list_value_type,
        "let a: number = [1, 2, 3];",
        Err(vec![(
            Error::TypeError(TypeError::MismatchType {
                expected: "number".to_string(),
                actual: "list<number>".to_string()
            }),
            0..26
        )])
    );

    validator_test!(
        validate_let_decl_typed_as_list_number_with_list_value_type,
        "let a: list<number> = [1, 2, 3];",
        Ok(())
    );

    validator_test!(
        validate_let_decl_typed_as_tuple_with_tuple_value_type,
        "let a: tuple<number, number, number> = (1, 2, 3,);",
        Ok(())
    );

    validator_test!(
        validate_let_decl_with_value_as_block_with_returning_expr,
        "let a: number = { 123 };",
        Ok(())
    );

    validator_test!(
        validate_let_decl_with_value_as_block_with_returning_expr_mismatch_types,
        "let a: number = { \"foo\" };",
        Err(vec![(
            Error::TypeError(TypeError::MismatchType {
                expected: "number".to_string(),
                actual: "string".to_string()
            }),
            0..26
        )])
    );

    validator_test!(
        validate_let_decl_with_value_as_block_without_returning_expr,
        "let a: () = { 123; };",
        Ok(())
    );

    validator_test!(validate_list_items_with_same_type, "[1, 2, 3];", Ok(()));

    validator_test!(
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
                Error::TypeError(TypeError::MismatchType {
                    expected: "number".to_string(),
                    actual: "string".to_string()
                }),
                44..49
            ),
            (
                Error::TypeError(TypeError::MismatchType {
                    expected: "number".to_string(),
                    actual: "()".to_string()
                }),
                63..69
            ),
            (
                Error::TypeError(TypeError::MismatchType {
                    expected: "number".to_string(),
                    actual: "range".to_string()
                }),
                83..88
            ),
            (
                Error::TypeError(TypeError::MismatchType {
                    expected: "number".to_string(),
                    actual: "bool".to_string()
                }),
                102..107
            ),
            (
                Error::TypeError(TypeError::MismatchType {
                    expected: "number".to_string(),
                    actual: "string".to_string()
                }),
                121..156
            )
        ])
    );

    validator_test!(
        validate_assign_mismatch_types_block_returning_string,
        "let a: () = { \"foo\" };",
        Err(vec![(
            Error::TypeError(TypeError::MismatchType {
                expected: "()".to_string(),
                actual: "string".to_string()
            }),
            0..22
        )])
    );

    validator_test!(
        validate_assign_mismatch_types_block_returning_number,
        "let a: () = { 123 };",
        Err(vec![(
            Error::TypeError(TypeError::MismatchType {
                expected: "()".to_string(),
                actual: "number".to_string()
            }),
            0..20
        )])
    );

    validator_test!(
        validate_assign_mismatch_types_block_returning_unit,
        "let a: () = { () };",
        Ok(())
    );

    validator_test!(
        validate_assign_mismatch_types_block_returning_list,
        "let a: () = { [1, 2, 3] };",
        Err(vec![(
            Error::TypeError(TypeError::MismatchType {
                expected: "()".to_string(),
                actual: "list<number>".to_string()
            }),
            0..26
        )])
    );

    validator_test!(
        validate_assign_mismatch_types_block_returning_tuple,
        "let a: () = { (1, 2, 3,) };",
        Err(vec![(
            Error::TypeError(TypeError::MismatchType {
                expected: "()".to_string(),
                actual: "tuple<number, number, number>".to_string()
            }),
            0..27
        )])
    );

    validator_test!(
        validate_assign_mismatch_types_block_returning_bool,
        "let a: () = { false };",
        Err(vec![(
            Error::TypeError(TypeError::MismatchType {
                expected: "()".to_string(),
                actual: "bool".to_string()
            }),
            0..22
        )])
    );

    validator_test!(
        validate_assign_mismatch_types_nested_block_returning_number,
        "let a: () = { { 123 } };",
        Err(vec![(
            Error::TypeError(TypeError::MismatchType {
                expected: "()".to_string(),
                actual: "number".to_string()
            }),
            0..24
        )])
    );

    validator_test!(
        validate_assign_chain_mismatched_types,
        "let a: () = b = 123;",
        Err(vec![(
            Error::TypeError(TypeError::MismatchType {
                expected: "()".to_string(),
                actual: "number".to_string()
            }),
            0..20
        )])
    );

    validator_test!(
        validate_assign_chain_matching_types,
        "let a: number = b = 123;",
        Ok(())
    );

    validator_test!(
        validate_if_else_mismatched_types,
        "if (true) { 123; } else { 123 };",
        Err(vec![(
            Error::TypeError(TypeError::MismatchType {
                expected: "()".to_string(),
                actual: "number".to_string()
            }),
            24..31
        )])
    );

    validator_test!(
        validate_if_cond_is_bool_mismatch_number,
        "if (123) {} else {};",
        Err(vec![(
            Error::TypeError(TypeError::MismatchType {
                expected: "bool".to_string(),
                actual: "number".to_string()
            }),
            4..7
        )])
    );

    validator_test!(
        validate_if_cond_is_bool_mismatch_number_plus_number,
        "if (123 + 456) {} else {};",
        Err(vec![(
            Error::TypeError(TypeError::MismatchType {
                expected: "bool".to_string(),
                actual: "number".to_string()
            }),
            4..13
        )])
    );

    validator_test!(
        validate_if_cond_is_bool_match_number_lt_number,
        "if (123 < 456) {} else {};",
        Ok(())
    );

    validator_test!(
        validate_if_cond_is_bool_match_number_gt_number,
        "if (123 > 456) {} else {};",
        Ok(())
    );

    validator_test!(
        validate_if_cond_is_bool_match_number_gte_number,
        "if (123 >= 456) {} else {};",
        Ok(())
    );

    validator_test!(
        validate_if_cond_is_bool_match_number_lte_number,
        "if (123 <= 456) {} else {};",
        Ok(())
    );

    validator_test!(
        validate_if_cond_is_bool_neq,
        "if (123 != 456) {} else {};",
        Ok(())
    );

    validator_test!(
        validate_if_cond_is_bool_eq,
        "if (123 == 456) {} else {};",
        Ok(())
    );

    validator_test!(validate_infix_types_plus_type_match, "1 + 1;", Ok(()));

    validator_test!(
        validate_infix_types_plus_type_mismatch_string,
        "1 + \"foo\";",
        Err(vec![(
            Error::TypeError(TypeError::MismatchType {
                expected: "number".to_string(),
                actual: "string".to_string()
            }),
            4..9
        )])
    );

    validator_test!(
        validate_infix_types_plus_type_mismatch_string_flipped,
        "\"foo\" + 1;",
        Err(vec![(
            Error::TypeError(TypeError::MismatchType {
                expected: "number".to_string(),
                actual: "string".to_string()
            }),
            0..5
        )])
    );

    validator_test!(
        testtt,
        "if (true and 1) { (); } else { (); };",
        Err(vec![(
            Error::TypeError(TypeError::MismatchType {
                expected: "bool".to_string(),
                actual: "number".to_string()
            }),
            13..14
        )])
    );

    validator_test!(
        validate_assign_empty_list_to_typed_list,
        "let a: list<number> = [];",
        Ok(())
    );

    validator_test!(
        validate_assign_empty_list_to_untyped_list,
        "let a = [];",
        Err(vec![(
            Error::TypeError(TypeError::UknownListType {}),
            0..11
        )])
    );

    validator_test!(
        validate_let_decl_empty_list_to_unknown_list,
        "let a: list<unknown> = [];",
        Err(vec![(
            Error::TypeError(TypeError::UknownListType {}),
            0..26
        )])
    );

    validator_test!(
        validate_const_decl_empty_list_to_unknown_list,
        "const a: list<unknown> = [];",
        Err(vec![(
            Error::TypeError(TypeError::UknownListType {}),
            0..28
        )])
    );

    validator_test!(
        validate_assign_mixed_type_list,
        "a = [1, \"a\"];",
        Err(vec![(
            Error::TypeError(TypeError::MismatchType {
                expected: "number".to_string(),
                actual: "string".to_string()
            }),
            8..11
        )])
    );

    validator_test!(
        validate_let_decl_unknown_type,
        "let a: unknown;",
        Err(vec![(Error::TypeError(TypeError::UnknownType {}), 0..15)])
    );

    validator_test!(
        validate_let_decl_untyped_sets_env_type_reassign_type_mismatch,
        "
        let a = 123;
        a = \"foo\";
        ",
        Err(vec![(
            Error::TypeError(TypeError::MismatchType {
                expected: TypeRef::number().to_string(),
                actual: TypeRef::string().to_string()
            }),
            34..39
        )])
    );

    validator_test!(
        validate_let_decl_untyped_assign_from_identifier_type_mismatch,
        "
        let a = 123;
        let b: string = a;
        ",
        Err(vec![(
            Error::TypeError(TypeError::MismatchType {
                expected: TypeRef::string().to_string(),
                actual: TypeRef::number().to_string()
            }),
            46..47
        )])
    );

    validator_test!(
        validate_const_decl_untyped_sets_env_type_reassign_type_mismatch,
        "
        const a = 123;
        a = \"foo\";
        ",
        Err(vec![(
            Error::SyntaxError(SyntaxError::ReassigningConst {
                name: "a".to_string()
            }),
            32..41
        )])
    );

    validator_test!(
        list_expression_with_mismatch_type_identifier_value,
        "
        let a = \"foo\";
        let b = [1, 2, a];
        ",
        Err(vec![(
            Error::TypeError(TypeError::MismatchType {
                expected: TypeRef::number().to_string(),
                actual: TypeRef::string().to_string()
            }),
            47..48
        )])
    );

    validator_test!(
        list_expression_with_typed_identifier_first_item_and_mismatch_type_item,
        "
        let a = \"foo\";
        let b = [a, \"bar\", 3];
        ",
        Err(vec![(
            Error::TypeError(TypeError::MismatchType {
                expected: TypeRef::string().to_string(),
                actual: TypeRef::number().to_string()
            }),
            51..52
        )])
    );

    validator_test!(
        validate_fn_expr_sum,
        "(a: number, b: number): number => { a + b };",
        Ok(())
    );

    validator_test!(
        validate_fn_expr_type_mismatch_in_body_identifier,
        "(a: string): number => { a };",
        Err(vec![(
            Error::TypeError(TypeError::MismatchType {
                expected: "number".to_string(),
                actual: "string".to_string()
            }),
            25..26
        )])
    );

    validator_test!(
        validate_fn_expr_type_mismatch_in_body_infix_bang,
        "(a: string): bool => { !a };",
        Err(vec![(
            Error::TypeError(TypeError::MismatchType {
                expected: "bool".to_string(),
                actual: "string".to_string()
            }),
            24..25
        )])
    );

    validator_test!(
        validate_fn_expr_type_mismatch_in_body_infix_bang_and_fn_return_type_mismatch,
        "(a: string): number => { !a };",
        Err(vec![
            (
                Error::TypeError(TypeError::MismatchType {
                    expected: "bool".to_string(),
                    actual: "string".to_string()
                }),
                26..27
            ),
            (
                Error::TypeError(TypeError::MismatchType {
                    expected: "number".to_string(),
                    actual: "bool".to_string()
                }),
                25..27
            )
        ])
    );

    validator_test!(
        validate_let_decl_typed_assigning_bang_prefixed_identifier,
        "
        let a = 123;
        let b: number = { !a };
        ",
        Err(vec![(
            Error::TypeError(TypeError::MismatchType {
                expected: "number".to_string(),
                actual: "bool".to_string()
            }),
            30..53
        )])
    );

    validator_test!(
        validate_fn_expr_sum_type_mismatch,
        "(a: string, b: ()): number => { a + b };",
        Err(vec![
            (
                Error::TypeError(TypeError::MismatchType {
                    expected: "number".to_string(),
                    actual: "string".to_string()
                }),
                32..33
            ),
            (
                Error::TypeError(TypeError::MismatchType {
                    expected: "number".to_string(),
                    actual: "()".to_string()
                }),
                36..37
            )
        ])
    );

    validator_test!(
        validate_type_alias,
        "
        type NumberList = list<number>;

        let a: NumberList = [1, 2, 3];
        ",
        Ok(())
    );

    validator_test!(
        validate_type_alias_nested,
        "
        type NumberList = list<number>;
        type Alias = NumberList;

        let a: Alias = [1, 2, 3];
        ",
        Ok(())
    );

    validator_test!(
        validate_type_alias_pascal_case,
        "type int = number;",
        Err(vec![(
            Error::SyntaxError(SyntaxError::InvalidTypeAlias {
                name: "int".to_string()
            }),
            0..18
        )])
    );

    validator_test!(
        validate_type_alias_pascal_case2,
        "type Number_List = list<number>;",
        Err(vec![(
            Error::SyntaxError(SyntaxError::InvalidTypeAlias {
                name: "Number_List".to_string()
            }),
            0..32
        )])
    );

    validator_test!(
        validate_fn_expr_mismatch_return_identifier_declared_in_body,
        "
        (): () => {
            type Int = number;
            let a: number = 5;
            let b: Int = a + 10;
            b
        };
        ",
        Err(vec![(
            Error::TypeError(TypeError::MismatchType {
                expected: "()".to_string(),
                actual: "number".to_string()
            }),
            128..129
        )])
    );

    validator_test!(
        validate_list_expr_mismatch_item_types_with_identifiers,
        "[1, a];",
        Err(vec![(
            Error::TypeError(TypeError::Undefined("a".to_string())),
            4..5
        )])
    );

    #[test]
    fn env_works() {
        let mut env = TypeEnvironment::new();

        env.set(
            "a",
            TypeEnvValue {
                typeref: TypeRef::number(),
                is_const: false,
            },
        );

        assert_eq!(
            Some(TypeEnvValue {
                typeref: TypeRef::number(),
                is_const: false,
            }),
            env.get("a")
        );

        let mut env2 = TypeEnvironment::extend(&env);

        env2.set(
            "a",
            TypeEnvValue {
                typeref: TypeRef::unit(),
                is_const: false,
            },
        );

        assert_eq!(
            Some(TypeEnvValue {
                typeref: TypeRef::unit(),
                is_const: false,
            }),
            env2.get("a")
        );

        assert_eq!(
            Some(TypeEnvValue {
                typeref: TypeRef::number(),
                is_const: false,
            }),
            env.get("a")
        );
    }

    #[test]
    fn env_type_aliases_work() {
        let mut env = TypeEnvironment::new();

        env.set(
            "Int",
            TypeEnvValue {
                typeref: TypeRef::typed(TypeRef::number()),
                is_const: true,
            },
        );

        assert_eq!(
            Some(TypeEnvValue {
                typeref: TypeRef::number(),
                is_const: true,
            }),
            env.get("Int")
        );
    }

    #[test]
    fn env_type_aliases_work_nested() {
        let mut env = TypeEnvironment::new();

        env.set(
            "NumberList",
            TypeEnvValue {
                typeref: TypeRef::typed(TypeRef::number()),
                is_const: true,
            },
        );

        env.set(
            "Alias",
            TypeEnvValue {
                typeref: TypeRef::typed(TypeRef("NumberList".to_string(), vec![])),
                is_const: true,
            },
        );

        assert_eq!(
            Some(TypeEnvValue {
                typeref: TypeRef::number(),
                is_const: true,
            }),
            env.get("Alias")
        );
    }
}
