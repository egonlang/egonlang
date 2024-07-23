use egonlang_core::{
    ast::{Expr, Module, Stmt},
    errors::ErrorS,
    span::Span,
};

use crate::{
    prelude::*,
    type_env::{TypeEnv, TypeEnvValue},
    verify_trace,
    visitor::Visitor,
};

pub type VerificationResult = Result<(), Vec<ErrorS>>;

/// Verify an AST [`Module`] using the registered [`Rule`] set
///
/// ```
/// use egonlang_core::prelude::*;
/// use egonlang_verifier::prelude::*;
///
/// /// Verifier with the default [`Rule`] set
/// let verifier = Verifier::default();
///
/// let module = parse("let a = 123;", 0).expect("Unable to parse");
///
/// let result = verify_module(&module);
///
/// matches!(result, Ok(()));
/// ```
pub struct Verifier<'a> {
    rules: Vec<Box<dyn rules::Rule<'a>>>,
}

impl Default for Verifier<'_> {
    /// Create a [`Verifier`] with the [`Rule`] set
    fn default() -> Self {
        Self::new().with_default_rules()
    }
}

impl<'a> Verifier<'a> {
    /// Create a [`Verifier`] with no default [`Rule`] set
    pub fn new() -> Self {
        Verifier {
            rules: Default::default(),
        }
    }

    /// Register a [`Rule`]
    ///
    /// ```ignore
    /// use egonlang_verifier::prelude::*;
    ///
    /// let verifier = Verifier::new().with_rule(TypeMismatchInfixRule);
    /// ```
    pub fn add_rule<R: rules::Rule<'a> + 'static>(&mut self, rule: R) {
        self.rules.push(Box::new(rule));
    }

    /// Register the core language [`Rule`] set
    ///
    /// ```
    /// use egonlang_verifier::prelude::*;
    ///
    /// // These are the same
    /// let verifier = Verifier::new().with_default_rules();
    /// let verifier = Verifier::default();
    /// ```
    pub fn with_default_rules(mut self) -> Self {
        self.add_rule(rules::core::TypeMismatchInfixRule);
        self.add_rule(rules::core::TypeMismatchPrefixRule);
        self.add_rule(rules::core::TypeMisMatchListItemsRule);
        self.add_rule(rules::core::TypeMismatchOnDeclarationsRule);
        self.add_rule(rules::core::DeclareConstWithoutValueRule);
        self.add_rule(rules::core::ReassigningConstValueRule);
        self.add_rule(rules::core::ReferencingUndefinedIdentifierRule);
        self.add_rule(rules::core::DivideByZeroRule);
        self.add_rule(rules::core::TypeMismatchFnReturnExprRule);
        self.add_rule(rules::core::TypeMismatchIfCondExprRule);
        self.add_rule(rules::core::TypeMismatchReassigningLetValuesRule);
        self.add_rule(rules::core::TypeMismatchIfthenElseExprRule);
        self.add_rule(rules::core::InvalidTypeAliasNameRule);

        self
    }

    /// Verify an AST [`Module`] using the registered [`Rule`] set
    pub fn verify(&self, module: &Module) -> VerificationResult {
        let mut all_errs: Vec<ErrorS> = vec![];

        let mut types = TypeEnv::new();

        for (stmt, stmt_span) in &module.stmts {
            if let Err(stmt_errs) = self.visit_stmt(stmt, stmt_span, &mut types) {
                all_errs.extend(stmt_errs);
            }
        }

        if !all_errs.is_empty() {
            return Err(all_errs);
        }

        Ok(())
    }
}

impl<'a> Visitor<'a> for Verifier<'a> {
    fn visit_stmt(&self, stmt: &Stmt, span: &Span, types: &mut TypeEnv) -> Result<(), Vec<ErrorS>> {
        let mut errs: Vec<ErrorS> = vec![];

        verify_trace!(visit_stmt: "{}", stmt.to_string().cyan());

        let result = match stmt {
            Stmt::Expr(stmt_expr) => {
                let (expr, expr_span) = &stmt_expr.expr;
                let expr_errs = self
                    .visit_expr(expr, expr_span, types)
                    .err()
                    .unwrap_or_default();

                errs.extend(expr_errs);

                if !errs.is_empty() {
                    return Err(errs);
                }

                Ok(())
            }
            Stmt::Assign(stmt_assign) => {
                if let Some((type_expr, type_expr_span)) = &stmt_assign.type_expr {
                    let type_expr_errs = self
                        .visit_expr(type_expr, type_expr_span, types)
                        .err()
                        .unwrap_or_default();

                    errs.extend(type_expr_errs);
                }

                if let Some((value_expr, value_expr_span)) = &stmt_assign.value {
                    let value_expr_errs = self
                        .visit_expr(value_expr, value_expr_span, types)
                        .err()
                        .unwrap_or_default();

                    errs.extend(value_expr_errs);
                }

                if !errs.is_empty() {
                    return Err(errs);
                }

                match (&stmt_assign.type_expr, &stmt_assign.value) {
                    (None, None) => {}
                    (None, Some((value_expr, value_span))) => {
                        let value_typeref = types.resolve_expr_type(value_expr, value_span)?;
                        types.set(
                            &stmt_assign.identifier.name,
                            TypeEnvValue {
                                typeref: value_typeref,
                                is_const: stmt_assign.is_const,
                            },
                        );
                    }
                    (Some((type_expr, type_span)), None) => {
                        let type_typeref = types.resolve_expr_type(type_expr, type_span)?;

                        types.set(
                            &stmt_assign.identifier.name,
                            TypeEnvValue {
                                typeref: type_typeref,
                                is_const: stmt_assign.is_const,
                            },
                        );
                    }
                    (Some((type_expr, type_span)), Some((_value_expr, _value_span))) => {
                        let type_typeref = types.resolve_expr_type(type_expr, type_span)?;

                        types.set(
                            &stmt_assign.identifier.name,
                            TypeEnvValue {
                                typeref: type_typeref,
                                is_const: stmt_assign.is_const,
                            },
                        );
                    }
                };

                Ok(())
            }
            Stmt::Fn(stmt_fn) => {
                let (fn_expr, fn_expr_span) = &stmt_fn.fn_expr;

                let fn_expr_errs = self
                    .visit_expr(fn_expr, fn_expr_span, types)
                    .err()
                    .unwrap_or_default();

                errs.extend(fn_expr_errs);

                if !errs.is_empty() {
                    return Err(errs);
                }

                //
                Ok(())
            }
            Stmt::Error => Ok(()),
        };

        for rule in &self.rules {
            let rule_errs = rule.visit_stmt(stmt, span, types).err().unwrap_or_default();

            errs.extend(rule_errs);
        }

        verify_trace!("There were a total of {} errors from rules", errs.len());

        verify_trace!(
            exit_stmt:
            "{}",
            stmt.to_string().cyan()
        );

        if !errs.is_empty() {
            return Err(errs);
        }

        result
    }

    fn visit_expr(&self, expr: &Expr, span: &Span, types: &mut TypeEnv) -> Result<(), Vec<ErrorS>> {
        let mut errs: Vec<ErrorS> = vec![];

        verify_trace!(visit_expr: "{}", expr.to_string().cyan());

        match expr {
            Expr::Block(block_expr) => {
                let mut block_types = types.extend();

                for (stmt, stmt_span) in &block_expr.stmts {
                    let stmt_errs = self
                        .visit_stmt(stmt, stmt_span, &mut block_types)
                        .err()
                        .unwrap_or_default();

                    errs.extend(stmt_errs);
                }

                if block_expr.return_expr.is_some() {
                    let (return_expr, return_span) = block_expr.return_expr.as_ref().unwrap();
                    let expr_errs = self
                        .visit_expr(return_expr, return_span, &mut block_types)
                        .err()
                        .unwrap_or_default();

                    errs.extend(expr_errs);
                }

                for rule in &self.rules {
                    let rule_errs = rule
                        .visit_expr(expr, span, &mut block_types)
                        .err()
                        .unwrap_or_default();

                    errs.extend(rule_errs);
                }
            }
            Expr::Assign(assign_expr) => {
                let (value_expr, value_span) = &assign_expr.value;

                let value_errs = self
                    .visit_expr(value_expr, value_span, types)
                    .err()
                    .unwrap_or_default();

                errs.extend(value_errs);

                for rule in &self.rules {
                    let rule_errs = rule.visit_expr(expr, span, types).err().unwrap_or_default();

                    errs.extend(rule_errs);
                }
            }
            Expr::Infix(infix_expr) => {
                let (lt_expr, lt_span) = &infix_expr.lt;

                let lt_errs = self
                    .visit_expr(lt_expr, lt_span, types)
                    .err()
                    .unwrap_or_default();

                errs.extend(lt_errs);

                let (rt_expr, rt_span) = &infix_expr.rt;

                let rt_errs = self
                    .visit_expr(rt_expr, rt_span, types)
                    .err()
                    .unwrap_or_default();

                errs.extend(rt_errs);

                for rule in &self.rules {
                    let rule_errs = rule.visit_expr(expr, span, types).err().unwrap_or_default();

                    errs.extend(rule_errs);
                }
            }
            Expr::Fn(fn_expr) => {
                let mut fn_types = types.extend();

                for ((ident, type_ref), _) in &fn_expr.params {
                    let name = &ident.name;

                    fn_types.set(
                        name,
                        TypeEnvValue {
                            typeref: type_ref.clone(),
                            is_const: true,
                        },
                    );
                }

                let (body_expr, body_span) = &fn_expr.body;

                let body_errs = self
                    .visit_expr(body_expr, body_span, &mut fn_types)
                    .err()
                    .unwrap_or_default();

                errs.extend(body_errs);

                for rule in &self.rules {
                    let rule_errs = rule
                        .visit_expr(expr, span, &mut fn_types)
                        .err()
                        .unwrap_or_default();

                    errs.extend(rule_errs);
                }
            }
            Expr::If(if_expr) => {
                let (cond_expr, cond_span) = &if_expr.cond;
                let cond_errs = self
                    .visit_expr(cond_expr, cond_span, types)
                    .err()
                    .unwrap_or_default();
                errs.extend(cond_errs);

                let (then_expr, then_cond) = &if_expr.then;
                let then_errs = self
                    .visit_expr(then_expr, then_cond, types)
                    .err()
                    .unwrap_or_default();
                errs.extend(then_errs);

                if let Some((else_expr, else_span)) = &if_expr.else_ {
                    let else_errs = self
                        .visit_expr(else_expr, else_span, types)
                        .err()
                        .unwrap_or_default();
                    errs.extend(else_errs);
                }

                for rule in &self.rules {
                    let rule_errs = rule.visit_expr(expr, span, types).err().unwrap_or_default();

                    errs.extend(rule_errs);
                }
            }
            _ => {
                for rule in &self.rules {
                    let rule_errs = rule.visit_expr(expr, span, types).err().unwrap_or_default();

                    errs.extend(rule_errs);
                }
            }
        };

        verify_trace!(
            exit_expr: "{}",
            expr.to_string().cyan()
        );

        if !errs.is_empty() {
            return Err(errs);
        }

        Ok(())
    }
}

#[cfg(test)]
mod verifier_tests {
    use crate::prelude::*;
    use egonlang_core::{
        ast::{ExprAssign, ExprList, ExprLiteral, Identifier, Module, StmtExpr},
        errors::{SyntaxError, TypeError},
        prelude::*,
    };
    use pretty_assertions::assert_eq;

    use super::Verifier;

    macro_rules! verifier_test {
        ($test_name:ident, $input:expr, $expected:expr) => {
            #[test]
            fn $test_name() {
                let result = parse($input, 0).and_then(|module| verify_module(&module));

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
            (TypeError::UnknownType.into(), 0..8),
            (
                SyntaxError::UninitializedConst {
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
        Err(vec![(TypeError::UnknownType.into(), 0..6)])
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
            TypeError::MismatchType {
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
            TypeError::MismatchType {
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
            TypeError::MismatchType {
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
            TypeError::MismatchType {
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
                TypeError::MismatchType {
                    expected: "number".to_string(),
                    actual: "string".to_string()
                }
                .into(),
                44..49
            ),
            (
                TypeError::MismatchType {
                    expected: "number".to_string(),
                    actual: "()".to_string()
                }
                .into(),
                63..69
            ),
            (
                TypeError::MismatchType {
                    expected: "number".to_string(),
                    actual: "range".to_string()
                }
                .into(),
                83..88
            ),
            (
                TypeError::MismatchType {
                    expected: "number".to_string(),
                    actual: "bool".to_string()
                }
                .into(),
                102..107
            ),
            (
                TypeError::MismatchType {
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
            TypeError::MismatchType {
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
            TypeError::MismatchType {
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
            TypeError::MismatchType {
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
            TypeError::MismatchType {
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
            TypeError::MismatchType {
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
            TypeError::MismatchType {
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
            TypeError::MismatchType {
                expected: "()".to_string(),
                actual: "number".to_string()
            }
            .into(),
            12..19
        )])
    );

    verifier_test!(
        validate_assign_chain_matching_types,
        "let a: number = b = 123;",
        Ok(())
    );

    verifier_test!(
        validate_if_else_mismatched_types,
        "if (true) { 123; } else { 123 };",
        Err(vec![(
            TypeError::MismatchType {
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
            TypeError::MismatchType {
                expected: "bool".to_string(),
                actual: "number".to_string()
            }
            .into(),
            4..7
        )])
    );

    verifier_test!(
        validate_if_cond_is_bool_mismatch_number_plus_number,
        "if (123 + 456) {} else {};",
        Err(vec![(
            TypeError::MismatchType {
                expected: "bool".to_string(),
                actual: "number".to_string()
            }
            .into(),
            4..13
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
            TypeError::MismatchType {
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
            TypeError::MismatchType {
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
            TypeError::MismatchType {
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
        Err(vec![(TypeError::UknownListType.into(), 8..10)])
    );

    verifier_test!(
        validate_let_decl_empty_list_to_unknown_list,
        "let a: list<unknown> = [];",
        Err(vec![(TypeError::UknownListType.into(), 0..26)])
    );

    verifier_test!(
        validate_const_decl_empty_list_to_unknown_list,
        "const a: list<unknown> = [];",
        Err(vec![(TypeError::UknownListType.into(), 0..28)])
    );

    verifier_test!(
        validate_assign_mixed_type_list,
        "a = [1, \"a\"];",
        Err(vec![(
            TypeError::MismatchType {
                expected: "number".to_string(),
                actual: "string".to_string()
            }
            .into(),
            8..11
        )])
    );

    verifier_test!(
        validate_let_decl_unknown_type,
        "let a: unknown;",
        Err(vec![(TypeError::UnknownType.into(), 7..14)])
    );

    verifier_test!(
        validate_let_decl_untyped_sets_env_type_reassign_type_mismatch,
        "
        let a = 123;
        a = \"foo\";
        ",
        Err(vec![(
            TypeError::MismatchType {
                expected: TypeRef::number().to_string(),
                actual: TypeRef::string().to_string()
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
            TypeError::MismatchType {
                expected: TypeRef::string().to_string(),
                actual: TypeRef::number().to_string()
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
            SyntaxError::ReassigningConst {
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
            TypeError::MismatchType {
                expected: TypeRef::number().to_string(),
                actual: TypeRef::string().to_string()
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
            TypeError::MismatchType {
                expected: TypeRef::string().to_string(),
                actual: TypeRef::number().to_string()
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
            TypeError::MismatchType {
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
            TypeError::MismatchType {
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
            TypeError::MismatchType {
                expected: "number".to_string(),
                actual: "bool".to_string()
            }
            .into(),
            21..27
        )])
    );

    verifier_test!(
        validate_let_decl_typed_assigning_bang_prefixed_identifier,
        "
        let a = false;
        let b: number = { !a };
        ",
        Err(vec![(
            TypeError::MismatchType {
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
                TypeError::MismatchType {
                    expected: "number".to_string(),
                    actual: "string".to_string()
                }
                .into(),
                32..33
            ),
            (
                TypeError::MismatchType {
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
            SyntaxError::InvalidTypeAlias {
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
            SyntaxError::InvalidTypeAlias {
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
    //         TypeError::MismatchType {
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
        Err(vec![(SyntaxError::DivideByZero.into(), 0..1)])
    );

    verifier_test!(
        validate_divide_by_zero_2,
        "1 / 0;",
        Err(vec![(SyntaxError::DivideByZero.into(), 4..5)])
    );

    verifier_test!(
        validate_divide_by_zero_3,
        "0 / 0;",
        Err(vec![
            (SyntaxError::DivideByZero.into(), 0..1),
            (SyntaxError::DivideByZero.into(), 4..5)
        ])
    );

    // verifier_test!(
    //     validate_mismatch_type_when_let_decl_with_block_value,
    //     r#"
    //     let a: number = {
    //         let a: string = "foo";

    //         a
    //     };
    //     "#,
    //     Err(vec![(
    //         TypeError::MismatchType {
    //             expected: TypeRef::number().to_string(),
    //             actual: TypeRef::string().to_string()
    //         }
    //         .into(),
    //         25..93
    //     )])
    // );

    #[test]
    fn errors_when_referencing_undefined_identifier() {
        let verifier = Verifier::default();

        let module = Module::from(vec![(
            StmtExpr {
                expr: (
                    Identifier {
                        name: "a".to_string(),
                    }
                    .into(),
                    1..2,
                ),
            }
            .into(),
            0..3,
        )]);

        let results = verifier.verify(&module);

        assert_eq!(
            Err(vec![(TypeError::Undefined("a".to_string()).into(), 1..2)]),
            results
        );
    }

    #[test]
    fn errors_when_referencing_multiple_undefined_identifiers() {
        let verifier = Verifier::default();

        let module = Module::from(vec![
            (
                StmtExpr {
                    expr: (
                        Identifier {
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
                StmtExpr {
                    expr: (
                        Identifier {
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

        let results = verifier.verify(&module);

        assert_eq!(
            Err(vec![
                (TypeError::Undefined("a".to_string()).into(), 1..2),
                (TypeError::Undefined("b".to_string()).into(), 5..6)
            ]),
            results
        );
    }

    #[test]
    fn errors_when_assigning_list_with_type_mismatched_items() {
        let verifier = Verifier::default();

        let module = Module::from(vec![(
            StmtExpr {
                expr: (
                    ExprAssign {
                        identifier: Identifier {
                            name: "a".to_string(),
                        },
                        value: (
                            ExprList {
                                items: vec![
                                    (ExprLiteral::Number(10f64).into(), 0..1),
                                    (ExprLiteral::Bool(false).into(), 2..3),
                                ],
                            }
                            .into(),
                            0..0,
                        ),
                    }
                    .into(),
                    1..2,
                ),
            }
            .into(),
            0..0,
        )]);

        let results = verifier.verify(&module);

        assert_eq!(
            Err(vec![(
                TypeError::MismatchType {
                    expected: TypeRef::number().to_string(),
                    actual: TypeRef::bool().to_string()
                }
                .into(),
                2..3
            )]),
            results
        );
    }
}
