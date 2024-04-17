use crate::{
    ast::{
        Expr, ExprInfix, ExprPrefix, ExprS, Identifier, Module, OpInfix, OpPrefix, Stmt, TypeRef,
    },
    errors::{Error, ErrorS, SyntaxError, TypeError},
    span::Spanned,
};

#[derive(Default)]
pub struct Validator;

impl Validator {
    /// Validate a [`Module`]'s AST
    pub fn validate(&mut self, module: &Module) -> Result<(), Vec<ErrorS>> {
        let mut all_errs: Vec<ErrorS> = vec![];

        for stmt in &module.stmts {
            if let Err(errs) = self.visit_stmt(stmt) {
                all_errs.extend(errs);
            }
        }

        if all_errs.is_empty() {
            return Ok(());
        }

        Err(all_errs)
    }

    #[allow(dead_code)]
    fn visit_ident(&mut self, _ident: &Spanned<Identifier>) -> Result<(), Vec<ErrorS>> {
        Ok(())
    }

    fn visit_stmt(&mut self, stmt: &Spanned<Stmt>) -> Result<(), Vec<ErrorS>> {
        let (stmt, span) = stmt;

        match stmt {
            Stmt::Expr(stmt_expr) => self.visit_expr(&stmt_expr.expr),
            Stmt::Assign(stmt_assign) => {
                let name = stmt_assign.identifier.name.clone();

                // const declarations
                if stmt_assign.is_const && stmt_assign.value.is_none() {
                    return Err(vec![(
                        Error::SyntaxError(SyntaxError::UninitializedConst { name }),
                        span.clone(),
                    )]);
                }

                // let declarations
                if stmt_assign.type_expr.is_none() && stmt_assign.value.is_none() {
                    return Err(vec![(
                        Error::SyntaxError(SyntaxError::UninitializedUntypedLet { name }),
                        span.clone(),
                    )]);
                }

                if stmt_assign.type_expr.is_some() {
                    if stmt_assign.value.is_some() {
                        let (type_expr, _) = stmt_assign.type_expr.clone().unwrap();
                        let type_identifier = type_expr.get_type_expr();
                        let value = stmt_assign.value.clone().unwrap();
                        let value_type = value.0.get_type_expr();

                        if type_identifier != value_type {
                            // This checks for empty lists being assigned to a list<T>
                            // e.g. let a: list<number> = [];
                            if type_identifier.0 == TypeRef::list(TypeRef::unknown()).0 {
                                if value_type == TypeRef::list(TypeRef::unknown()) {
                                    return Ok(());
                                }
                            };

                            return Err(vec![(
                                Error::TypeError(crate::errors::TypeError::MismatchType {
                                    expected: type_identifier.to_string(),
                                    actual: value_type.to_string(),
                                }),
                                span.clone(),
                            )]);
                        }
                    }
                } else {
                    // This checks for empty lists being assigned to a untyped list<unknown>
                    // e.g. let a = [];
                    if stmt_assign.value.is_some() {
                        let value = stmt_assign.value.clone().unwrap();
                        let value_type = value.0.get_type_expr();

                        if value_type == TypeRef::list(TypeRef::unknown()) {
                            return Err(vec![(
                                Error::TypeError(crate::errors::TypeError::UknownListType {}),
                                span.clone(),
                            )]);
                        }
                    }
                }

                Ok(())
            }
            Stmt::Error => todo!(),
        }
    }

    fn visit_expr(&mut self, expr: &Spanned<Expr>) -> Result<(), Vec<ErrorS>> {
        let (expr, _) = expr;

        match expr {
            Expr::List(list) => {
                if list.items.is_empty() {
                    return Ok(());
                }

                let (first_item, _) = list.items.first().unwrap().clone();

                let first_item_type_ident = &first_item.get_type_expr();

                let remaining_items: Vec<ExprS> = list.items.clone().into_iter().skip(1).collect();

                let mut errs = vec![];

                for (item_expr, item_span) in remaining_items {
                    let item_expr_type_ident = &item_expr.get_type_expr();

                    if item_expr_type_ident != first_item_type_ident {
                        errs.push((
                            Error::TypeError(TypeError::MismatchType {
                                expected: first_item_type_ident.to_string(),
                                actual: item_expr_type_ident.to_string(),
                            }),
                            item_span.clone(),
                        ));
                    }
                }

                Err(errs)
            }
            Expr::If(if_) => {
                let mut errs = vec![];

                if let Err(cond_errs) = self.visit_expr(&if_.cond) {
                    errs.extend(cond_errs);
                }

                if let Err(then_errs) = self.visit_expr(&if_.then) {
                    errs.extend(then_errs);
                }

                if let Some(else_errs) = &if_.else_ {
                    if let Err(else_errs) = self.visit_expr(else_errs) {
                        errs.extend(else_errs);
                    }
                }

                let cond_expr = if_.cond.0.clone().get_type_expr();

                if cond_expr.0 != *"bool" {
                    return Err(vec![(
                        Error::TypeError(crate::errors::TypeError::MismatchType {
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
                            Error::TypeError(crate::errors::TypeError::MismatchType {
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
                    OpInfix::Add => self.validate_infix_types(infix, number_typeref),
                    OpInfix::Subtract => self.validate_infix_types(infix, number_typeref),
                    OpInfix::Multiply => self.validate_infix_types(infix, number_typeref),
                    OpInfix::Divide => self.validate_infix_types(infix, number_typeref),
                    OpInfix::Modulus => self.validate_infix_types(infix, number_typeref),
                    OpInfix::Less => self.validate_infix_types(infix, number_typeref),
                    OpInfix::LessEqual => self.validate_infix_types(infix, number_typeref),
                    OpInfix::Greater => self.validate_infix_types(infix, number_typeref),
                    OpInfix::GreaterEqual => self.validate_infix_types(infix, number_typeref),
                    OpInfix::LogicAnd => self.validate_infix_types(infix, bool_typeref),
                    OpInfix::LogicOr => self.validate_infix_types(infix, bool_typeref),
                    _ => Ok(()),
                }
            }
            Expr::Prefix(prefix) => {
                let number_typeref = TypeRef::number();
                let bool_typeref = TypeRef::bool();

                match prefix.op {
                    OpPrefix::Negate => self.validate_prefix_types(prefix, number_typeref),
                    OpPrefix::Not => self.validate_prefix_types(prefix, bool_typeref),
                }
            }
            Expr::Block(block) => {
                let mut errs: Vec<ErrorS> = vec![];

                for stmt in &block.stmts {
                    if let Err(stmt_errs) = self.visit_stmt(&stmt) {
                        errs.extend(stmt_errs);
                    }
                }

                if let Some(returning_expr) = &block.return_expr {
                    if let Err(expr_errs) = self.visit_expr(&returning_expr) {
                        errs.extend(expr_errs);
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
    ) -> Result<(), Vec<ErrorS>> {
        let mut errs = vec![];

        let lt_errs = self.visit_expr(&infix.lt).err().unwrap_or_default();
        errs.extend(lt_errs);

        let rt_errs = self.visit_expr(&infix.rt).err().unwrap_or_default();
        errs.extend(rt_errs);

        let (lt, lt_span) = infix.lt.clone();
        let lt_typeref = lt.get_type_expr();
        let (rt, rt_span) = infix.rt.clone();
        let rt_typeref = rt.get_type_expr();

        if lt_typeref != expected_type {
            errs.push((
                Error::TypeError(TypeError::MismatchType {
                    expected: expected_type.to_string(),
                    actual: lt_typeref.to_string(),
                }),
                lt_span,
            ));
        }
        if rt_typeref != expected_type {
            errs.push((
                Error::TypeError(TypeError::MismatchType {
                    expected: expected_type.to_string(),
                    actual: rt_typeref.to_string(),
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
    ) -> Result<(), Vec<ErrorS>> {
        let mut errs = vec![];

        let rt_errs = self.visit_expr(&prefix.rt).err().unwrap_or_default();
        errs.extend(rt_errs);

        let (rt, rt_span) = prefix.rt.clone();
        let rt_typeref = rt.get_type_expr();

        if rt_typeref != expected_type {
            errs.push((
                Error::TypeError(TypeError::MismatchType {
                    expected: expected_type.to_string(),
                    actual: rt_typeref.to_string(),
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
        errors::{Error, TypeError},
        parser::parse,
    };

    use super::Validator;

    macro_rules! validator_test {
        ($test_name:ident, $input:expr, $expected:expr) => {
            #[test]
            fn $test_name() {
                let result =
                    parse($input, 0).and_then(|module| Validator::default().validate(&module));

                assert_eq!(result, $expected);
            }
        };
    }

    validator_test!(validate_number_expr_stmt, "123;", Ok(()));

    validator_test!(
        validate_const_declaration_requires_value,
        "const a;",
        Err(vec![(
            Error::SyntaxError(crate::errors::SyntaxError::UninitializedConst {
                name: "a".to_string()
            }),
            0..8
        )])
    );

    validator_test!(
        validate_const_declaration_requires_value_with_value,
        "const a = 123;",
        Ok(())
    );

    validator_test!(
        validate_let_declaration_requires_type_or_value,
        "let a;",
        Err(vec![(
            Error::SyntaxError(crate::errors::SyntaxError::UninitializedUntypedLet {
                name: "a".to_string()
            }),
            0..6
        )])
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
            { void },
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
                63..71
            ),
            (
                Error::TypeError(TypeError::MismatchType {
                    expected: "number".to_string(),
                    actual: "range".to_string()
                }),
                85..90
            ),
            (
                Error::TypeError(TypeError::MismatchType {
                    expected: "number".to_string(),
                    actual: "bool".to_string()
                }),
                104..109
            ),
            (
                Error::TypeError(TypeError::MismatchType {
                    expected: "number".to_string(),
                    actual: "string".to_string()
                }),
                123..158
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
        validate_assign_mismatch_types_block_returning_void,
        "let a: () = { void };",
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
        "if (true and 1) { void; } else { void; };",
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
}
