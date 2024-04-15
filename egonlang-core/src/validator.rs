use crate::{
    ast::{Expr, ExprS, Identifier, Module, Stmt},
    errors::{Error, ErrorS, SyntaxError},
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
                if stmt_assign.is_const {
                    if stmt_assign.value.is_none() {
                        return Err(vec![(
                            Error::SyntaxError(SyntaxError::UninitializedConst { name }),
                            span.clone(),
                        )]);
                    }
                }

                // let declarations
                if stmt_assign.type_expr.is_none() && stmt_assign.value.is_none() {
                    return Err(vec![(
                        Error::SyntaxError(SyntaxError::UninitializedUntypedLet { name }),
                        span.clone(),
                    )]);
                }

                if stmt_assign.type_expr.is_some() && stmt_assign.value.is_some() {
                    let (type_expr, _) = stmt_assign.type_expr.clone().unwrap();
                    let type_identifier = type_expr.get_type_expr();
                    let value = stmt_assign.value.clone().unwrap();
                    let value_type = value.0.get_type_expr();

                    if type_identifier != value_type {
                        return Err(vec![(
                            Error::TypeError(crate::errors::TypeError::MismatchType {
                                expected: type_identifier.to_string(),
                                actual: value_type.to_string(),
                            }),
                            span.clone(),
                        )]);
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
                            Error::TypeError(crate::errors::TypeError::MismatchType {
                                expected: first_item_type_ident.to_string(),
                                actual: item_expr_type_ident.to_string(),
                            }),
                            item_span.clone(),
                        ));
                    }
                }

                Err(errs)
            }
            _ => Ok(()),
        }
    }
}

#[cfg(test)]
mod parser_tests {
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
        "let a: Number;",
        Ok(())
    );

    validator_test!(
        validate_let_declarations_with_value_and_type_must_match,
        "let a: Number = \"foo\";",
        Err(vec![(
            Error::TypeError(TypeError::MismatchType {
                expected: "Number".to_string(),
                actual: "String".to_string()
            }),
            0..22
        )])
    );

    validator_test!(
        validate_let_decl_typed_as_number_with_range_value_type,
        "let a: Number = 0..10;",
        Err(vec![(
            Error::TypeError(TypeError::MismatchType {
                expected: "Number".to_string(),
                actual: "Range".to_string()
            }),
            0..22
        )])
    );

    validator_test!(
        validate_let_decl_typed_as_number_with_list_value_type,
        "let a: Number = [1, 2, 3];",
        Err(vec![(
            Error::TypeError(TypeError::MismatchType {
                expected: "Number".to_string(),
                actual: "List".to_string()
            }),
            0..26
        )])
    );

    validator_test!(
        validate_let_decl_typed_as_tuple_with_tuple_value_type,
        "let a: Tuple = (1, 2, 3,);",
        Ok(())
    );

    validator_test!(
        validate_let_decl_with_value_as_block_with_returning_expr,
        "let a: Number = { 123 };",
        Ok(())
    );

    validator_test!(
        validate_let_decl_with_value_as_block_with_returning_expr_mismatch_types,
        "let a: Number = { \"foo\" };",
        Err(vec![(
            Error::TypeError(TypeError::MismatchType {
                expected: "Number".to_string(),
                actual: "String".to_string()
            }),
            0..26
        )])
    );

    validator_test!(
        validate_let_decl_with_value_as_block_without_returning_expr,
        "let a: Void = { 123; };",
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
                    expected: "Number".to_string(),
                    actual: "String".to_string()
                }),
                44..49
            ),
            (
                Error::TypeError(TypeError::MismatchType {
                    expected: "Number".to_string(),
                    actual: "Void".to_string()
                }),
                63..71
            ),
            (
                Error::TypeError(TypeError::MismatchType {
                    expected: "Number".to_string(),
                    actual: "Range".to_string()
                }),
                85..90
            ),
            (
                Error::TypeError(TypeError::MismatchType {
                    expected: "Number".to_string(),
                    actual: "Bool".to_string()
                }),
                104..109
            ),
            (
                Error::TypeError(TypeError::MismatchType {
                    expected: "Number".to_string(),
                    actual: "String".to_string()
                }),
                123..158
            )
        ])
    );

    validator_test!(
        validate_assign_mismatch_types_block_returning_string,
        "let a: Void = { \"foo\" };",
        Err(vec![(
            Error::TypeError(TypeError::MismatchType {
                expected: "Void".to_string(),
                actual: "String".to_string()
            }),
            0..24
        )])
    );

    validator_test!(
        validate_assign_mismatch_types_block_returning_number,
        "let a: Void = { 123 };",
        Err(vec![(
            Error::TypeError(TypeError::MismatchType {
                expected: "Void".to_string(),
                actual: "Number".to_string()
            }),
            0..22
        )])
    );

    validator_test!(
        validate_assign_mismatch_types_block_returning_void,
        "let a: Void = { void };",
        Ok(())
    );

    validator_test!(
        validate_assign_mismatch_types_block_returning_list,
        "let a: Void = { [1, 2, 3] };",
        Err(vec![(
            Error::TypeError(TypeError::MismatchType {
                expected: "Void".to_string(),
                actual: "List".to_string()
            }),
            0..28
        )])
    );

    validator_test!(
        validate_assign_mismatch_types_block_returning_tuple,
        "let a: Void = { (1, 2, 3,) };",
        Err(vec![(
            Error::TypeError(TypeError::MismatchType {
                expected: "Void".to_string(),
                actual: "Tuple".to_string()
            }),
            0..29
        )])
    );

    validator_test!(
        validate_assign_mismatch_types_block_returning_bool,
        "let a: Void = { false };",
        Err(vec![(
            Error::TypeError(TypeError::MismatchType {
                expected: "Void".to_string(),
                actual: "Bool".to_string()
            }),
            0..24
        )])
    );

    validator_test!(
        validate_assign_mismatch_types_nested_block_returning_number,
        "let a: Void = { { 123 } };",
        Err(vec![(
            Error::TypeError(TypeError::MismatchType {
                expected: "Void".to_string(),
                actual: "Number".to_string()
            }),
            0..26
        )])
    );

    validator_test!(
        validate_assign_chain_mismatched_types,
        "let a: Void = b = 123;",
        Err(vec![(
            Error::TypeError(TypeError::MismatchType {
                expected: "Void".to_string(),
                actual: "Number".to_string()
            }),
            0..22
        )])
    );

    validator_test!(
        validate_assign_chain_matching_types,
        "let a: Number = b = 123;",
        Ok(())
    );
}
