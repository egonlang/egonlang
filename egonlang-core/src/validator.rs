use crate::{
    ast::{Expr, Identifier, Module, Stmt},
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
                if stmt_assign.type_identifier.is_none() && stmt_assign.value.is_none() {
                    return Err(vec![(
                        Error::SyntaxError(SyntaxError::UninitializedUntypedLet { name }),
                        span.clone(),
                    )]);
                }

                if stmt_assign.type_identifier.is_some() && stmt_assign.value.is_some() {
                    let identifier = stmt_assign.type_identifier.clone().unwrap();
                    let type_identifier = identifier.name;
                    let value = stmt_assign.value.clone().unwrap();
                    let value_type = value.0.get_type_identifier();

                    if type_identifier != value_type {
                        return Err(vec![(
                            Error::TypeError(crate::errors::TypeError::MismatchType {
                                expected: type_identifier,
                                actual: value_type,
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

    fn visit_expr(&mut self, _expr: &Spanned<Expr>) -> Result<(), Vec<ErrorS>> {
        Ok(())
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
}
