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

    fn visit_ident(&mut self, ident: &Spanned<Identifier>) -> Result<(), Vec<ErrorS>> {
        Ok(())
    }

    fn visit_stmt(&mut self, stmt: &Spanned<Stmt>) -> Result<(), Vec<ErrorS>> {
        if let (stmt, span) = stmt {
            return match stmt {
                Stmt::Expr(stmt_expr) => self.visit_expr(&stmt_expr.expr),
                Stmt::Assign(stmt_assign) => {
                    if let None = stmt_assign.value {
                        let name = stmt_assign.identifier.name.clone();

                        if stmt_assign.is_const {
                            return Err(vec![(
                                Error::SyntaxError(SyntaxError::UninitializedConst { name }),
                                span.clone(),
                            )]);
                        } else if stmt_assign.type_identifier.is_none() {
                            return Err(vec![(
                                Error::SyntaxError(SyntaxError::UninitializedUntypedLet { name }),
                                span.clone(),
                            )]);
                        }
                    };

                    Ok(())
                }
                Stmt::Error => todo!(),
            };
        };

        Ok(())
    }

    fn visit_expr(&mut self, expr: &Spanned<Expr>) -> Result<(), Vec<ErrorS>> {
        Ok(())
    }
}

#[cfg(test)]
mod parser_tests {
    use pretty_assertions::assert_eq;

    use crate::{errors::Error, parser::parse};

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
        validate_let_declaration_requires_type_or_value,
        "let a;",
        Err(vec![(
            Error::SyntaxError(crate::errors::SyntaxError::UninitializedUntypedLet {
                name: "a".to_string()
            }),
            0..6
        )])
    );
}
