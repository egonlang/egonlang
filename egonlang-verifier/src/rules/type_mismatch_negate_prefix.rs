use egonlang_core::{
    ast::{Expr, OpPrefix, Stmt, TypeRef},
    errors::TypeError,
    span::Span,
};

use crate::{type_env::TypeEnv, verifier::VerificationResult};

use crate::rules::rule::Rule;

pub struct TypeMisMatchNegatePrefixRule;
impl<'a> Rule<'a> for TypeMisMatchNegatePrefixRule {
    fn visit_stmt(&self, _stmt: &Stmt, _span: &Span, _types: &mut TypeEnv) -> VerificationResult {
        Ok(())
    }

    fn visit_expr(&self, expr: &Expr, _span: &Span, types: &mut TypeEnv) -> VerificationResult {
        match expr {
            Expr::Prefix(prefix_expr) => match prefix_expr.op {
                OpPrefix::Negate => {
                    let (value_expr, value_span) = &prefix_expr.rt;
                    let value_typeref = types.resolve_expr_type(value_expr, value_span)?;

                    if value_typeref != TypeRef::number() {
                        return Err(vec![(
                            TypeError::MismatchType {
                                expected: TypeRef::number().to_string(),
                                actual: value_typeref.to_string(),
                            }
                            .into(),
                            value_span.clone(),
                        )]);
                    }
                }
                OpPrefix::Not => {
                    let (value_expr, value_span) = &prefix_expr.rt;
                    let value_typeref = types.resolve_expr_type(value_expr, value_span)?;

                    if value_typeref != TypeRef::bool() {
                        return Err(vec![(
                            TypeError::MismatchType {
                                expected: TypeRef::bool().to_string(),
                                actual: value_typeref.to_string(),
                            }
                            .into(),
                            value_span.clone(),
                        )]);
                    }
                }
            },
            _ => {}
        };

        Ok(())
    }
}

#[cfg(test)]
mod type_mismatch_negate_prefix_tests {
    use egonlang_core::{
        ast::{Expr, ExprLiteral, ExprPrefix, TypeRef},
        errors::TypeError,
    };
    use pretty_assertions::assert_eq;

    use crate::{rules::rule::Rule, type_env::TypeEnv};

    use super::TypeMisMatchNegatePrefixRule;

    #[test]
    fn returns_ok_if_negate_prefix_on_number_literal_expr() {
        let rule = TypeMisMatchNegatePrefixRule;

        let mut types = TypeEnv::new();

        let expr: Expr = ExprPrefix {
            op: egonlang_core::ast::OpPrefix::Negate,
            rt: (ExprLiteral::Number(100f64).into(), 0..0),
        }
        .into();

        let span = 0..0;

        assert_eq!(Ok(()), rule.visit_expr(&expr, &span, &mut types));
    }

    #[test]
    fn returns_err_if_negate_prefix_on_bool_literal_expr() {
        let rule = TypeMisMatchNegatePrefixRule;

        let mut types = TypeEnv::new();

        let expr: Expr = ExprPrefix {
            op: egonlang_core::ast::OpPrefix::Negate,
            rt: (ExprLiteral::Bool(true).into(), 0..0),
        }
        .into();

        let span = 0..0;

        assert_eq!(
            Err(vec![(
                TypeError::MismatchType {
                    expected: TypeRef::number().to_string(),
                    actual: TypeRef::bool().to_string()
                }
                .into(),
                0..0
            )]),
            rule.visit_expr(&expr, &span, &mut types)
        );
    }
}
