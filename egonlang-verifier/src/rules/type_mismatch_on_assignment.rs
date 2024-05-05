use egonlang_core::{ast::Stmt, errors::TypeError, span::Span};

use crate::{type_env::TypeEnv, verifier::VerificationResult};

use super::Rule;

pub struct TypeMisMatchOnAssignmentRule;
impl<'a> Rule<'a> for TypeMisMatchOnAssignmentRule {
    fn visit_stmt(&self, stmt: &Stmt, _span: &Span, types: &mut TypeEnv<'a>) -> VerificationResult {
        if let Stmt::Assign(stmt_assign) = stmt {
            if stmt_assign.type_expr.is_some() && stmt_assign.value.is_some() {
                let (assign_type_expr, assign_type_expr_span) =
                    stmt_assign.type_expr.as_ref().unwrap();
                let (value_expr, value_span) = stmt_assign.value.as_ref().unwrap();

                let assign_typeref =
                    types.resolve_expr_type(assign_type_expr, assign_type_expr_span)?;

                let value_typeref = types.resolve_expr_type(value_expr, value_span)?;

                if assign_typeref != value_typeref {
                    return Err(vec![(
                        TypeError::MismatchType {
                            expected: assign_typeref.to_string(),
                            actual: value_typeref.to_string(),
                        }
                        .into(),
                        value_span.clone(),
                    )]);
                }
            }
        }

        Ok(())
    }

    fn visit_expr(
        &self,
        expr: &egonlang_core::ast::Expr,
        _span: &Span,
        _types: &mut TypeEnv<'a>,
    ) -> VerificationResult {
        let _ = expr;
        Ok(())
    }
}

#[cfg(test)]
mod type_mismatch_on_assignment_tests {
    use egonlang_core::{
        ast::{ExprLiteral, ExprType, Identifier, Stmt, StmtAssign, TypeRef},
        errors::TypeError,
    };
    use pretty_assertions::assert_eq;

    use crate::{rules::Rule, type_env::TypeEnv};

    use super::TypeMisMatchOnAssignmentRule;

    #[test]
    fn returns_ok_if_assignment_type_and_value_type_match() {
        let rule = TypeMisMatchOnAssignmentRule;

        let mut types = TypeEnv::new();

        let stmt: Stmt = StmtAssign {
            identifier: Identifier {
                name: "a".to_string(),
            },
            type_expr: Some((ExprType(TypeRef::number()).into(), 0..0)),
            is_const: true,
            value: Some((ExprLiteral::Number(123f64).into(), 0..0)),
        }
        .into();

        let span = 0..0;

        assert_eq!(Ok(()), rule.visit_stmt(&stmt, &span, &mut types));
    }

    #[test]
    fn returns_err_if_assignment_type_and_value_type_mismatch() {
        let rule = TypeMisMatchOnAssignmentRule;

        let mut types = TypeEnv::new();

        let stmt: Stmt = StmtAssign {
            identifier: Identifier {
                name: "a".to_string(),
            },
            type_expr: Some((ExprType(TypeRef::unit()).into(), 0..1)),
            is_const: true,
            value: Some((ExprLiteral::Number(123f64).into(), 2..3)),
        }
        .into();

        let span = 0..0;

        assert_eq!(
            Err(vec![(
                TypeError::MismatchType {
                    expected: TypeRef::unit().to_string(),
                    actual: TypeRef::number().to_string()
                }
                .into(),
                2..3
            )]),
            rule.visit_stmt(&stmt, &span, &mut types)
        );
    }
}
