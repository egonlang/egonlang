use egonlang_core::{
    ast::{Expr, Stmt},
    errors::SyntaxError,
    span::Span,
};

use crate::{type_env::TypeEnv, verifier::VerificationResult, verify_trace};

use crate::rules::rule::Rule;

pub struct ReassigningConstValueRule;
impl<'a> Rule<'a> for ReassigningConstValueRule {
    fn visit_stmt(&self, _stmt: &Stmt, _span: &Span, _types: &mut TypeEnv) -> VerificationResult {
        Ok(())
    }

    fn visit_expr(&self, expr: &Expr, span: &Span, types: &mut TypeEnv) -> VerificationResult {
        let mut errs = vec![];

        if let Expr::Assign(expr_assign) = expr {
            verify_trace!("Verifying assign expression doesn't reassign const: {expr}");

            let identifier = &expr_assign.identifier.name;
            let type_env_value = types.get(identifier);
            let is_const = type_env_value.map(|x| x.is_const).unwrap_or(false);

            if is_const {
                verify_trace!("Error: Reassigned const: {expr}");

                errs.push((
                    SyntaxError::ReassigningConst {
                        name: identifier.clone(),
                    }
                    .into(),
                    span.clone(),
                ));
            }
        };

        if !errs.is_empty() {
            return Err(errs);
        }

        Ok(())
    }
}

#[cfg(test)]
mod reassigning_const_values_tests {
    use egonlang_core::{
        ast::{Expr, ExprAssign, ExprLiteral, Identifier, TypeRef},
        errors::SyntaxError,
    };
    use pretty_assertions::assert_eq;

    use crate::{
        rules::rule::Rule,
        type_env::{TypeEnv, TypeEnvValue},
    };

    use super::ReassigningConstValueRule;

    #[test]
    fn returns_ok_if_identifier_not_const() {
        let rule = ReassigningConstValueRule;

        let mut types = TypeEnv::new();

        types.set(
            "a",
            TypeEnvValue {
                typeref: TypeRef::number(),
                is_const: false,
            },
        );

        let expr: Expr = ExprAssign {
            identifier: Identifier {
                name: "a".to_string(),
            },
            value: (ExprLiteral::Number(100f64).into(), 0..0),
        }
        .into();

        let span = 0..0;

        assert_eq!(Ok(()), rule.visit_expr(&expr, &span, &mut types));
    }

    #[test]
    fn returns_err_if_identifier_is_const() {
        let rule = ReassigningConstValueRule;

        let mut types = TypeEnv::new();

        types.set(
            "a",
            TypeEnvValue {
                typeref: TypeRef::number(),
                is_const: true,
            },
        );

        let expr: Expr = ExprAssign {
            identifier: Identifier {
                name: "a".to_string(),
            },
            value: (ExprLiteral::Number(100f64).into(), 0..0),
        }
        .into();

        let span = 0..10;

        assert_eq!(
            Err(vec![(
                SyntaxError::ReassigningConst {
                    name: "a".to_string()
                }
                .into(),
                span.clone()
            )]),
            rule.visit_expr(&expr, &span, &mut types)
        );
    }
}
