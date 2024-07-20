use egonlang_core::{ast::Expr, errors::SyntaxError, span::Span};

use crate::{expr_rule, type_env::TypeEnv, verifier::VerificationResult, verify_trace};

expr_rule!(
    ReassigningConstValueRule,
    fn (expr: &Expr, span: &Span, types: &mut TypeEnv) {
        let mut errs = vec![];

        if let Expr::Assign(expr_assign) = expr {
            verify_trace!(
                "Verifying assign expression doesn't reassign const: {}",
                expr.to_string().cyan()
            );

            let identifier = &expr_assign.identifier.name;
            let type_env_value = types.get(identifier);
            let is_const = type_env_value.map(|x| x.is_const).unwrap_or(false);

            if is_const {
                verify_trace!(error: "Reassigned const: {}", expr.to_string().cyan());

                errs.push((
                    SyntaxError::ReassigningConst {
                        name: identifier.clone(),
                    }
                    .into(),
                    span.clone(),
                ));
            }
        };

        errs
    }
);

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
