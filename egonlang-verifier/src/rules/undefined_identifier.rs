use egonlang_core::{
    ast::{Expr, ExprIdentifier, Stmt},
    errors::TypeError,
    span::Span,
};

use crate::{type_env::TypeEnv, verifier::VerificationResult, verify_trace};

use crate::rules::rule::Rule;

pub struct UndefinedIdentifierRule;
impl<'a> Rule<'a> for UndefinedIdentifierRule {
    fn visit_stmt(&self, _stmt: &Stmt, _span: &Span, _types: &mut TypeEnv) -> VerificationResult {
        Ok(())
    }

    fn visit_expr(&self, expr: &Expr, span: &Span, types: &mut TypeEnv) -> VerificationResult {
        if let Expr::Identifier(ExprIdentifier { identifier }) = expr {
            verify_trace!(
                "Verifying identifier expression: {}",
                expr.to_string().cyan()
            );

            let name = &identifier.name;

            if types.get(name).is_none() {
                verify_trace!("Error: identifier not defined: {expr}");

                return Err(vec![(
                    TypeError::Undefined(name.to_string()).into(),
                    span.clone(),
                )]);
            }
        }

        Ok(())
    }
}

#[cfg(test)]
mod undefined_identifier_test {
    use egonlang_core::{
        ast::{Expr, Identifier, TypeRef},
        errors::TypeError,
    };
    use pretty_assertions::assert_eq;

    use crate::{
        rules::rule::Rule,
        type_env::{TypeEnv, TypeEnvValue},
    };

    use super::UndefinedIdentifierRule;

    #[test]
    fn returns_error_if_identifier_is_undefined() {
        let rule = UndefinedIdentifierRule;

        let mut types = TypeEnv::new();

        let expr: Expr = Identifier {
            name: "a".to_string(),
        }
        .into();

        let span = 0..0;

        assert_eq!(
            Err(vec![(
                TypeError::Undefined("a".to_string()).into(),
                span.clone()
            )]),
            rule.visit_expr(&expr, &span, &mut types)
        );
    }

    #[test]
    fn returns_ok_if_identifier_is_defined() {
        let rule = UndefinedIdentifierRule;

        let mut types = TypeEnv::new();

        types.set(
            "a",
            TypeEnvValue {
                typeref: TypeRef::unit(),
                is_const: true,
            },
        );

        let expr: Expr = Identifier {
            name: "a".to_string(),
        }
        .into();

        let span = 0..0;

        assert_eq!(Ok(()), rule.visit_expr(&expr, &span, &mut types));
    }
}
