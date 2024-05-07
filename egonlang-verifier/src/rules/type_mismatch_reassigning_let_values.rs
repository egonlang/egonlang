use egonlang_core::{
    ast::{Expr, Stmt},
    errors::TypeError,
    span::Span,
};

use crate::{type_env::TypeEnv, verifier::VerificationResult, verify_trace};

use crate::rules::rule::Rule;

pub struct TypeMismatchReassigningLetValuesRule;
impl<'a> Rule<'a> for TypeMismatchReassigningLetValuesRule {
    fn visit_stmt(&self, _stmt: &Stmt, _span: &Span, _types: &mut TypeEnv) -> VerificationResult {
        Ok(())
    }

    fn visit_expr(&self, expr: &Expr, _span: &Span, types: &mut TypeEnv) -> VerificationResult {
        let mut errs = vec![];

        if let Expr::Assign(expr_assign) = expr {
            let identifier = &expr_assign.identifier.name;

            if let Some(type_env_value) = types.get(identifier) {
                verify_trace!("Verifying assign expression types: {expr}");

                let type_env_typeref = type_env_value.typeref;
                let is_const = &type_env_value.is_const;

                let (value_expr, value_span) = &expr_assign.value;
                let value_typeref = types.resolve_expr_type(value_expr, value_span)?;

                if !is_const && type_env_typeref != value_typeref {
                    verify_trace!("Error: Type mismatching reassigning value ({type_env_typeref:?} vs {value_typeref:?}): {expr}");

                    errs.push((
                        TypeError::MismatchType {
                            expected: type_env_typeref.to_string(),
                            actual: value_typeref.to_string(),
                        }
                        .into(),
                        value_span.clone(),
                    ));
                }
            }
        };

        if !errs.is_empty() {
            return Err(errs);
        }

        Ok(())
    }
}
