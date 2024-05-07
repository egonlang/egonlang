use egonlang_core::{
    ast::{Expr, Stmt},
    errors::SyntaxError,
    span::Span,
};
use regex::Regex;

use crate::{type_env::TypeEnv, verifier::VerificationResult};

use crate::rules::rule::Rule;

pub struct InvalidTypeAliasNameRule;
impl<'a> Rule<'a> for InvalidTypeAliasNameRule {
    fn visit_stmt(&self, stmt: &Stmt, span: &Span, types: &mut TypeEnv) -> VerificationResult {
        if let Stmt::Assign(assign_stmt) = &stmt {
            if let Some((value_expr, value_span)) = &assign_stmt.value {
                let value_typeref = types.resolve_expr_type(value_expr, value_span)?;

                if value_typeref.is_type() {
                    let name = &assign_stmt.identifier.name;
                    let pattern = Regex::new("^[A-Z][A-Za-z0-9]*$").unwrap();

                    if !pattern.is_match(name) {
                        return Err(vec![(
                            SyntaxError::InvalidTypeAlias {
                                name: name.to_string(),
                            }
                            .into(),
                            span.clone(),
                        )]);
                    }
                }
            }
        }

        Ok(())
    }

    fn visit_expr(&self, _expr: &Expr, _span: &Span, _types: &mut TypeEnv) -> VerificationResult {
        Ok(())
    }
}
