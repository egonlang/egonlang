use egonlang_core::{
    ast::{Expr, Stmt},
    span::Span,
};

use crate::{type_env::TypeEnv, verifier::VerificationResult};

pub trait Rule<'a> {
    fn visit_stmt(&self, stmt: &Stmt, span: &Span, types: &mut TypeEnv) -> VerificationResult;

    fn visit_expr(&self, expr: &Expr, span: &Span, types: &mut TypeEnv) -> VerificationResult;
}
