use egonlang_core::{
    ast::{Expr, Stmt},
    errors::ErrorS,
    span::Span,
};

use crate::type_env::TypeEnv;

pub mod type_mismatch_negate_prefix;
pub mod type_mismatch_on_assignment;
pub mod undefined_identifier;

pub trait Rule<'a> {
    fn visit_stmt(
        &self,
        stmt: &Stmt,
        span: &Span,
        types: &mut TypeEnv<'a>,
    ) -> Result<(), Vec<ErrorS>>;

    fn visit_expr(
        &self,
        expr: &Expr,
        span: &Span,
        types: &mut TypeEnv<'a>,
    ) -> Result<(), Vec<ErrorS>>;
}
