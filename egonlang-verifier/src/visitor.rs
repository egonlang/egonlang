use egonlang_core::{
    ast::{Expr, Stmt},
    errors::ErrorS,
    span::Span,
};

use crate::type_env::TypeEnv;

pub trait Visitor<'a> {
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
