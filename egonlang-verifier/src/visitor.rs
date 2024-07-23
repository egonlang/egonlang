use crate::type_env::TypeEnv;
use egonlang_core::prelude::*;

pub trait Visitor<'a> {
    fn visit_stmt(
        &self,
        stmt: &ast::Stmt,
        span: &Span,
        types: &mut TypeEnv,
    ) -> Result<(), Vec<EgonErrorS>>;

    fn visit_expr(
        &self,
        expr: &ast::Expr,
        span: &Span,
        types: &mut TypeEnv,
    ) -> Result<(), Vec<EgonErrorS>>;
}
