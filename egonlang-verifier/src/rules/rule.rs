use egonlang_core::{
    ast::{Expr, Stmt},
    span::Span,
};

use crate::{type_env::TypeEnv, verifier::VerificationResult};

pub trait Rule<'a> {
    fn visit_stmt(&self, stmt: &Stmt, span: &Span, types: &mut TypeEnv) -> VerificationResult;

    fn visit_expr(&self, expr: &Expr, span: &Span, types: &mut TypeEnv) -> VerificationResult;
}

#[macro_export]
macro_rules! rule {
    ($name:ident, fn visit_expr $params:tt $body:expr $(,)*) => {
        pub struct $name;

        impl<'a> Rule<'a> for $name {
            fn visit_stmt(
                &self,
                _stmt: &Stmt,
                _span: &Span,
                _types: &mut TypeEnv,
            ) -> VerificationResult {
                Ok(())
            }

            fn visit_expr(
                &self,
                expr: &Expr,
                span: &Span,
                types: &mut TypeEnv,
            ) -> VerificationResult {
                fn internal $params -> Vec<ErrorS> {
                    $body
                }

                let errs = internal(expr, span, types);

                if !errs.is_empty() {
                    return Err(errs);
                }

                Ok(())
            }
        }
    };

    ($name:ident, fn visit_stmt $params:tt $body:expr $(,)*) => {
        pub struct $name;

        impl<'a> Rule<'a> for $name {
            fn visit_stmt(
                &self,
                stmt: &Stmt,
                span: &Span,
                types: &mut TypeEnv,
            ) -> VerificationResult {
                fn internal $params -> Vec<ErrorS> {
                    $body
                }

                let errs = internal(stmt, span, types);

                if !errs.is_empty() {
                    return Err(errs);
                }

                Ok(())
            }

            fn visit_expr(
                &self,
                _expr: &Expr,
                _span: &Span,
                _types: &mut TypeEnv,
            ) -> VerificationResult {
                Ok(())
            }
        }
    };

    ($name:ident, fn visit_stmt $stmt_params:tt $stmt_body:expr, fn visit_expr $expr_params:tt $expr_body:expr $(,)*) => {
        pub struct $name;

        impl<'a> Rule<'a> for $name {
            fn visit_stmt(
                &self,
                stmt: &Stmt,
                span: &Span,
                types: &mut TypeEnv,
            ) -> VerificationResult {
                fn internal $stmt_params -> Vec<ErrorS> {
                    $stmt_body
                }

                internal(stmt, span, types)
            }

            fn visit_expr(
                &self,
                expr: &Expr,
                span: &Span,
                types: &mut TypeEnv,
            ) -> VerificationResult {
                fn internal $expr_params -> Vec<ErrorS> {
                    $expr_body
                }

                internal(expr, span, types)
            }
        }
    };

    ($name:ident, fn visit_expr $expr_params:tt $expr_body:expr, fn visit_stmt $stmt_params:tt $stmt_body:expr $(,)*) => {
        pub struct $name;

        impl<'a> Rule<'a> for $name {
            fn visit_stmt(
                &self,
                stmt: &Stmt,
                span: &Span,
                types: &mut TypeEnv,
            ) -> VerificationResult {
                fn internal $stmt_params -> Vec<ErrorS> {
                    $stmt_body
                }

                internal(self, stmt, span, types)
            }

            fn visit_expr(
                &self,
                expr: &Expr,
                span: &Span,
                types: &mut TypeEnv,
            ) -> VerificationResult {
                fn internal $expr_params -> Vec<ErrorS> {
                    $expr_body
                }

                internal(self, expr, span, types)
            }
        }
    };
}
