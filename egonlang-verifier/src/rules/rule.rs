/// Rule for verifying statements and expressions
pub trait Rule<'a> {
    fn visit_stmt(
        &self,
        stmt: &::egonlang_core::ast::Stmt,
        span: &::egonlang_core::span::Span,
        types: &mut crate::TypeEnv,
    ) -> crate::VerificationResult;

    fn visit_expr(
        &self,
        expr: &::egonlang_core::ast::Expr,
        span: &::egonlang_core::span::Span,
        types: &mut crate::TypeEnv,
    ) -> crate::VerificationResult;
}

/// Create a verifier [`Rule`] for an expression
#[macro_export]
macro_rules! expr_rule {
    ($(#[$attributes:meta])* $name:ident, fn $params:tt $body:expr) => {
        $(#[$attributes])*
        pub struct $name;

        impl<'a> $crate::rules::rule::Rule<'a> for $name {
            fn visit_stmt(
                &self,
                _stmt: &::egonlang_core::ast::Stmt,
                _span: &::egonlang_core::span::Span,
                _types: &mut $crate::TypeEnv,
            ) -> VerificationResult {
                Ok(())
            }

            fn visit_expr(
                &self,
                expr: &::egonlang_core::ast::Expr,
                span: &::egonlang_core::span::Span,
                types: &mut TypeEnv,
            ) -> VerificationResult {
                fn internal $params -> Vec<::egonlang_core::errors::ErrorS> {
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
}

/// Create a verifier [`Rule`] for a statement
#[macro_export]
macro_rules! stmt_rule {
    ($(#[$attributes:meta])* $name:ident, fn $params:tt $body:expr) => {
        $(#[$attributes])*
        pub struct $name;

        impl<'a> Rule<'a> for $name {
            fn visit_stmt(
                &self,
                stmt: &::egonlang_core::ast::Stmt,
                span: &::egonlang_core::span::Span,
                types: &mut $crate::TypeEnv,
            ) -> VerificationResult {
                fn internal $params -> Vec<::egonlang_core::errors::ErrorS> {
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
                _expr: &::egonlang_core::ast::Expr,
                _span: &::egonlang_core::span::Span,
                _types: &mut TypeEnv,
            ) -> VerificationResult {
                Ok(())
            }
        }
    };
}
