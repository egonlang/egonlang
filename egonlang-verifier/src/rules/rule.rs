pub trait ResolveIdent: Fn(&str) -> Option<crate::TypeEnvValue> {}
pub trait ResolveExpr:
    Fn(&::egonlang_core::ast::Expr, &::span::Span) -> Option<crate::TypeEnvValue>
{
}

impl<F> ResolveIdent for F where F: Fn(&str) -> Option<crate::TypeEnvValue> {}
impl<F> ResolveExpr for F where
    F: Fn(&::egonlang_core::ast::Expr, &::span::Span) -> Option<crate::TypeEnvValue>
{
}

/// Rule for verifying statements and expressions
pub trait Rule<'a> {
    fn visit_stmt(
        &self,
        stmt: &::egonlang_core::ast::Stmt,
        span: &::span::Span,
        resolve_ident: &dyn ResolveIdent,
        resolve_expr: &dyn ResolveExpr,
    ) -> crate::VerificationResult;

    fn visit_expr(
        &self,
        expr: &::egonlang_core::ast::Expr,
        span: &::span::Span,
        resolve_ident: &dyn ResolveIdent,
        resolve_expr: &dyn ResolveExpr,
    ) -> crate::VerificationResult;
}

/// Create a verifier [`Rule`] for an expression
#[macro_export]
macro_rules! expr_rule {
    ($(#[$attributes:meta])* $name:ident, |$expr:ident| $body:expr) => {
        ::paste::paste! {
            /// Egon Verifier Expression Rule
            ///
            $(#[$attributes])*
            pub struct [<$name Rule>];

            impl<'a> $crate::rules::rule::Rule<'a> for [<$name Rule>] {
                fn visit_stmt(
                    &self,
                    _stmt: &::egonlang_core::ast::Stmt,
                    _span: &::span::Span,
                    _resolve_ident: &dyn $crate::rules::rule::ResolveIdent,
                    _resolve_expr: &dyn $crate::rules::rule::ResolveExpr,
                ) -> VerificationResult {
                    Ok(())
                }

                fn visit_expr(
                    &self,
                    expr: &::egonlang_core::ast::Expr,
                    _span: &::span::Span,
                    _resolve_ident: &dyn $crate::rules::rule::ResolveIdent,
                    _resolve_expr: &dyn $crate::rules::rule::ResolveExpr,
                ) -> VerificationResult {
                    let internal = |$expr: &::egonlang_core::ast::Expr| ->
                        Vec<::egonlang_errors::EgonErrorS> {
                            $body
                        };

                    let errs = internal(expr);

                    if !errs.is_empty() {
                        return Err(errs);
                    }

                    Ok(())
                }
            }
        }
    };

    ($(#[$attributes:meta])* $name:ident, |$expr:ident, $span:ident| $body:expr) => {
        ::paste::paste! {
            /// Egon Verifier Expression Rule
            ///
            $(#[$attributes])*
            pub struct [<$name Rule>];

            impl<'a> $crate::rules::rule::Rule<'a> for [<$name Rule>] {
                fn visit_stmt(
                    &self,
                    _stmt: &::egonlang_core::ast::Stmt,
                    _span: &::egonlang_core::span::Span,
                    _resolve_ident: &dyn $crate::rules::rule::ResolveIdent,
                    _resolve_expr: &dyn $crate::rules::rule::ResolveExpr,
                ) -> VerificationResult {
                    Ok(())
                }

                fn visit_expr(
                    &self,
                    expr: &::egonlang_core::ast::Expr,
                    span: &::egonlang_core::span::Span,
                    _resolve_ident: &dyn $crate::rules::rule::ResolveIdent,
                    _resolve_expr: &dyn $crate::rules::rule::ResolveExpr,
                ) -> VerificationResult {
                    let internal = | $expr: &::egonlang_core::ast::Expr,
                                     $span: &::egonlang_core::span::Span |
                        { $body };

                    let errs = internal((expr, span));

                    if !errs.is_empty() {
                        return Err(errs);
                    }

                    Ok(())
                }
            }
        }
    };

    ($(#[$attributes:meta])* $name:ident, |$expr:ident, $span:ident, $resolve_ident:ident, $resolve_expr:ident| $body:expr) => {
        ::paste::paste! {
            /// Egon Verifier Expression Rule
            ///
            $(#[$attributes])*
            pub struct [<$name Rule>];

            impl<'a> $crate::rules::rule::Rule<'a> for [<$name Rule>] {
                fn visit_stmt(
                    &self,
                    _stmt: &::egonlang_core::ast::Stmt,
                    _span: &::span::Span,
                    _resolve_ident: &dyn $crate::rules::rule::ResolveIdent,
                    _resolve_expr: &dyn $crate::rules::rule::ResolveExpr,
                ) -> VerificationResult {
                    Ok(())
                }

                fn visit_expr(
                    &self,
                    expr: &::egonlang_core::ast::Expr,
                    span: &::span::Span,
                    resolve_ident: &dyn $crate::rules::rule::ResolveIdent,
                    resolve_expr: &dyn $crate::rules::rule::ResolveExpr,
                ) -> VerificationResult {
                    let internal = | $expr: &::egonlang_core::ast::Expr,
                                     $span: &::span::Span,
                                     $resolve_ident: &dyn $crate::rules::rule::ResolveIdent,
                                     $resolve_expr: &dyn $crate::rules::rule::ResolveExpr |
                        { $body };

                    let errs = internal(expr, span, resolve_ident, resolve_expr);

                    if !errs.is_empty() {
                        return Err(errs);
                    }

                    Ok(())
                }
            }
        }
    };
}

/// Create a verifier [`Rule`] for a statement
#[macro_export]
macro_rules! stmt_rule {
    ($(#[$attributes:meta])* $name:ident, |$stmt:ident, $span:ident, $resolve_ident:ident, $resolve_expr:ident| $body:expr) => {
        paste::paste! {
            /// Egon Verifier Statement Rule
            ///
            $(#[$attributes])*
            pub struct [<$name Rule>];

            impl<'a> $crate::rules::Rule<'a> for [<$name Rule>] {
                fn visit_stmt(
                    &self,
                    stmt: &::egonlang_core::ast::Stmt,
                    span: &::span::Span,
                    resolve_ident: &dyn $crate::rules::rule::ResolveIdent,
                    resolve_expr: &dyn $crate::rules::rule::ResolveExpr,
                ) -> $crate::VerificationResult {
                    let internal = | $stmt: &::egonlang_core::ast::Stmt,
                                     $span: &::span::Span,
                                     $resolve_ident: &dyn $crate::rules::rule::ResolveIdent,
                                     $resolve_expr: &dyn $crate::rules::rule::ResolveExpr |
                        { $body };

                    let errs = internal(stmt, span, resolve_ident, resolve_expr);

                    if !errs.is_empty() {
                        return Err(errs);
                    }

                    Ok(())
                }

                fn visit_expr(
                    &self,
                    _expr: &::egonlang_core::ast::Expr,
                    _span: &::span::Span,
                    _resolve_ident: &dyn $crate::rules::rule::ResolveIdent,
                    _resolve_expr: &dyn $crate::rules::rule::ResolveExpr,
                ) -> $crate::VerificationResult {
                    Ok(())
                }
            }
        }
    };
}

/// Easily write tests for egon's verifier
///
/// Testing valid code:
///
/// ```ignore
/// verifier_rule_test!(
///   NameOfRule, // Rule under test
///   should_return_err_if, // Test name/description
///   "let a = 123;", // Egon code under test
/// );
/// ```
///
/// Testing invalid code:
///
/// ```ignore
/// verifier_rule_test!(
///   NameOfRule, // Rule under test
///   should_return_err_if, // Test name/description
///   "let a = 123", // Egon code under test,
///   Err(vec![]) // Expected errors
/// );
/// ```
///
/// Attributes can still be used
///
/// ```ignore
/// verifier_rule_test!(
///   #[ignore = "Example test"]
///   NameOfRule, // Rule under test
///   should_return_err_if, // Test name/description
///   "let a = 123", // Egon code under test,
///   Err(vec![]) // Expected errors
/// );
/// ```
#[cfg(test)]
#[macro_export]
macro_rules! verifier_rule_test {
    ($(#[$attributes:meta])* $rule:ident, $name:ident, $input:expr) => {
        $crate::verifier_rule_test!($(#[$attributes])* $rule, $name, $input, Ok(()));
    };

    ($(#[$attributes:meta])* $rule:ident, $name:ident, $input:expr, $expected:expr) => {
        #[test]
        $(#[$attributes])*
        fn $name() {
            let mut module = ::egonlang_core::parser::parse($input, 0)
                .expect("Unable to parse source to module");

            let mut verifier = $crate::verifier::Verifier::new();

            verifier.add_rule($rule);

            let result = verifier.verify(&mut module);

            ::pretty_assertions::assert_eq!($expected, result);
        }
    };
}
