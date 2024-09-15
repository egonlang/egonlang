use std::sync::Arc;

use egonlang_errors::EgonResultMultiSpannedErr;

/// Rule for verifying statements and expressions
pub trait Rule<'a>: std::fmt::Display {
    fn visit_stmt(
        &self,
        stmt: &::egonlang_core::ast::Stmt,
        span: &::span::Span,
        resolve_ident: &::egonlang_types::type_env::TypeEnv,
        expr_types: &crate::verifier::VerifierExprTypeCache,
    ) -> EgonResultMultiSpannedErr<()>;

    fn visit_expr(
        &self,
        expr: Arc<::egonlang_core::ast::Expr>,
        span: &::span::Span,
        resolve_ident: &::egonlang_types::type_env::TypeEnv,
        expr_types: &crate::verifier::VerifierExprTypeCache,
    ) -> EgonResultMultiSpannedErr<()>;
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
                    _resolve_ident: &::egonlang_types::type_env::TypeEnv,
                    _expr_types: &$crate::verifier::VerifierExprTypeCache,
                ) -> ::egonlang_errors::EgonResultMultiSpannedErr<()> {
                    Ok(())
                }

                fn visit_expr(
                    &self,
                    expr: ::std::sync::Arc<::egonlang_core::ast::Expr>,
                    span: &::span::Span,
                    _resolve_ident: &::egonlang_types::type_env::TypeEnv,
                    _expr_types: &$crate::verifier::VerifierExprTypeCache,
                ) -> ::egonlang_errors::EgonResultMultiSpannedErr<()> {
                    let internal = |$expr: ::egonlang_core::ast::ExprS | ->
                        Vec<::egonlang_errors::EgonErrorS> {
                            $body
                        };

                    let errs = internal((expr, span.clone()));

                    if !errs.is_empty() {
                        return Err(errs);
                    }

                    Ok(())
                }
            }

            impl ::std::fmt::Display for [<$name Rule>] {
                fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> Result<(), ::std::fmt::Error> {
                    f.write_fmt(format_args!(
                        "{}",
                        stringify!([<$name Rule>])
                    ))
                 }
            }
        }
    };

    ($(#[$attributes:meta])* $name:ident, |$expr:ident, $resolve_ident:ident, $resolve_expr:ident| $body:expr) => {
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
                    _resolve_ident: &::egonlang_types::type_env::TypeEnv,
                    _expr_types: &$crate::verifier::VerifierExprTypeCache,
                ) -> ::egonlang_errors::EgonResultMultiSpannedErr<()> {
                    Ok(())
                }

                fn visit_expr(
                    &self,
                    expr: ::std::sync::Arc<::egonlang_core::ast::Expr>,
                    span: &::span::Span,
                    resolve_ident: &::egonlang_types::type_env::TypeEnv,
                    expr_types: &$crate::verifier::VerifierExprTypeCache,
                ) -> ::egonlang_errors::EgonResultMultiSpannedErr<()> {
                    let internal = | $expr: ::egonlang_core::ast::ExprS,
                                     $resolve_ident: &::egonlang_types::type_env::TypeEnv,
                                     $resolve_expr: &$crate::verifier::VerifierExprTypeCache |
                        { $body };

                    let errs = internal((expr, span.clone()), resolve_ident, expr_types);

                    if !errs.is_empty() {
                        return Err(errs);
                    }

                    Ok(())
                }
            }

            impl ::std::fmt::Display for [<$name Rule>] {
                fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> Result<(), ::std::fmt::Error> {
                    f.write_fmt(format_args!(
                        "{}",
                        stringify!([<$name Rule>])
                    ))
                 }
            }
        }
    };
}

/// Create a verifier [`Rule`] for a statement
#[macro_export]
macro_rules! stmt_rule {
    ($(#[$attributes:meta])* $name:ident, |$stmt:ident, $resolve_ident:ident, $resolve_expr:ident| $body:expr) => {
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
                    resolve_ident: &::egonlang_types::type_env::TypeEnv,
                    expr_types: &$crate::verifier::VerifierExprTypeCache,
                ) -> ::egonlang_errors::EgonResultMultiSpannedErr<()> {
                    let internal = | $stmt: &::egonlang_core::ast::StmtS,
                                     $resolve_ident: &::egonlang_types::type_env::TypeEnv,
                                     $resolve_expr: &$crate::verifier::VerifierExprTypeCache |
                        { $body };

                    let errs = internal(&(stmt.clone(), span.clone()), resolve_ident, expr_types);

                    if !errs.is_empty() {
                        return Err(errs);
                    }

                    Ok(())
                }

                fn visit_expr(
                    &self,
                    _expr: ::std::sync::Arc<::egonlang_core::ast::Expr>,
                    _span: &::span::Span,
                    _resolve_ident: &::egonlang_types::type_env::TypeEnv,
                    _expr_types: &$crate::verifier::VerifierExprTypeCache,
                ) -> ::egonlang_errors::EgonResultMultiSpannedErr<()> {
                    Ok(())
                }
            }

            impl ::std::fmt::Display for [<$name Rule>] {
                fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> Result<(), ::std::fmt::Error> {
                    f.write_fmt(format_args!(
                        "{}",
                        stringify!([<$name Rule>])
                    ))
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
