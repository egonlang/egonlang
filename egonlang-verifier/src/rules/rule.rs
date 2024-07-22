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
        paste::paste! {
            /// Egon Verifier Expression Rule
            ///
            $(#[$attributes])*
            pub struct [<$name Rule>];


            impl<'a> $crate::rules::rule::Rule<'a> for [<$name Rule>] {
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
        }
    };
}

/// Create a verifier [`Rule`] for a statement
#[macro_export]
macro_rules! stmt_rule {
    ($(#[$attributes:meta])* $name:ident, fn $params:tt $body:expr) => {
        paste::paste! {
            /// Egon Verifier Statement Rule
            ///
            $(#[$attributes])*
            pub struct [<$name Rule>];

            impl<'a> Rule<'a> for [<$name Rule>] {
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
        verifier_rule_test!($(#[$attributes])* $rule, $name, $input, Ok(()));
    };

    ($(#[$attributes:meta])* $rule:ident, $name:ident, $input:expr, $expected:expr) => {
        #[test]
        $(#[$attributes])*
        fn $name() {
            let module = ::egonlang_core::parser::parse($input, 0)
                .expect("Unable to parse source to module");

            let mut verifier = $crate::verifier::Verifier::new();

            verifier.add_rule($rule);

            let result = verifier.verify(&module);

            ::pretty_assertions::assert_eq!($expected, result);
        }
    };
}
