use std::sync::Arc;

use egonlang_core::ast::{Expr, Stmt};
use egonlang_errors::EgonResultMultiSpannedErr;
use egonlang_types::type_env::{TypeEnv, TypeEnvGetByIdent};
use span::Span;

use crate::verifier::VerifierExprTypeCache;

#[derive(Debug, PartialEq, Clone)]
pub enum RuleTarget<'target> {
    Stmt(&'target Stmt),
    Expr(Arc<Expr>),
}

#[derive(Debug)]
pub struct RuleContext<'target> {
    type_env: &'target TypeEnv,
    expr_types: &'target VerifierExprTypeCache,
    target: RuleTarget<'target>,
    span: &'target Span,
}

impl<'target> RuleContext<'target> {
    pub fn new(
        type_env: &'target TypeEnv,
        expr_types: &'target VerifierExprTypeCache,
        target: RuleTarget<'target>,
        span: &'target Span,
    ) -> Self {
        Self {
            type_env,
            expr_types,
            target,
            span,
        }
    }
}

impl<'target> RuleContext<'target> {
    /// The target value for the rule
    ///
    /// This will be a statement expression
    pub fn target(&self) -> RuleTarget<'target> {
        self.target.clone()
    }

    /// The span of the value for the rule
    pub fn span(&self) -> &Span {
        self.span
    }

    pub fn scope_depth(&self) -> usize {
        self.type_env.get_scope_depth()
    }

    /// Resolve a constant or variable's type from an indentifier
    pub fn resolve_identifier(&self, key: &str) -> Option<TypeEnvGetByIdent> {
        self.type_env.get_by_identifier(key)
    }

    /// Resolve an expression's type
    pub fn resolve_expr(&self, expr: Arc<Expr>, span: &Span) -> Option<&egonlang_types::Type> {
        self.expr_types.get(&(expr.clone(), span.clone()))
    }
}

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
        type_env: &::egonlang_types::type_env::TypeEnv,
        expr: Arc<::egonlang_core::ast::Expr>,
        span: &::span::Span,
        expr_types: &VerifierExprTypeCache,
    ) -> EgonResultMultiSpannedErr<()>;
}

/// Create a verifier [`Rule`] for an expression
#[macro_export]
macro_rules! expr_rule {
    ($(#[$attributes:meta])* $name:ident, |$context:ident| $body:expr) => {
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
                    type_env: &::egonlang_types::type_env::TypeEnv,
                    expr: ::std::sync::Arc<::egonlang_core::ast::Expr>,
                    span: &::span::Span,
                    expr_types: &$crate::verifier::VerifierExprTypeCache,
                ) -> ::egonlang_errors::EgonResultMultiSpannedErr<()> {
                    let context = $crate::rules::rule::RuleContext::new(
                        type_env,
                        expr_types,
                        $crate::rules::rule::RuleTarget::Expr(expr),
                        span,
                    );

                    let internal = |$context: &$crate::rules::rule::RuleContext| ->
                        Vec<::egonlang_errors::EgonErrorS> {
                            $body
                        };

                    let errs = internal(&context);

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
    ($(#[$attributes:meta])* $name:ident, |$context:ident| $body:expr) => {
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
                    type_env: &::egonlang_types::type_env::TypeEnv,
                    expr_types: &$crate::verifier::VerifierExprTypeCache,
                ) -> ::egonlang_errors::EgonResultMultiSpannedErr<()> {
                    let context = $crate::rules::rule::RuleContext::new(
                        type_env,
                        expr_types,
                        $crate::rules::rule::RuleTarget::Stmt(stmt),
                        span,
                    );

                    let internal = |$context: &$crate::rules::rule::RuleContext| ->
                        Vec<::egonlang_errors::EgonErrorS> {
                            $body
                        };

                    let errs = internal(&context);

                    if !errs.is_empty() {
                        return Err(errs);
                    }

                    Ok(())
                }

                fn visit_expr(
                    &self,
                    _type_env: &::egonlang_types::type_env::TypeEnv,
                    _expr: ::std::sync::Arc<::egonlang_core::ast::Expr>,
                    _span: &::span::Span,
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
