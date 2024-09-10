use egonlang_core::ast::{Expr, Stmt};
use egonlang_errors::EgonResultMultiSpannedErr;
use egonlang_types::{type_env::TypeEnv, Type};
use span::Span;

pub trait ResolveIdent:
    Fn(&str, &::span::Span) -> EgonResultMultiSpannedErr<egonlang_types::Type>
{
}
pub trait ResolveExpr:
    Fn(&::egonlang_core::ast::Expr, &::span::Span) -> EgonResultMultiSpannedErr<egonlang_types::Type>
{
}

impl std::fmt::Debug for dyn ResolveExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "ResolveExpr")
    }
}

impl<F> ResolveIdent for F where
    F: Fn(&str, &::span::Span) -> EgonResultMultiSpannedErr<egonlang_types::Type>
{
}
impl<F> ResolveExpr for F where
    F: Fn(
        &::egonlang_core::ast::Expr,
        &::span::Span,
    ) -> EgonResultMultiSpannedErr<egonlang_types::Type>
{
}

#[derive(Debug, PartialEq)]
pub enum RuleTarget<'target> {
    Stmt(&'target Stmt),
    Expr(&'target Expr),
}

#[derive(Debug)]
pub struct RuleContext<'target> {
    type_env: &'target TypeEnv,
    target: RuleTarget<'target>,
    target_type: Option<&'target Type>,
    span: &'target Span,
}

impl<'target> RuleContext<'target> {
    pub fn new(
        type_env: &'target TypeEnv,
        target: RuleTarget<'target>,
        target_type: Option<&'target Type>,
        span: &'target Span,
    ) -> Self {
        Self {
            type_env,
            target,
            target_type,
            span,
        }
    }
}

impl<'target> RuleContext<'target> {
    pub fn target(&self) -> &RuleTarget<'target> {
        &self.target
    }

    pub fn target_type(&self) -> &Option<&'target Type> {
        &self.target_type
    }

    pub fn span(&self) -> &Span {
        &self.span
    }

    pub fn resolve_identifier(&self, key: &str) -> Option<&egonlang_types::Type> {
        self.type_env
            .get_const(key)
            .or_else(|| self.type_env.get_variable(key))
    }

    pub fn resolve_expr(&self, expr: &Expr, span: &Span) -> Option<egonlang_types::Type> {
        todo!()
    }
}

/// Rule for verifying statements and expressions
pub trait Rule<'a>: std::fmt::Display {
    fn visit_stmt(
        &self,
        stmt: &::egonlang_core::ast::Stmt,
        span: &::span::Span,
        resolve_ident: &dyn ResolveIdent,
        resolve_expr: &dyn ResolveExpr,
    ) -> EgonResultMultiSpannedErr<()>;

    fn visit_expr(
        &self,
        type_env: &::egonlang_types::type_env::TypeEnv,
        expr: &::egonlang_core::ast::Expr,
        span: &::span::Span,
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
                    _resolve_ident: &dyn $crate::rules::rule::ResolveIdent,
                    _resolve_expr: &dyn $crate::rules::rule::ResolveExpr,
                ) -> ::egonlang_errors::EgonResultMultiSpannedErr<()> {
                    Ok(())
                }

                fn visit_expr(
                    &self,
                    type_env: &::egonlang_types::type_env::TypeEnv,
                    expr: &::egonlang_core::ast::Expr,
                    span: &::span::Span,
                ) -> ::egonlang_errors::EgonResultMultiSpannedErr<()> {
                    let context = $crate::rules::rule::RuleContext::new(
                        type_env,
                        $crate::rules::rule::RuleTarget::Expr(expr),
                        None,
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
                ) -> ::egonlang_errors::EgonResultMultiSpannedErr<()> {
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
                    _type_env: &::egonlang_types::type_env::TypeEnv,
                    _expr: &::egonlang_core::ast::Expr,
                    _span: &::span::Span,
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
