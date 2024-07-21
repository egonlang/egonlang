use egonlang_core::{errors::SyntaxError, prelude::*};
use regex::Regex;

use crate::prelude::*;

stmt_rule!(
    /// Checks type aliases are formatted correctly
    ///
    /// ```egon
    /// type ValidTypeAlias = string;
    /// type invalidTypeAlias = string; // SyntaxError∂
    /// ```
    InvalidTypeAliasName,
    fn (stmt: &Stmt, span: &Span, types: &mut TypeEnv) {
        let mut errs = vec![];

        if let Stmt::Assign(assign_stmt) = &stmt {
            if let Some((value_expr, value_span)) = &assign_stmt.value {
                let value_typeref = types.resolve_expr_type(value_expr, value_span).unwrap();

                if value_typeref.is_type() {
                    let name = &assign_stmt.identifier.name;
                    let pattern = Regex::new("^[A-Z][A-Za-z0-9]*$").unwrap();

                    if !pattern.is_match(name) {
                        errs.push((
                            SyntaxError::InvalidTypeAlias {
                                name: name.to_string(),
                            }
                            .into(),
                            span.clone(),
                        ));
                    }
                }
            }
        }

        errs
    }
);

#[cfg(test)]
mod invalid_type_alias_name_tests {
    use pretty_assertions::assert_eq;

    use crate::prelude::*;
    use egonlang_core::{errors, prelude::*};

    use super::InvalidTypeAliasNameRule;

    #[test]
    fn returns_ok_if_alias_capitalized() {
        let stmt: Stmt = "type Bool = bool;".try_into().unwrap();
        let span = 0..0;
        let mut types = TypeEnv::new();

        assert_eq!(
            Ok(()),
            InvalidTypeAliasNameRule.visit_stmt(&stmt, &span, &mut types)
        );
    }

    #[test]
    fn returns_ok_if_alias_contains_number() {
        let stmt: Stmt = "type Bool2 = bool;".try_into().unwrap();
        let span = 0..0;
        let mut types = TypeEnv::new();

        assert_eq!(
            Ok(()),
            InvalidTypeAliasNameRule.visit_stmt(&stmt, &span, &mut types)
        );
    }

    #[test]
    fn returns_err_if_alias_is_lowercase() {
        let stmt: Stmt = "type booly = bool;".try_into().unwrap();
        let span = 0..0;
        let mut types = TypeEnv::new();

        assert_eq!(
            Err(vec![(
                errors::SyntaxError::InvalidTypeAlias {
                    name: "booly".to_string()
                }
                .into(),
                0..0
            )]),
            InvalidTypeAliasNameRule.visit_stmt(&stmt, &span, &mut types)
        );
    }

    #[test]
    fn returns_err_if_alias_contains_underscore() {
        let stmt: Stmt = "type Bool_Value = bool;".try_into().unwrap();
        let span = 0..0;
        let mut types = TypeEnv::new();

        assert_eq!(
            Err(vec![(
                errors::SyntaxError::InvalidTypeAlias {
                    name: "Bool_Value".to_string()
                }
                .into(),
                0..0
            )]),
            InvalidTypeAliasNameRule.visit_stmt(&stmt, &span, &mut types)
        );
    }

    #[test]
    fn returns_err_if_alias_starts_with_an_underscore() {
        let stmt: Stmt = "type _Bool = bool;".try_into().unwrap();
        let span = 0..0;
        let mut types = TypeEnv::new();

        assert_eq!(
            Err(vec![(
                errors::SyntaxError::InvalidTypeAlias {
                    name: "_Bool".to_string()
                }
                .into(),
                0..0
            )]),
            InvalidTypeAliasNameRule.visit_stmt(&stmt, &span, &mut types)
        );
    }

    #[test]
    fn returns_err_if_alias_duplicates_primitive_type() {
        let stmt: Stmt = "type bool = bool;".try_into().unwrap();
        let span = 0..0;
        let mut types = TypeEnv::new();

        assert_eq!(
            Err(vec![(
                errors::SyntaxError::InvalidTypeAlias {
                    name: "bool".to_string()
                }
                .into(),
                0..0
            )]),
            InvalidTypeAliasNameRule.visit_stmt(&stmt, &span, &mut types)
        );
    }
}
