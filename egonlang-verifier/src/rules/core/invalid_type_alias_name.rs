use crate::prelude::*;
use egonlang_core::prelude::*;
use regex::Regex;

stmt_rule!(
    /// Checks type aliases are formatted correctly
    ///
    /// ```egon
    /// type ValidTypeAlias = string;
    /// type invalidTypeAlias = string; // SyntaxError∂
    /// ```
    InvalidTypeAliasName,
    | stmt, span, _resolve_ident, _resolve_expr | {
        let mut errs = vec![];

        if let ast::Stmt::TypeAlias(stmt_type_alias) = &stmt {
            let name = &stmt_type_alias.alias.0.name;
            let pattern = Regex::new("^[A-Z][A-Za-z0-9]*$").unwrap();

            if !pattern.is_match(name) {
                errs.push((
                    EgonSyntaxError::InvalidTypeAlias {
                        name: name.to_string(),
                    }
                    .into(),
                    span.clone(),
                ));
            }
        }

        errs
    }
);

#[cfg(test)]
mod tests {
    use super::InvalidTypeAliasNameRule;
    use crate::verifier_rule_test;
    use egonlang_core::prelude::*;

    verifier_rule_test!(
        InvalidTypeAliasNameRule,
        returns_ok_if_alias_capitalized,
        "type Bool = bool;"
    );

    verifier_rule_test!(
        InvalidTypeAliasNameRule,
        returns_ok_if_alias_contains_number,
        "type Bool2 = bool;"
    );

    verifier_rule_test!(
        InvalidTypeAliasNameRule,
        returns_err_if_alias_is_lowercase,
        "type booly = bool;",
        Err(vec![(
            EgonSyntaxError::InvalidTypeAlias {
                name: "booly".to_string()
            }
            .into(),
            0..18
        )])
    );

    verifier_rule_test!(
        InvalidTypeAliasNameRule,
        returns_err_if_alias_contains_underscore,
        "type Bool_Value = bool;",
        Err(vec![(
            EgonSyntaxError::InvalidTypeAlias {
                name: "Bool_Value".to_string()
            }
            .into(),
            0..23
        )])
    );

    verifier_rule_test!(
        InvalidTypeAliasNameRule,
        returns_err_if_alias_starts_with_an_underscore,
        "type _Bool = bool;",
        Err(vec![(
            EgonSyntaxError::InvalidTypeAlias {
                name: "_Bool".to_string()
            }
            .into(),
            0..18
        )])
    );
}
