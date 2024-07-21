use egonlang_core::{errors::SyntaxError, prelude::*};

use crate::prelude::*;

stmt_rule!(
    /// Checks assignment statements initialize consts with a value
    DeclareConstWithoutValue,
    fn (stmt: &Stmt, span: &Span, _types: &mut TypeEnv) {
        let mut errs = vec![];

        if let Stmt::Assign(stmt_assign) = stmt {
            verify_trace!(
                "Verifying const declaration has value: {}",
                stmt.to_string().cyan()
            );

            if stmt_assign.is_const && stmt_assign.value.is_none() {
                verify_trace!(
                    error: "const declaration has no value: {}",
                    stmt.to_string().cyan()
                );

                errs.push((
                    SyntaxError::UninitializedConst {
                        name: stmt_assign.identifier.name.clone(),
                    }
                    .into(),
                    span.clone(),
                ));
            }
        };

        errs
    }
);

#[cfg(test)]
mod declare_const_without_value_tests {
    use egonlang_core::{errors::SyntaxError, prelude::*};
    use pretty_assertions::assert_eq;

    use crate::prelude::*;

    use super::DeclareConstWithoutValueRule;

    #[test]
    fn returns_ok_const_declared_with_value() {
        let stmt: Stmt = "const a = 123;".try_into().unwrap();
        let span = 0..0;
        let mut types = TypeEnv::new();

        assert_eq!(
            Ok(()),
            DeclareConstWithoutValueRule.visit_stmt(&stmt, &span, &mut types)
        );
    }

    #[test]
    fn returns_err_const_declared_without_type_or_value() {
        let stmt: Stmt = "const a;".try_into().unwrap();
        let span = 0..0;
        let mut types = TypeEnv::new();

        assert_eq!(
            Err(vec![(
                SyntaxError::UninitializedConst {
                    name: "a".to_string()
                }
                .into(),
                span.clone()
            )]),
            DeclareConstWithoutValueRule.visit_stmt(&stmt, &span, &mut types)
        );
    }

    #[test]
    fn returns_err_const_declared_without_value() {
        let stmt: Stmt = "const a: number;".try_into().unwrap();
        let span = 0..0;
        let mut types = TypeEnv::new();

        assert_eq!(
            Err(vec![(
                SyntaxError::UninitializedConst {
                    name: "a".to_string()
                }
                .into(),
                span.clone()
            )]),
            DeclareConstWithoutValueRule.visit_stmt(&stmt, &span, &mut types)
        );
    }
}
