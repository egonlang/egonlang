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
    use egonlang_core::{
        ast::{ExprLiteral, ExprType, Identifier, StmtAssign},
        errors::SyntaxError,
        prelude::*,
    };
    use pretty_assertions::assert_eq;

    use crate::prelude::*;

    use super::DeclareConstWithoutValue;

    #[test]
    fn returns_ok_const_declared_with_value() {
        let rule = DeclareConstWithoutValue;

        let mut types = TypeEnv::new();

        let stmt: Stmt = StmtAssign {
            identifier: Identifier {
                name: "a".to_string(),
            },
            type_expr: None,
            is_const: true,
            value: Some((ExprLiteral::Number(123f64).into(), 0..0)),
        }
        .into();

        let span = 0..0;

        assert_eq!(Ok(()), rule.visit_stmt(&stmt, &span, &mut types));
    }

    #[test]
    fn returns_err_const_declared_without_type_or_value() {
        let rule = DeclareConstWithoutValue;

        let mut types = TypeEnv::new();

        let stmt: Stmt = StmtAssign {
            identifier: Identifier {
                name: "a".to_string(),
            },
            type_expr: None,
            is_const: true,
            value: None,
        }
        .into();

        let span = 0..0;

        assert_eq!(
            Err(vec![(
                SyntaxError::UninitializedConst {
                    name: "a".to_string()
                }
                .into(),
                span.clone()
            )]),
            rule.visit_stmt(&stmt, &span, &mut types)
        );
    }

    #[test]
    fn returns_err_const_declared_without_value() {
        let rule = DeclareConstWithoutValue;

        let mut types = TypeEnv::new();

        let stmt: Stmt = StmtAssign {
            identifier: Identifier {
                name: "a".to_string(),
            },
            type_expr: Some((ExprType(TypeRef::number()).into(), 0..0)),
            is_const: true,
            value: None,
        }
        .into();

        let span = 0..0;

        assert_eq!(
            Err(vec![(
                SyntaxError::UninitializedConst {
                    name: "a".to_string()
                }
                .into(),
                span.clone()
            )]),
            rule.visit_stmt(&stmt, &span, &mut types)
        );
    }
}
