use crate::prelude::*;
use egonlang_core::{errors::TypeError, prelude::*};

stmt_rule!(
    TypeMismatchOnDeclarationsRule,
    fn (stmt: &Stmt, span: &Span, types: &mut TypeEnv) {
        let mut errs = vec![];

        if let Stmt::Assign(stmt_assign) = stmt {
            verify_trace!(
                "Verifying assignment statement: {}",
                stmt.to_string().cyan()
            );

            match (&stmt_assign.type_expr, &stmt_assign.value) {
                // let a;
                (None, None) => {
                    verify_trace!(error:
                        "Declaration of unknown type and no initial value: {}",
                        stmt.to_string().cyan()
                    );
                    errs.push((TypeError::UnknownType.into(), span.clone()));
                }
                // let a = 123;
                (None, Some((value_expr, value_span))) => {
                    let value_typeref = types.resolve_expr_type(value_expr, value_span).unwrap();

                    // Check for empty list assignment
                    // Example:
                    // let a = [];
                    if value_typeref == TypeRef::list(TypeRef::unknown()) {
                        verify_trace!(error:
                            "Unknown list type in declaration: {}",
                            stmt.to_string().cyan()
                        );
                        errs.push((TypeError::UknownListType.into(), value_span.clone()));
                    }
                }
                // let a: number;
                (Some((assign_type_expr, assign_type_span)), None) => {
                    let assign_typeref = types
                        .resolve_expr_type(assign_type_expr, assign_type_span)
                        .unwrap();

                    // Check for empty list assignment
                    // Example:
                    // let a: unknown;
                    if assign_typeref == TypeRef::unknown() {
                        verify_trace!(error: "Unknown type in declaration: {stmt}");
                        errs.push((TypeError::UnknownType.into(), assign_type_span.clone()));
                    }
                }
                // let a: number = 123;
                (Some((assign_type_expr, assign_type_span)), Some((value_expr, value_span))) => {
                    let assign_typeref = types
                        .resolve_expr_type(assign_type_expr, assign_type_span)
                        .unwrap();

                    let value_typeref = types.resolve_expr_type(value_expr, value_span).unwrap();

                    // Types mismatched
                    if assign_typeref != value_typeref {
                        // Check for empty list assignment
                        // Example:
                        // let a: list<number> = [];
                        if value_typeref == TypeRef::list(TypeRef::unknown())
                            && assign_typeref.is_known_list()
                        {
                            return vec![];
                        }

                        if value_typeref.0 == *"identifier" {
                            let identifier = &stmt_assign.identifier.name;

                            if let Some(identifier_type) = types.get(identifier) {
                                if assign_typeref == identifier_type.typeref {
                                    return vec![];
                                }
                            }
                        }

                        verify_trace!(error: "Type mismatch in declaration: {stmt}");

                        errs.push((
                            TypeError::MismatchType {
                                expected: assign_typeref.to_string(),
                                actual: value_typeref.to_string(),
                            }
                            .into(),
                            value_span.clone(),
                        ));
                    } else {
                        // Check for empty list assignment
                        // Example:
                        // let a: list<unknown> = [];
                        if value_typeref == TypeRef::list(TypeRef::unknown())
                            && assign_typeref.is_unknown_list()
                        {
                            verify_trace!(error:
                                "Unknown list type in declaration: {}",
                                stmt.to_string().cyan()
                            );
                            errs.push((TypeError::UknownListType.into(), span.clone()));
                        }
                    }
                }
            }
        }

        errs
    }
);

#[cfg(test)]
mod type_mismatch_on_assignment_tests {
    use egonlang_core::{
        ast::{ExprList, ExprLiteral, ExprType, Identifier, StmtAssign},
        errors::TypeError,
        prelude::*,
    };
    use pretty_assertions::assert_eq;

    use crate::{rules::rule::Rule, type_env::TypeEnv};

    use super::TypeMismatchOnDeclarationsRule;

    #[test]
    fn returns_ok_if_assignment_type_and_value_type_match() {
        let rule = TypeMismatchOnDeclarationsRule;

        let mut types = TypeEnv::new();

        let stmt: Stmt = StmtAssign {
            identifier: Identifier {
                name: "a".to_string(),
            },
            type_expr: Some((ExprType(TypeRef::number()).into(), 0..0)),
            is_const: true,
            value: Some((ExprLiteral::Number(123f64).into(), 0..0)),
        }
        .into();

        let span = 0..0;

        assert_eq!(Ok(()), rule.visit_stmt(&stmt, &span, &mut types));
    }

    #[test]
    fn returns_err_if_assignment_type_and_value_type_mismatch() {
        let rule = TypeMismatchOnDeclarationsRule;

        let mut types = TypeEnv::new();

        let stmt: Stmt = StmtAssign {
            identifier: Identifier {
                name: "a".to_string(),
            },
            type_expr: Some((ExprType(TypeRef::unit()).into(), 0..1)),
            is_const: true,
            value: Some((ExprLiteral::Number(123f64).into(), 2..3)),
        }
        .into();

        let span = 0..0;

        assert_eq!(
            Err(vec![(
                TypeError::MismatchType {
                    expected: TypeRef::unit().to_string(),
                    actual: TypeRef::number().to_string()
                }
                .into(),
                2..3
            )]),
            rule.visit_stmt(&stmt, &span, &mut types)
        );
    }

    #[test]
    fn returns_ok_if_empty_list_assigned_to_known_list_type() {
        let rule = TypeMismatchOnDeclarationsRule;

        let mut types = TypeEnv::new();

        let stmt: Stmt = StmtAssign {
            identifier: Identifier {
                name: "a".to_string(),
            },
            type_expr: Some((ExprType(TypeRef::list(TypeRef::number())).into(), 0..0)),
            is_const: true,
            value: Some((ExprList { items: vec![] }.into(), 0..0)),
        }
        .into();

        let span = 0..0;

        assert_eq!(Ok(()), rule.visit_stmt(&stmt, &span, &mut types));
    }

    #[test]
    fn returns_err_if_empty_list_assigned_to_unknown_list_type() {
        let rule = TypeMismatchOnDeclarationsRule;

        let mut types = TypeEnv::new();

        let stmt: Stmt = StmtAssign {
            identifier: Identifier {
                name: "a".to_string(),
            },
            type_expr: Some((ExprType(TypeRef::list(TypeRef::unknown())).into(), 0..0)),
            is_const: true,
            value: Some((ExprList { items: vec![] }.into(), 0..0)),
        }
        .into();

        let span = 0..0;

        assert_eq!(
            Err(vec![(TypeError::UknownListType.into(), 0..0)]),
            rule.visit_stmt(&stmt, &span, &mut types)
        );
    }
}
