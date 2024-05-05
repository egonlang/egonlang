use egonlang_core::ast::TypeRef;
use egonlang_core::{ast::Stmt, errors::TypeError, span::Span};

use crate::{type_env::TypeEnv, verifier::VerificationResult};

use crate::rules::rule::Rule;

pub struct TypeMismatchOnDeclarationsRule;
impl<'a> Rule<'a> for TypeMismatchOnDeclarationsRule {
    fn visit_stmt(&self, stmt: &Stmt, span: &Span, types: &mut TypeEnv) -> VerificationResult {
        if let Stmt::Assign(stmt_assign) = stmt {
            match (&stmt_assign.type_expr, &stmt_assign.value) {
                // let a;
                (None, None) => {
                    return Err(vec![(TypeError::UnknownType.into(), span.clone())]);
                }
                // let a = 123;
                (None, Some((value_expr, value_span))) => {
                    let value_typeref = types.resolve_expr_type(value_expr, value_span)?;

                    // Check for empty list assignment
                    // Example:
                    // let a = [];
                    if value_typeref == TypeRef::list(TypeRef::unknown()) {
                        return Err(vec![(TypeError::UknownListType.into(), value_span.clone())]);
                    }
                }
                // let a: number;
                (Some(_), None) => {}
                // let a: number = 123;
                (Some((assign_type_expr, assign_type_span)), Some((value_expr, value_span))) => {
                    let assign_typeref =
                        types.resolve_expr_type(assign_type_expr, assign_type_span)?;

                    let value_typeref = types.resolve_expr_type(value_expr, value_span)?;

                    // Types mismatched
                    if assign_typeref != value_typeref {
                        // Check for empty list assignment
                        // Example:
                        // let a: list<number> = [];
                        if value_typeref == TypeRef::list(TypeRef::unknown()) {
                            if assign_typeref.is_known_list() {
                                return Ok(());
                            }
                        }

                        if value_typeref.0 == *"identifier" {
                            let identifier = &stmt_assign.identifier.name;

                            if let Some(identifier_type) = types.get(identifier) {
                                if assign_typeref == identifier_type.typeref {
                                    return Ok(());
                                }
                            }
                        }

                        return Err(vec![(
                            TypeError::MismatchType {
                                expected: assign_typeref.to_string(),
                                actual: value_typeref.to_string(),
                            }
                            .into(),
                            value_span.clone(),
                        )]);
                    } else {
                        // Check for empty list assignment
                        // Example:
                        // let a: list<unknown> = [];
                        if value_typeref == TypeRef::list(TypeRef::unknown()) {
                            if assign_typeref.is_unknown_list() {
                                return Err(vec![(
                                    TypeError::UknownListType.into(),
                                    value_span.clone(),
                                )]);
                            }
                        }
                    }
                }
            }
        }

        Ok(())
    }

    fn visit_expr(
        &self,
        expr: &egonlang_core::ast::Expr,
        _span: &Span,
        _types: &mut TypeEnv,
    ) -> VerificationResult {
        let _ = expr;
        Ok(())
    }
}

#[cfg(test)]
mod type_mismatch_on_assignment_tests {
    use egonlang_core::{
        ast::{ExprList, ExprLiteral, ExprType, Identifier, Stmt, StmtAssign, TypeRef},
        errors::TypeError,
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
