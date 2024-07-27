use crate::prelude::*;
use egonlang_core::prelude::*;

stmt_rule!(
    /// Checks the value type of an assignment declaration matches the declaration type
    ///
    /// ```egon
    /// const a: number = "foo"; // TypeError
    /// let b: string = false; // TypeError
    /// let c: bool = true;
    /// ```
    TypeMismatchOnDeclarations,
    fn (stmt: &ast::Stmt, span: &Span, types: &mut TypeEnv) {
        let mut errs = vec![];

        if let ast::Stmt::Assign(stmt_assign) = stmt {
            verify_trace!(
                verifier rule:
                "Verifying assignment statement: {}",
                stmt.to_string().cyan()
            );

            match (&stmt_assign.type_expr, &stmt_assign.value) {
                // let a;
                (None, None) => {
                    verify_trace!(
                        verifier rule error:
                        "Declaration of unknown type and no initial value: {}",
                        stmt.to_string().cyan()
                    );
                    errs.push((EgonTypeError::UnknownType.into(), span.clone()));
                }
                // let a = 123;
                (None, Some((value_expr, value_span))) => {
                    if let Ok(value_typeref) = types.resolve_expr_type(value_expr, value_span) {
                        // Check for empty list assignment
                        // Example:
                        // let a = [];
                        if value_typeref == ast::TypeRef::list(ast::TypeRef::unknown()) {
                            verify_trace!(
                                verifier rule error:
                                "Unknown list type in declaration: {}",
                                stmt.to_string().cyan()
                            );
                            errs.push((EgonTypeError::UknownListType.into(), value_span.clone()));
                        }
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
                    if assign_typeref == ast::TypeRef::unknown() {
                        verify_trace!(
                            verifier rule error: "Unknown type in declaration: {stmt}");
                        errs.push((EgonTypeError::UnknownType.into(), assign_type_span.clone()));
                    }
                }
                // let a: number = 123;
                (Some((assign_type_expr, assign_type_span)), Some((value_expr, value_span))) => {
                    let assign_typeref = types
                        .resolve_expr_type(assign_type_expr, assign_type_span)
                        .unwrap();

                    if let Ok(value_typeref) = types.resolve_expr_type(value_expr, value_span) {
                        // Types mismatched
                        if assign_typeref != value_typeref {
                            // Check for empty list assignment
                            // Example:
                            // let a: list<number> = [];
                            if value_typeref == ast::TypeRef::list(ast::TypeRef::unknown())
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

                            verify_trace!(verifier rule error: "Type mismatch in declaration: {stmt}");

                            errs.push((
                                EgonTypeError::MismatchType {
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
                            if value_typeref == ast::TypeRef::list(ast::TypeRef::unknown())
                                && assign_typeref.is_unknown_list()
                            {
                                verify_trace!(
                                    verifier rule error:
                                    "Unknown list type in declaration: {}",
                                    stmt.to_string().cyan()
                                );
                                errs.push((EgonTypeError::UknownListType.into(), span.clone()));
                            }
                        }
                    }

                }
            }
        }

        errs
    }
);

#[cfg(test)]
mod tests {
    use super::TypeMismatchOnDeclarationsRule;
    use crate::verifier_rule_test;
    use egonlang_core::prelude::*;

    verifier_rule_test! {
        TypeMismatchOnDeclarationsRule,
        returns_ok_if_assignment_type_and_value_type_match,
        "const a: number = 123;"
    }

    verifier_rule_test! {
        TypeMismatchOnDeclarationsRule,
        returns_err_if_assignment_type_and_value_type_mismatch,
        "const a: () = 123;",
        Err(vec![(
            EgonTypeError::MismatchType {
                expected: ast::TypeRef::unit().to_string(),
                actual: ast::TypeRef::number().to_string()
            }
            .into(),
            14..17
        )])
    }

    verifier_rule_test! {
        TypeMismatchOnDeclarationsRule,
        returns_ok_if_empty_list_assigned_to_known_list_type,
        "const a: list<number> = [];"
    }

    verifier_rule_test! {
        TypeMismatchOnDeclarationsRule,
        returns_err_if_empty_list_assigned_to_unknown_list_type,
        "const a: list<unknown> = [];",
        Err(vec![(EgonTypeError::UknownListType.into(), 0..28)])
    }
}
