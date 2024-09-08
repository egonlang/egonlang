use crate::prelude::*;
use egonlang_core::prelude::*;
use egonlang_errors::EgonTypeError;
use egonlang_types::Type;

stmt_rule!(
    /// Checks the value type of an assignment declaration matches the declaration type
    ///
    /// ```egon
    /// const a: number = "foo"; // TypeError
    /// let b: string = false; // TypeError
    /// let c: bool = true;
    /// ```
    TypeMismatchOnDeclarations,
    | stmt, span, resolve_ident, resolve_expr | {
        let mut errs = vec![];

        if let ast::Stmt::Assign(stmt_assign) = stmt {
            match (&stmt_assign.type_expr, &stmt_assign.value) {
                // let a;
                (None, None) => {
                    errs.push((EgonTypeError::UnknownType.into(), span.clone()));
                }
                // let a = 123;
                (None, Some((value_expr, value_span))) => {
                    if let Ok(value_typeref) = resolve_expr(value_expr, value_span) {
                        // Check for empty list assignment
                        // Example:
                        // let a = [];
                        if value_typeref.of_type == Type::list(Type::unknown()) {
                            errs.push((EgonTypeError::UknownListType.into(), value_span.clone()));
                        }
                    }

                }
                // let a: number;
                (Some((assign_type_expr, assign_type_span)), None) => {
                    match resolve_expr(assign_type_expr, assign_type_span) {
                        Ok(assign_type) => {
                            // Check for empty list assignment
                            // Example:
                            // let a: unknown;
                            if assign_type.of_type.is_unknown() {
                                errs.push((
                                    EgonTypeError::UnknownType.into(),
                                    assign_type_span.clone()
                                ));
                            }
                        },
                        Err(e) => {
                            errs.extend(e)
                        },
                    };

                }
                // let a: number = 123;
                (Some((assign_type_expr, assign_type_span)), Some((value_expr, value_span))) => {
                    if let Ok(assign_typeref) = resolve_expr(assign_type_expr, assign_type_span) {
                        let assign_typeref = resolve_ident(&assign_typeref.of_type.name(), span)
                            .unwrap_or(assign_typeref.clone());

                        // Check for empty list assignment
                        // Example:
                        // let a: unknown;
                        if assign_typeref.of_type.is_unknown() {
                            errs.push((
                                EgonTypeError::UnknownType.into(),
                                assign_type_span.clone()
                            ));
                        } else if let Ok(value_typeref) = resolve_expr(value_expr, value_span) {
                            // Types mismatched
                            if assign_typeref.of_type != value_typeref.of_type {
                                // Check for empty list assignment
                                // Example:
                                // let a: list<number> = [];
                                if value_typeref.of_type == Type::list(Type::unknown())
                                    && assign_typeref.of_type.is_known_list()
                                {
                                    return vec![];
                                }

                                if value_typeref.of_type.is_identifier() {
                                    let identifier = &stmt_assign.identifier.0.name;

                                    if let Ok(identifier_type) = resolve_ident(identifier,  &stmt_assign.identifier.1) {
                                        if assign_typeref.of_type == identifier_type.of_type {
                                            return vec![];
                                        }
                                    }
                                }

                                errs.push((
                                    EgonTypeError::MismatchType {
                                        expected: assign_typeref.of_type.to_string(),
                                        actual: value_typeref.of_type.to_string(),
                                    }
                                    .into(),
                                    value_span.clone(),
                                ));
                            } else {
                                // Check for empty list assignment
                                // Example:
                                // let a: list<unknown> = [];
                                if value_typeref.of_type == Type::list(Type::unknown())
                                    && assign_typeref.of_type.is_unknown_list()
                                {
                                    errs.push((EgonTypeError::UknownListType.into(), span.clone()));
                                }
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
    use egonlang_errors::EgonTypeError;
    use egonlang_types::Type;

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
                expected: Type::unit().to_string(),
                actual: Type::number().to_string()
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
