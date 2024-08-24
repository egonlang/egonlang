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
                    if let Some(value_typeref) = resolve_expr(value_expr, value_span) {
                        // Check for empty list assignment
                        // Example:
                        // let a = [];
                        if value_typeref.typeref == ast::TypeRef::list(ast::TypeRef::unknown()) {
                            errs.push((EgonTypeError::UknownListType.into(), value_span.clone()));
                        }
                    }

                }
                // let a: number;
                (Some((assign_type_expr, assign_type_span)), None) => {
                    match resolve_expr(assign_type_expr, assign_type_span) {
                        Some(assign_type) => {
                            // Check for empty list assignment
                            // Example:
                            // let a: unknown;
                            if assign_type.typeref.is_unknown() {
                                errs.push((
                                    EgonTypeError::UnknownType.into(),
                                    assign_type_span.clone()
                                ));
                            }
                        },
                        None => {
                            errs.push((
                                EgonTypeError::Undefined(assign_type_expr.to_string()).into(),
                                assign_type_span.clone()
                            ));
                        },
                    };

                }
                // let a: number = 123;
                (Some((assign_type_expr, assign_type_span)), Some((value_expr, value_span))) => {
                    if let Some(assign_typeref) = resolve_expr(assign_type_expr, assign_type_span) {
                        // Check for empty list assignment
                        // Example:
                        // let a: unknown;
                        if assign_typeref.typeref.is_unknown() {
                            errs.push((
                                EgonTypeError::UnknownType.into(),
                                assign_type_span.clone()
                            ));
                        } else if let Some(value_typeref) = resolve_expr(value_expr, value_span) {
                            // Types mismatched
                            if assign_typeref.typeref != value_typeref.typeref {
                                // Check for empty list assignment
                                // Example:
                                // let a: list<number> = [];
                                if value_typeref.typeref == ast::TypeRef::list(ast::TypeRef::unknown())
                                    && assign_typeref.typeref.is_known_list()
                                {
                                    return vec![];
                                }

                                if value_typeref.typeref.0 == *"identifier" {
                                    let identifier = &stmt_assign.identifier.0.name;

                                    if let Some(identifier_type) = resolve_ident(identifier) {
                                        if assign_typeref.typeref == identifier_type.typeref {
                                            return vec![];
                                        }
                                    }
                                }

                                errs.push((
                                    EgonTypeError::MismatchType {
                                        expected: assign_typeref.typeref.to_string(),
                                        actual: value_typeref.typeref.to_string(),
                                    }
                                    .into(),
                                    value_span.clone(),
                                ));
                            } else {
                                // Check for empty list assignment
                                // Example:
                                // let a: list<unknown> = [];
                                if value_typeref.typeref == ast::TypeRef::list(ast::TypeRef::unknown())
                                    && assign_typeref.typeref.is_unknown_list()
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
