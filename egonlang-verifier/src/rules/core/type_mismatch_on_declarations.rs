use crate::prelude::*;
use egonlang_core::prelude::*;
use egonlang_errors::EgonTypeError;
use rules::rule::RuleTarget;

stmt_rule!(
    /// Checks the value type of an assignment declaration matches the declaration type
    ///
    /// ```egon
    /// const a: number = "foo"; // TypeError
    /// let b: string = false; // TypeError
    /// let c: bool = true;
    /// ```
    TypeMismatchOnDeclarations,
    |context| {
        let mut errs = vec![];

        if let RuleTarget::Stmt(ast::Stmt::Assign(stmt_assign)) = context.target() {
            match (&stmt_assign.type_expr, &stmt_assign.value) {
                // let a;
                (None, None) => {
                    errs.push((EgonTypeError::UnknownType.into(), context.span().clone()));
                }
                // let a = 123;
                (None, Some((value_expr, value_span))) => {
                    if let Some(value_typeref) = context.resolve_expr(value_expr.clone(), value_span) {
                        // Check for empty list assignment
                        // Example:
                        // let a = [];
                        if value_typeref.is_unknown_list() {
                            errs.push((EgonTypeError::UknownListType.into(), value_span.clone()));
                        }
                    }

                }
                // let a: number;
                (Some((assign_type_expr, assign_type_span)), None) => {
                    if let Some(assign_type) = context.resolve_expr(assign_type_expr.clone(), assign_type_span) {
                        // Check for empty list assignment
                        // Example:
                        // let a: unknown;
                        if assign_type.is_unknown() {
                            errs.push((
                                EgonTypeError::UnknownType.into(),
                                assign_type_span.clone()
                            ));
                        }
                    }
                }
                // let a: number = 123;
                (Some((assign_type_expr, assign_type_span)), Some((value_expr, value_span))) => {
                    if let Some(assign_typeref) = context.resolve_expr(assign_type_expr.clone(), assign_type_span) {
                        // Check for empty list assignment
                        // Example:
                        // let a: unknown;
                        if assign_typeref.is_unknown() {
                            errs.push((
                                EgonTypeError::UnknownType.into(),
                                assign_type_span.clone()
                            ));
                        } else if let Some(value_typeref) = context.resolve_expr(value_expr.clone(), value_span) {
                            // Types mismatched
                            if assign_typeref != value_typeref {
                                // Check for empty list assignment
                                // Example:
                                // let a: list<number> = [];
                                if value_typeref.is_unknown_list()
                                    && assign_typeref.is_known_list()
                                {
                                    return vec![];
                                }

                                if value_typeref.is_identifier() {
                                    let identifier = &stmt_assign.identifier.0.name;

                                    if let Some(resolved_identifier) = context.resolve_identifier(identifier) {
                                        if *assign_typeref == resolved_identifier.of_type {
                                            return vec![];
                                        }
                                    }
                                }

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
                                if value_typeref.is_unknown_list()
                                    && assign_typeref.is_unknown_list()
                                {
                                    errs.push((EgonTypeError::UknownListType.into(), context.span().clone()));
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
    use crate::{verifier_rule_test, Verifier};
    use egonlang_errors::EgonTypeError;
    use egonlang_types::Type;

    verifier_rule_test! {
        TypeMismatchOnDeclarationsRule,
        returns_ok_if_assignment_type_and_value_type_match,
        "const a: number = 123;"
    }

    #[test]
    fn returns_ok_2if_assignment_type_and_value_type_match() {
        let mut module = ::egonlang_core::parser::parse("const a: number = 123;", 0)
            .expect("Unable to parse source to module");
        let mut verifier = Verifier::new();
        verifier.add_rule(TypeMismatchOnDeclarationsRule);
        let result = verifier.verify(&mut module);
        ::pretty_assertions::assert_eq!((Ok(())), result);
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
