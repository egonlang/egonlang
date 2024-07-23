use egonlang_core::prelude::*;

use crate::prelude::*;

expr_rule!(
    /// Check that let variables can't be assigned with a different type
    ///
    /// ```egon
    /// let a = 123;
    /// a = 456;
    /// a = "foo"; // TypeError
    ///
    /// let b: string;
    /// b = 123; // TypeError
    /// b = "bar";
    ///
    /// let c: bool = false;
    /// c = 123; // TypeRrror
    /// c = true;
    /// ```
    TypeMismatchReassigningLetValues,
    fn (expr: &ast::Expr, _span: &Span, types: &mut TypeEnv) {
        let mut errs = vec![];

        if let ast::Expr::Assign(expr_assign) = expr {
            let identifier = &expr_assign.identifier.name;

            if let Some(type_env_value) = types.get(identifier) {
                verify_trace!(
                    "Verifying assign expression types: {}",
                    expr.to_string().cyan()
                );

                let type_env_typeref = type_env_value.typeref;
                let is_const = &type_env_value.is_const;

                let (value_expr, value_span) = &expr_assign.value;
                let value_typeref = types.resolve_expr_type(value_expr, value_span).unwrap();

                if !is_const && type_env_typeref != value_typeref {
                    verify_trace!(error:
                        "Type mismatching reassigning value ({type_env_typeref:?} vs {value_typeref:?}): {expr}");

                    errs.push((
                        EgonTypeError::MismatchType {
                            expected: type_env_typeref.to_string(),
                            actual: value_typeref.to_string(),
                        }
                        .into(),
                        value_span.clone(),
                    ));
                }
            }
        };

        errs
    }
);

#[cfg(test)]
mod tests {
    use super::TypeMismatchReassigningLetValuesRule;
    use crate::verifier_rule_test;
    use egonlang_core::prelude::*;

    verifier_rule_test! {
        TypeMismatchReassigningLetValuesRule,
        returns_ok_reassigning_with_the_same_type,
        "let a = 123; a = 456;"
    }

    verifier_rule_test! {
        TypeMismatchReassigningLetValuesRule,
        returns_err_reassigning_with_different_type,
        "let a = 123; a = false;",
        Err(vec![(
            EgonTypeError::MismatchType {
                expected: ast::TypeRef::number().to_string(),
                actual: ast::TypeRef::bool().to_string()
            }.into(),
            17..22
        )])
    }

    verifier_rule_test! {
        TypeMismatchReassigningLetValuesRule,
        returns_err_reassigning_with_different_type_2,
        "let a = [1, 2, 3]; a = [false, true];",
        Err(vec![(
            EgonTypeError::MismatchType {
                expected: ast::TypeRef::list(ast::TypeRef::number()).to_string(),
                actual: ast::TypeRef::list(ast::TypeRef::bool()).to_string()
            }.into(),
            23..36
        )])
    }
}
