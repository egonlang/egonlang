use egonlang_core::{errors::TypeError, prelude::*};

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
    fn (expr: &Expr, _span: &Span, types: &mut TypeEnv) {
        let mut errs = vec![];

        if let Expr::Assign(expr_assign) = expr {
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
                        TypeError::MismatchType {
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
