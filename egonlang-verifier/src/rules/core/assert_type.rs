use crate::prelude::*;
use egonlang_core::ast::Stmt;
use egonlang_errors::{EgonErrorS, EgonTypeError};

stmt_rule!(AssertType, |stmt, span, _resolve_ident, resolve_expr| {
    let mut errs: Vec<EgonErrorS> = vec![];

    if let Stmt::AssertType(x) = stmt {
        if let Ok(value_type) = resolve_expr(&x.value.0, &x.value.1) {
            if let Ok(expected_type_type) = resolve_expr(&x.expected_type.0, &x.expected_type.1) {
                if value_type != expected_type_type {
                    errs.push((
                        EgonTypeError::MismatchType {
                            expected: expected_type_type.to_string(),
                            actual: value_type.to_string(),
                        }
                        .into(),
                        span.clone(),
                    ));
                }
            }
        }
    }

    errs
});
