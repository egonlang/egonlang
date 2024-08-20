use crate::prelude::*;
use egonlang_core::{
    ast::Stmt,
    errors::{EgonErrorS, EgonTypeError},
};

stmt_rule!(AssertType, |stmt, span, _resolve_ident, resolve_expr| {
    let mut errs: Vec<EgonErrorS> = vec![];

    if let Stmt::AssertType(x) = stmt {
        let value_type = resolve_expr(&x.value.0, &x.value.1).unwrap();
        let expected_type_type = resolve_expr(&x.expected_type.0, &x.expected_type.1).unwrap();

        if value_type.typeref != expected_type_type.typeref {
            errs.push((
                EgonTypeError::MismatchType {
                    expected: expected_type_type.typeref.to_string(),
                    actual: value_type.typeref.to_string(),
                }
                .into(),
                span.clone(),
            ));
        }
    }

    errs
});
