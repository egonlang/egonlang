use crate::prelude::*;
use egonlang_core::prelude::*;

stmt_rule!(
    NoReturnOutsideBlock,
    |stmt, span, _resolve_ident, _resolve_expr| {
        let mut errs = vec![];

        if let ast::Stmt::Return(stmt_return) = stmt {
            if !stmt_return.get_used_in_block() {
                errs.push((
                    EgonSyntaxError::ReturnedUsedOutsideBlock.into(),
                    span.clone(),
                ));
            }
        }

        errs
    }
);
