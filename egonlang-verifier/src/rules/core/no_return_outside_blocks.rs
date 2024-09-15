use crate::prelude::*;
use egonlang_core::prelude::*;
// use egonlang_errors::EgonSyntaxError;

stmt_rule!(
    NoReturnOutsideBlock,
    |stmt, _span, _resolve_ident, _resolve_expr| {
        let errs = vec![];

        if let ast::Stmt::Return(_stmt_return) = stmt {
            // if !stmt_return.get_used_in_block() {
            //     errs.push((
            //         EgonSyntaxError::ReturnedUsedOutsideBlock.into(),
            //         span.clone(),
            //     ));
            // }
        }

        errs
    }
);
