use crate::prelude::*;
use egonlang_core::prelude::*;

expr_rule!(
    NoStmtsAfterReturnStmt,
    |expr, _span, _resolve_ident, _resolve_expr| {
        let mut errs = vec![];

        if let ast::Expr::Block(expr_block) = expr {
            let mut return_stmt_found = false;

            for (stmt, stmt_span) in &expr_block.stmts {
                if return_stmt_found {
                    verify_trace!("Statement found after return statement was found. ERROR");
                    errs.push((EgonSyntaxError::UnreachableCode.into(), stmt_span.clone()));
                    continue;
                }

                if let ast::Stmt::Return(_) = &stmt {
                    verify_trace!("Return statement found in block expression");
                    return_stmt_found = true;
                }
            }

            if return_stmt_found {
                if let Some((_, return_expr_span)) = &expr_block.return_expr {
                    verify_trace!(
                        "Return expression found after return statement was found. ERROR"
                    );
                    errs.push((
                        EgonSyntaxError::UnreachableCode.into(),
                        return_expr_span.clone(),
                    ));
                }
            }
        }

        errs
    }
);

#[cfg(test)]
mod tests {
    use egonlang_core::errors::EgonSyntaxError;
    use rules::core::NoStmtsAfterReturnStmtRule;

    use crate::{prelude::*, verifier_rule_test};

    verifier_rule_test!(
        NoStmtsAfterReturnStmtRule,
        returns_ok,
        r#"
        {
            123;
            return 456;
        };"#
    );

    verifier_rule_test!(
        NoStmtsAfterReturnStmtRule,
        returns_errs,
        r#"
        {
            return 456;
            123;
        };"#,
        Err(vec![(EgonSyntaxError::UnreachableCode.into(), 47..51)])
    );

    verifier_rule_test!(
        NoStmtsAfterReturnStmtRule,
        returns_errs_b,
        r#"
        {
            return 456;
            123
        };"#,
        Err(vec![(EgonSyntaxError::UnreachableCode.into(), 47..50)])
    );

    verifier_rule_test!(
        NoStmtsAfterReturnStmtRule,
        returns_errs_c,
        r#"
        {
            return 456;
            false;
            123
        };"#,
        Err(vec![
            (EgonSyntaxError::UnreachableCode.into(), 47..53),
            (EgonSyntaxError::UnreachableCode.into(), 66..69)
        ])
    );

    //

    verifier_rule_test!(
        NoStmtsAfterReturnStmtRule,
        returns_errs_d,
        r#"
        (): number => {
            return 456;
            123;
        };"#,
        Err(vec![(EgonSyntaxError::UnreachableCode.into(), 61..65)])
    );

    verifier_rule_test!(
        NoStmtsAfterReturnStmtRule,
        returns_errs_e,
        r#"
        (): number => {
            return 456;
            123
        };"#,
        Err(vec![(EgonSyntaxError::UnreachableCode.into(), 61..64)])
    );

    verifier_rule_test!(
        NoStmtsAfterReturnStmtRule,
        returns_errs_f,
        r#"
        (): number => {
            return 456;
            false;
            123
        };"#,
        Err(vec![
            (EgonSyntaxError::UnreachableCode.into(), 61..67),
            (EgonSyntaxError::UnreachableCode.into(), 80..83)
        ])
    );
}
