use crate::prelude::*;
use egonlang_core::prelude::*;
use egonlang_errors::EgonSyntaxError;
use rules::rule::RuleTarget;

stmt_rule!(NoReturnOutsideBlock, |context| {
    let mut errs = vec![];

    if let RuleTarget::Stmt(ast::Stmt::Return(_stmt_return)) = context.target() {
        if context.scope_depth() == 1 {
            errs.push((
                EgonSyntaxError::ReturnedUsedOutsideBlock.into(),
                context.span().clone(),
            ));
        }
    }

    errs
});

#[cfg(test)]
mod tests {
    use egonlang_errors::EgonSyntaxError;

    use crate::{rules::core::NoReturnOutsideBlockRule, verifier_rule_test};

    verifier_rule_test!(
        NoReturnOutsideBlockRule,
        returns_ok_if_return_stmt_in_block,
        r#"
        {
            return 456;
        };
        
        "#,
        Ok(())
    );

    verifier_rule_test!(
        NoReturnOutsideBlockRule,
        returns_ok_if_return_stmt_in_nested_block,
        r#"
        {
            {
                return 456;
            };
        };
        "#,
        Ok(())
    );

    verifier_rule_test!(
        NoReturnOutsideBlockRule,
        returns_ok_if_return_stmt_after_nested_block,
        r#"
        {
            {};

            return 456;
        };
        "#,
        Ok(())
    );

    verifier_rule_test!(
        NoReturnOutsideBlockRule,
        returns_ok_if_return_stmt_after_nested_block_with_return_stmt,
        r#"
        {
            {
                return 456;
            };

            return 456;
        };
        "#,
        Ok(())
    );

    verifier_rule_test!(
        NoReturnOutsideBlockRule,
        returns_err_if_return_stmt_at_top_level,
        r#"
        {
            123
        };
        
        return 456;
        "#,
        Err(vec![(
            EgonSyntaxError::ReturnedUsedOutsideBlock.into(),
            55..66
        )])
    );

    verifier_rule_test!(
        NoReturnOutsideBlockRule,
        returns_err_if_return_stmt_at_top_level_after_block_with_return_stmt,
        r#"
        {
            return 123;
        };
        
        return 456;
        "#,
        Err(vec![(
            EgonSyntaxError::ReturnedUsedOutsideBlock.into(),
            63..74
        )])
    );
}
