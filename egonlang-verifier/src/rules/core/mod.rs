//! Core syntax rules for the egon language

mod assert_type;
mod const_declaration_with_no_value;
mod divide_by_zero;
mod invalid_type_alias_name;
mod no_non_callable_called;
mod no_return_outside_blocks;
mod no_stmts_after_return_stmt;
mod reassigning_const_values;
mod type_mismatch_args_in_call_expr;
mod type_mismatch_fn_return_expr;
mod type_mismatch_if_cond_expr;
mod type_mismatch_if_then_else_exprs;
mod type_mismatch_infix;
mod type_mismatch_list_items;
mod type_mismatch_on_declarations;
mod type_mismatch_prefix;
mod type_mismatch_reassigning_values;
mod wrong_number_of_args_calling_fn;

pub use assert_type::AssertTypeRule;
pub use const_declaration_with_no_value::DeclareConstWithoutValueRule;
pub use divide_by_zero::DivideByZeroRule;
pub use invalid_type_alias_name::InvalidTypeAliasNameRule;
pub use no_non_callable_called::NoNonCallableCalledRule;
pub use no_return_outside_blocks::NoReturnOutsideBlockRule;
pub use no_stmts_after_return_stmt::NoStmtsAfterReturnStmtRule;
pub use reassigning_const_values::ReassigningConstValueRule;
pub use type_mismatch_args_in_call_expr::TypeMismatchArgsInCallExprRule;
pub use type_mismatch_fn_return_expr::TypeMismatchFnReturnExprRule;
pub use type_mismatch_if_cond_expr::TypeMismatchIfCondExprRule;
pub use type_mismatch_if_then_else_exprs::TypeMismatchIfthenElseExprRule;
pub use type_mismatch_infix::TypeMismatchInfixRule;
pub use type_mismatch_list_items::TypeMisMatchListItemsRule;
pub use type_mismatch_on_declarations::TypeMismatchOnDeclarationsRule;
pub use type_mismatch_prefix::TypeMismatchPrefixRule;
pub use type_mismatch_reassigning_values::TypeMismatchReassigningValuesRule;
pub use wrong_number_of_args_calling_fn::WrongNumberOfArgsCallingFnRule;
