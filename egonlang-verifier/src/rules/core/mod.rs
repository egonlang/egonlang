//! Core syntax rules for the egon language

mod assert_type;
mod const_declaration_with_no_value;
mod divide_by_zero;
mod invalid_type_alias_name;
mod no_return_outside_blocks;
mod reassigning_const_values;
mod type_mismatch_fn_return_expr;
mod type_mismatch_if_cond_expr;
mod type_mismatch_if_then_else_exprs;
mod type_mismatch_infix;
mod type_mismatch_list_items;
mod type_mismatch_on_declarations;
mod type_mismatch_prefix;
mod type_mismatch_reassigning_let_values;
mod undefined_identifier;

pub use assert_type::AssertTypeRule;
pub use const_declaration_with_no_value::DeclareConstWithoutValueRule;
pub use divide_by_zero::DivideByZeroRule;
pub use invalid_type_alias_name::InvalidTypeAliasNameRule;
pub use no_return_outside_blocks::NoReturnOutsideBlockRule;
pub use reassigning_const_values::ReassigningConstValueRule;
pub use type_mismatch_fn_return_expr::TypeMismatchFnReturnExprRule;
pub use type_mismatch_if_cond_expr::TypeMismatchIfCondExprRule;
pub use type_mismatch_if_then_else_exprs::TypeMismatchIfthenElseExprRule;
pub use type_mismatch_infix::TypeMismatchInfixRule;
pub use type_mismatch_list_items::TypeMisMatchListItemsRule;
pub use type_mismatch_on_declarations::TypeMismatchOnDeclarationsRule;
pub use type_mismatch_prefix::TypeMismatchPrefixRule;
pub use type_mismatch_reassigning_let_values::TypeMismatchReassigningLetValuesRule;
pub use undefined_identifier::ReferencingUndefinedIdentifierRule;
