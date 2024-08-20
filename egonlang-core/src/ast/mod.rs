pub use expressions::*;
pub use module::*;
use serde::{Deserialize, Serialize};
pub use statements::*;

mod expressions;
mod module;
mod statements;

use std::fmt::{self, Debug, Display, Formatter};

/// Value representing a type
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct TypeRef(pub String, pub Vec<TypeRef>);

impl TypeRef {
    pub fn is_type(&self) -> bool {
        self.0 == *"type"
    }

    pub fn is_list(&self) -> bool {
        self.0 == *"list"
    }

    pub fn is_known_list(&self) -> bool {
        if self.is_list() {
            let f = self.1.first().unwrap();

            TypeRef::unknown() != *f
        } else {
            false
        }
    }

    /// Is this an unknown type?
    pub fn is_unknown(&self) -> bool {
        self.0 == *"unknown"
    }

    /// Is this an unknown list type?
    pub fn is_unknown_list(&self) -> bool {
        !self.is_known_list()
    }

    /// Is this a builtin type?
    ///
    /// - `bool`
    /// - `number`
    /// - `string`
    /// - `list<T>`
    /// - `range`
    /// - `()`
    /// - `tuple<T, ...>`
    /// - `unknown`
    /// - `function<T>`
    pub fn is_builtin(&self) -> bool {
        self.is_bool()
            || self.is_number()
            || self.is_string()
            || self.is_list()
            || self.is_range()
            || self.is_unit()
            || self.is_tuple()
            || self.is_unknown()
            || self.is_function()
            || self.is_identifier()
    }

    /// Is this a bool type?
    pub fn is_bool(&self) -> bool {
        "bool" == self.0
    }

    /// Is this a number type?
    pub fn is_number(&self) -> bool {
        "number" == self.0
    }

    /// Is this a string type?
    pub fn is_string(&self) -> bool {
        "string" == self.0
    }

    /// Is this a tuple type?
    pub fn is_tuple(&self) -> bool {
        "tuple" == self.0
    }

    /// Is this a range type?
    pub fn is_range(&self) -> bool {
        "range" == self.0
    }

    /// Is this a function type?
    pub fn is_function(&self) -> bool {
        "function" == self.0
    }

    /// Is this a unit type?
    pub fn is_unit(&self) -> bool {
        "()" == self.0
    }

    /// Is this an identifier type?
    pub fn is_identifier(&self) -> bool {
        "identifier" == self.0
    }

    /// Create a `string` type instance
    pub fn string() -> TypeRef {
        TypeRef("string".to_string(), vec![])
    }

    /// Create a `number` type instance
    pub fn number() -> TypeRef {
        TypeRef("number".to_string(), vec![])
    }

    /// Create a `bool` type instance
    pub fn bool() -> TypeRef {
        TypeRef("bool".to_string(), vec![])
    }

    /// Create a `()` type instance
    pub fn unit() -> TypeRef {
        TypeRef("()".to_string(), vec![])
    }

    /// Create a `range` type instance
    pub fn range() -> TypeRef {
        TypeRef("range".to_string(), vec![])
    }

    /// Create a `list<T>` type instance
    pub fn list(item_type: TypeRef) -> TypeRef {
        TypeRef("list".to_string(), vec![item_type])
    }

    /// Create a `list<unknown>` type instance
    pub fn unknown_list() -> TypeRef {
        TypeRef::list(TypeRef::unknown())
    }

    /// Create a `tuple<T, U...>` type instance
    pub fn tuple(item_types: Vec<TypeRef>) -> TypeRef {
        TypeRef("tuple".to_string(), item_types)
    }

    /// Create an `identifier` type instance
    pub fn identifier() -> TypeRef {
        TypeRef("identifier".to_string(), vec![])
    }

    /// Create an `unknown` type instance
    pub fn unknown() -> TypeRef {
        TypeRef("unknown".to_string(), vec![])
    }

    /// Create a `function` type instance
    pub fn function(params_types: Vec<TypeRef>, return_type: TypeRef) -> TypeRef {
        TypeRef(
            "function".to_string(),
            vec![TypeRef::tuple(params_types), return_type],
        )
    }

    /// Create a `type` type instance
    pub fn typed(value: TypeRef) -> TypeRef {
        TypeRef("type".to_string(), vec![value])
    }
}

impl Display for TypeRef {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let base = if self.1.is_empty() {
            self.0.clone()
        } else if self.is_type() {
            self.1.first().unwrap().to_string()
        } else {
            let m = format!(
                "{}<{}>",
                self.0,
                self.1
                    .clone()
                    .into_iter()
                    .map(|typeref| typeref.to_string())
                    .collect::<Vec<String>>()
                    .join(", ")
            );
            m
        };
        f.write_fmt(format_args!("{}", base))
    }
}

#[cfg(test)]
mod is_tests {
    use pretty_assertions::assert_eq;

    use crate::ast::TypeRef;

    macro_rules! typeref_is_type_test {
        ($test_name:ident, $typeref:expr, $($is_fn:ident),+ $(,)?) => {
            $(
                ::paste::paste! {
                    #[test]
                    fn [<$test_name _ $is_fn>]() {
                        let expr: $crate::ast::TypeRef = $typeref;
                        let result: bool = $crate::ast::TypeRef::$is_fn(&expr);

                        assert_eq!(true, result);
                    }
                }
            )+

            ::paste::paste! {
                #[test]
                fn [<$test_name _ is_typed>]() {
                    let expr: $crate::ast::TypeRef =TypeRef::typed($typeref);
                    let result: bool = $crate::ast::TypeRef::is_type(&expr);

                    assert_eq!(true, result);
                }
            }
        };
    }

    macro_rules! typeref_is_not_type_test {
        ($test_name:ident, $typeref:expr, $($is_fn:ident),+ $(,)?) => {
            $(
                ::paste::paste! {
                    #[test]
                    fn [<$test_name _not_ $is_fn>]() {
                        let expr: $crate::ast::TypeRef = $typeref;
                        let result: bool = $crate::ast::TypeRef::$is_fn(&expr);

                        assert_eq!(false, result);
                    }
                }
            )+
        };
    }

    typeref_is_type_test!(test_unit_typeref, TypeRef::unit(), is_unit, is_builtin);

    typeref_is_not_type_test!(test_unknown_typeref, TypeRef::unknown(), is_unit);

    typeref_is_type_test!(
        test_unknown_typeref_is_builtin,
        TypeRef::unknown(),
        is_unknown,
        is_builtin
    );

    typeref_is_type_test!(
        test_number_typeref_is_number,
        TypeRef::number(),
        is_number,
        is_builtin
    );

    typeref_is_type_test!(
        test_number_typeref,
        TypeRef::string(),
        is_string,
        is_builtin
    );

    typeref_is_type_test!(
        test_unknown_list_typeref,
        TypeRef::unknown_list(),
        is_list,
        is_unknown_list
    );

    typeref_is_not_type_test!(
        test_unknown_list_typeref,
        TypeRef::unknown_list(),
        is_known_list
    );

    typeref_is_type_test!(
        test_number_list_typeref,
        TypeRef::list(TypeRef::number()),
        is_list,
        is_known_list,
        is_builtin
    );

    typeref_is_not_type_test!(
        test_number_list_typeref,
        TypeRef::list(TypeRef::number()),
        is_unknown_list,
    );

    typeref_is_not_type_test!(
        test_typeof_number_typeref,
        TypeRef::typed(TypeRef::number()),
        is_number
    );

    typeref_is_type_test!(test_bool_typeref, TypeRef::bool(), is_bool, is_builtin);

    typeref_is_type_test!(
        test_identifier_typeref,
        TypeRef::identifier(),
        is_identifier,
        is_builtin
    );

    typeref_is_type_test!(test_range_typeref, TypeRef::range(), is_range, is_builtin);

    typeref_is_type_test!(
        test_tuple_typeref,
        TypeRef::tuple(vec![TypeRef::string(), TypeRef::number()]),
        is_tuple,
        is_builtin
    );

    typeref_is_type_test!(
        test_empty_tuple_typeref,
        TypeRef::tuple(vec![]),
        is_tuple,
        is_builtin
    );

    typeref_is_type_test!(
        test_function_typeref_with_no_params_or_return_value,
        TypeRef::function(vec![], TypeRef::unit()),
        is_function,
        is_builtin
    );

    typeref_is_type_test!(
        test_function_typeref_with_one_params_but_no_return_value,
        TypeRef::function(vec![TypeRef::number()], TypeRef::unit()),
        is_function,
        is_builtin
    );

    typeref_is_type_test!(
        test_function_typeref_with_multiple_params_but_no_return_value,
        TypeRef::function(
            vec![TypeRef::number(), TypeRef::string(), TypeRef::bool()],
            TypeRef::unit()
        ),
        is_function,
        is_builtin
    );

    typeref_is_type_test!(
        test_function_typeref_with_multiple_params_and_return_value,
        TypeRef::function(
            vec![TypeRef::number(), TypeRef::number()],
            TypeRef::number()
        ),
        is_function,
        is_builtin
    );

    typeref_is_type_test!(
        test_function_typeref_curried,
        TypeRef::function(
            vec![TypeRef::number()],
            TypeRef::function(vec![TypeRef::number()], TypeRef::number())
        ),
        is_function,
        is_builtin
    );
}
