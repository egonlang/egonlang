pub mod type_env;

use std::fmt::{self, Display, Formatter};

use serde::{Deserialize, Serialize};
use type_env::TypeEnvValue;

/// Type in the Egon language
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Type(pub String, pub Vec<Type>);

impl From<Type> for TypeEnvValue {
    fn from(value: Type) -> Self {
        TypeEnvValue {
            of_type: value,
            is_const: true,
        }
    }
}

impl Type {
    pub fn type_name(&self) -> &str {
        &self.0
    }

    pub fn type_args(&self) -> &Vec<Type> {
        &self.1
    }

    /// Is this type a type?
    pub fn is_type(&self) -> bool {
        self.type_name() == "type"
    }

    /// Is this type a list?
    pub fn is_list(&self) -> bool {
        self.type_name() == "list"
    }

    /// Is this type a list with a known value type?
    pub fn is_known_list(&self) -> bool {
        if self.is_list() {
            Type::unknown() != *self.type_args().first().unwrap()
        } else {
            false
        }
    }

    /// Is this type unknown?
    pub fn is_unknown(&self) -> bool {
        self.type_name() == "unknown"
    }

    /// Is this type a list with a unknown value type?
    pub fn is_unknown_list(&self) -> bool {
        !self.is_known_list()
    }

    /// Is this type a builtin type?
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

    /// Is this type a bool type?
    pub fn is_bool(&self) -> bool {
        "bool" == self.type_name()
    }

    /// Is this type a number type?
    pub fn is_number(&self) -> bool {
        "number" == self.type_name()
    }

    /// Is this type a string type?
    pub fn is_string(&self) -> bool {
        "string" == self.type_name()
    }

    /// Is this type a tuple type?
    pub fn is_tuple(&self) -> bool {
        "tuple" == self.type_name()
    }

    /// Is this type a range type?
    pub fn is_range(&self) -> bool {
        "range" == self.type_name()
    }

    /// Is this type a function type?
    pub fn is_function(&self) -> bool {
        "function" == self.type_name()
    }

    pub fn get_function_return(&self) -> Type {
        if !self.is_function() {
            panic!("Type {} is not a function", self);
        }

        self.type_args().get(1).cloned().unwrap_or(Type::unit())
    }

    /// Is this type the unit type?
    pub fn is_unit(&self) -> bool {
        "()" == self.type_name()
    }

    /// Is this type an identifier type?
    pub fn is_identifier(&self) -> bool {
        "identifier" == self.type_name()
    }

    /// Create a `string` type instance
    pub fn string() -> Type {
        Type("string".to_string(), vec![])
    }

    /// Create a `number` type instance
    pub fn number() -> Type {
        Type("number".to_string(), vec![])
    }

    /// Create a `bool` type instance
    pub fn bool() -> Type {
        Type("bool".to_string(), vec![])
    }

    /// Create a `()` type instance
    pub fn unit() -> Type {
        Type("()".to_string(), vec![])
    }

    /// Create a `range` type instance
    pub fn range() -> Type {
        Type("range".to_string(), vec![])
    }

    /// Create a `list<T>` type instance
    pub fn list(item_type: Type) -> Type {
        Type("list".to_string(), vec![item_type])
    }

    /// Create a `list<unknown>` type instance
    pub fn unknown_list() -> Type {
        Type::list(Type::unknown())
    }

    /// Create a `tuple<T, U...>` type instance
    pub fn tuple(item_types: Vec<Type>) -> Type {
        Type("tuple".to_string(), item_types)
    }

    /// Create an `identifier` type instance
    pub fn identifier() -> Type {
        Type("identifier".to_string(), vec![])
    }

    /// Create an `unknown` type instance
    pub fn unknown() -> Type {
        Type("unknown".to_string(), vec![])
    }

    /// Create a `function` type instance
    pub fn function(params_types: Type, return_type: Type) -> Type {
        Type("function".to_string(), vec![params_types, return_type])
    }

    /// Create a `type` type instance
    pub fn typed(value: Type) -> Type {
        Type("type".to_string(), vec![value])
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let base = if self.type_args().is_empty() {
            self.type_name().to_string()
        } else if self.is_type() {
            self.type_args().clone().first().unwrap().to_string()
        } else {
            format!(
                "{}<{}>",
                self.type_name(),
                self.type_args()
                    .iter()
                    .map(|typeref| typeref.to_string())
                    .collect::<Vec<String>>()
                    .join(", ")
            )
        };

        f.write_fmt(format_args!("{}", base))
    }
}

#[macro_export]
macro_rules! egon_unit {
    () => {
        $crate::Type::unit()
    };
}

#[macro_export]
macro_rules! egon_unknown {
    () => {
        $crate::Type::unknown()
    };
}

#[macro_export]
macro_rules! egon_string {
    () => {
        $crate::Type::string()
    };
}

#[macro_export]
macro_rules! egon_bool {
    () => {
        $crate::Type::bool()
    };
}

#[macro_export]
macro_rules! egon_range {
    () => {
        $crate::Type::range()
    };
}

#[macro_export]
macro_rules! egon_identifier {
    () => {
        $crate::Type::identifier()
    };
}

#[macro_export]
macro_rules! egon_number {
    () => {
        $crate::Type::number()
    };
}

#[macro_export]
macro_rules! egon_list {
    ($subtype:expr) => {
        $crate::Type::list($subtype)
    };
}

#[macro_export]
macro_rules! egon_tuple {
    () => {
        $crate::Type::tuple(vec![])
    };
    ($($arg:expr),+) => {
        $crate::Type::tuple(vec![$($arg),+])
    };
}

#[macro_export]
macro_rules! egon_fn {
    () => {
        $crate::egon_fn(vec![]; $crate::egon_unit!())
    };
    ($($arg:expr),+) => {
        $crate::Type::function(vec![$($arg),+], $crate::Type::unit())
    };
    ($($arg:expr),+; $return:expr) => {
        $crate::Type::function(egon_tuple!($($arg),+), $return)
    };
}

#[cfg(test)]
mod is_tests {
    use pretty_assertions::assert_eq;

    use crate::Type;

    macro_rules! type_is_test {
        ($test_name:ident, $typeref:expr, $($is_fn:ident),+ $(,)?) => {
            $(
                ::paste::paste! {
                    #[test]
                    fn [<$test_name _ $is_fn>]() {
                        let expr: $crate::Type = $typeref;
                        let result: bool = $crate::Type::$is_fn(&expr);

                        assert_eq!(true, result);
                    }
                }
            )+

            ::paste::paste! {
                #[test]
                fn [<$test_name _ is_typed>]() {
                    let expr: $crate::Type = $crate::Type::typed($typeref);
                    let result: bool = $crate::Type::is_type(&expr);

                    assert_eq!(true, result);
                }
            }
        };
    }

    macro_rules! type_is_not_test {
        ($test_name:ident, $typeref:expr, $($is_fn:ident),+ $(,)?) => {
            $(
                ::paste::paste! {
                    #[test]
                    fn [<$test_name _not_ $is_fn>]() {
                        let expr: $crate::Type = $typeref;
                        let result: bool = $crate::Type::$is_fn(&expr);

                        assert_eq!(false, result);
                    }
                }
            )+
        };
    }

    type_is_test!(test_unit_type_macro, egon_unit!(), is_unit, is_builtin);

    type_is_test!(test_unit_typeref, Type::unit(), is_unit, is_builtin);

    type_is_not_test!(test_unknown_type_macro, egon_unknown!(), is_unit);
    type_is_not_test!(test_unknown_typeref, Type::unknown(), is_unit);

    type_is_test!(
        test_unknown_type_macro_is_builtin,
        egon_unknown!(),
        is_unknown,
        is_builtin
    );

    type_is_test!(
        test_unknown_typeref_is_builtin,
        Type::unknown(),
        is_unknown,
        is_builtin
    );

    type_is_test!(
        test_number_type_macro_is_number,
        egon_number!(),
        is_number,
        is_builtin
    );

    type_is_test!(
        test_number_typeref_is_number,
        Type::number(),
        is_number,
        is_builtin
    );

    type_is_test!(
        test_number_type_macro,
        egon_string!(),
        is_string,
        is_builtin
    );

    type_is_test!(test_number_typeref, Type::string(), is_string, is_builtin);

    type_is_test!(
        test_unknown_list_type_macro,
        egon_list!(egon_unknown!()),
        is_list,
        is_unknown_list
    );

    type_is_test!(
        test_unknown_list_typeref,
        Type::unknown_list(),
        is_list,
        is_unknown_list
    );

    type_is_not_test!(
        test_unknown_list_type_macro,
        egon_list![egon_unknown!()],
        is_known_list
    );

    type_is_not_test!(
        test_unknown_list_typeref,
        Type::unknown_list(),
        is_known_list
    );

    type_is_test!(
        test_number_list_type_macro,
        egon_list!(egon_number!()),
        is_list,
        is_known_list,
        is_builtin
    );

    type_is_test!(
        test_number_list_typeref,
        Type::list(Type::number()),
        is_list,
        is_known_list,
        is_builtin
    );

    type_is_not_test!(
        test_number_list_type_macro,
        egon_list!(egon_number!()),
        is_unknown_list,
    );

    type_is_not_test!(
        test_number_list_typeref,
        Type::list(Type::number()),
        is_unknown_list,
    );

    type_is_not_test!(
        test_typeof_number_type_macro,
        egon_list!(egon_number!()),
        is_number
    );

    type_is_not_test!(
        test_typeof_number_typeref,
        Type::typed(Type::number()),
        is_number
    );

    type_is_test!(test_bool_type_macro, egon_bool!(), is_bool, is_builtin);

    type_is_test!(test_bool_typeref, Type::bool(), is_bool, is_builtin);

    type_is_test!(
        test_identifier_type_macro,
        egon_identifier!(),
        is_identifier,
        is_builtin
    );

    type_is_test!(
        test_identifier_typeref,
        Type::identifier(),
        is_identifier,
        is_builtin
    );

    type_is_test!(test_range_type_macro, egon_range!(), is_range, is_builtin);

    type_is_test!(test_range_typeref, Type::range(), is_range, is_builtin);

    type_is_test!(
        test_tuple_type_macro,
        egon_tuple!(egon_string!(), egon_number!()),
        is_tuple,
        is_builtin
    );

    type_is_test!(
        test_tuple_typeref,
        Type::tuple(vec![Type::string(), Type::number()]),
        is_tuple,
        is_builtin
    );

    // //--------------

    type_is_test!(
        test_empty_tuple_type_macro,
        egon_tuple!(),
        is_tuple,
        is_builtin
    );
    // //--------------

    type_is_test!(
        test_empty_tuple_typeref,
        Type::tuple(vec![]),
        is_tuple,
        is_builtin
    );

    type_is_test!(
        test_function_type_macro_with_no_params_or_return_value,
        egon_fn!(egon_tuple!(); egon_unit!()),
        is_function,
        is_builtin
    );

    type_is_test!(
        test_function_typeref_with_no_params_or_return_value,
        Type::function(Type::tuple(vec![]), Type::unit()),
        is_function,
        is_builtin
    );

    type_is_test!(
        test_function_type_macro_with_one_params_but_no_return_value,
        egon_fn!(egon_tuple!(egon_number!()); egon_unit!()),
        is_function,
        is_builtin
    );

    type_is_test!(
        test_function_typeref_with_one_params_but_no_return_value,
        Type::function(Type::tuple(vec![Type::number()]), Type::unit()),
        is_function,
        is_builtin
    );

    type_is_test!(
        test_function_type_macro_with_multiple_params_but_no_return_value,
        egon_fn!(egon_tuple!(egon_number!(), egon_string!(), egon_bool!()); egon_unit!()),
        is_function,
        is_builtin
    );

    type_is_test!(
        test_function_typeref_with_multiple_params_but_no_return_value,
        Type::function(
            Type::tuple(vec![Type::number(), Type::string(), Type::bool()]),
            Type::unit()
        ),
        is_function,
        is_builtin
    );

    type_is_test!(
        test_function_type_macro_with_multiple_params_and_return_value,
        egon_fn!(egon_tuple!(egon_number!(), egon_number!()); egon_number!()),
        is_function,
        is_builtin
    );

    type_is_test!(
        test_function_typeref_with_multiple_params_and_return_value,
        Type::function(
            Type::tuple(vec![Type::number(), Type::number()]),
            Type::number()
        ),
        is_function,
        is_builtin
    );

    type_is_test!(
        test_function_type_macro_curried,
        egon_fn!(egon_tuple!(egon_number!()); egon_fn!(egon_tuple!(egon_number!()); egon_number!())),
        is_function,
        is_builtin
    );

    type_is_test!(
        test_function_typeref_curried,
        Type::function(
            Type::tuple(vec![Type::number()]),
            Type::function(Type::tuple(vec![Type::number()]), Type::number())
        ),
        is_function,
        is_builtin
    );

    #[test]
    fn test_get_return_type_from_function() {
        let fn_type = Type::function(Type::tuple(vec![Type::number()]), Type::number());

        assert_eq!(Type::number(), fn_type.get_function_return());
    }
}
