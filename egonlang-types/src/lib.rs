pub mod type_env;

use std::fmt::{self, Display, Formatter};

use egonlang_errors::EgonError;
use serde::{Deserialize, Serialize};
use type_env::TypeEnvValue;

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct TypeParam {
    pub name: String,
    pub bound_type: Option<Type>,
}

impl From<&str> for TypeParam {
    fn from(value: &str) -> Self {
        TypeParam {
            name: value.to_string(),
            bound_type: None,
        }
    }
}

impl From<Type> for TypeParam {
    fn from(value: Type) -> Self {
        TypeParam {
            name: "T?".to_string(),
            bound_type: Some(value),
        }
    }
}

/// Type in the Egon language
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Type {
    name: String,
    params: Vec<TypeParam>,
}

impl From<Type> for TypeEnvValue {
    fn from(value: Type) -> Self {
        TypeEnvValue {
            of_type: value,
            is_const: true,
        }
    }
}

impl Type {
    pub fn new(type_name: &str) -> Self {
        Self::new_with_args(type_name, vec![])
    }

    pub fn new_with_args(name: &str, params: Vec<TypeParam>) -> Self {
        Self {
            name: name.to_string(),
            params,
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn params(&self) -> &Vec<TypeParam> {
        &self.params
    }

    pub fn is_bound_type(&self) -> bool {
        self.bound_params().is_ok()
    }

    /// Try to get bound parameters the Type
    ///
    /// If any parameters aren't bound then an Err is returned
    pub fn bound_params(&self) -> Result<Vec<Type>, Vec<EgonError>> {
        let errs: Vec<EgonError> = vec![];

        let types = self
            .params()
            .into_iter()
            .map(|x| x.clone().bound_type.unwrap_or(Type::unknown()))
            .collect();

        if !errs.is_empty() {
            return Err(errs);
        }

        Ok(types)
    }

    pub fn bind_params(&mut self, binds: Vec<Type>) -> Result<(), Vec<EgonError>> {
        if binds.len() != self.params.len() {
            return Err(vec![EgonError::TypeError(
                egonlang_errors::EgonTypeError::WrongNumberOfArgs {
                    expected: self.params.len(),
                    actual: binds.len(),
                },
            )]);
        }

        Ok(())
    }

    /// Is this type a type?
    pub fn is_type(&self) -> bool {
        self.name() == "type"
    }

    /// Is this type a list?
    pub fn is_list(&self) -> bool {
        self.name() == "list"
    }

    /// Is this type a list with a known value type?
    pub fn is_known_list(&self) -> bool {
        if self.is_bound_type() && self.is_list() {
            Type::unknown()
                != self
                    .params()
                    .first()
                    .unwrap()
                    .bound_type
                    .clone()
                    .unwrap_or(Type::unknown())
        } else {
            false
        }
    }

    /// Is this type unknown?
    pub fn is_unknown(&self) -> bool {
        self.name() == "unknown"
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
        "bool" == self.name
    }

    /// Is this type a number type?
    pub fn is_number(&self) -> bool {
        "number" == self.name
    }

    /// Is this type a string type?
    pub fn is_string(&self) -> bool {
        "string" == self.name
    }

    /// Is this type a tuple type?
    pub fn is_tuple(&self) -> bool {
        "tuple" == self.name
    }

    /// Is this type a range type?
    pub fn is_range(&self) -> bool {
        "range" == self.name
    }

    /// Is this type a function type?
    pub fn is_function(&self) -> bool {
        "function" == self.name
    }

    pub fn get_function_params(&self) -> Vec<&TypeParam> {
        if !self.is_function() {
            panic!("Type {} is not a function", self);
        }

        let n_params = self.params().len() - 1;

        self.params.iter().take(n_params).collect()
    }

    pub fn get_function_return(&self) -> &TypeParam {
        if !self.is_function() {
            panic!("Type {} is not a function", self);
        }

        self.params
            .last()
            .expect("Function type params should not be empty")
    }

    /// Is this type the unit type?
    pub fn is_unit(&self) -> bool {
        "()" == self.name()
    }

    /// Is this type an identifier type?
    pub fn is_identifier(&self) -> bool {
        "identifier" == self.name()
    }

    /// Create a `string` type instance
    pub fn string() -> Self {
        Self::new("string")
    }

    /// Create a `number` type instance
    pub fn number() -> Self {
        Self::new("number")
    }

    /// Create a `bool` type instance
    pub fn bool() -> Self {
        Self::new("bool")
    }

    /// Create a `()` type instance
    pub fn unit() -> Self {
        Self::new("()")
    }

    /// Create a `range` type instance
    pub fn range() -> Self {
        Self::new("range")
    }

    /// Create a `list<T>` type instance
    pub fn list(item_type: Type) -> Self {
        Self::new_with_args(
            "list",
            vec![TypeParam {
                name: "T".to_string(),
                bound_type: Some(item_type),
            }],
        )
    }

    /// Create a `list<unknown>` type instance
    pub fn unknown_list() -> Self {
        Self::list(Type::unknown())
    }

    /// Create a `tuple<T0, T1...>` type instance
    pub fn tuple(item_types: Vec<Type>) -> Self {
        let params = item_types
            .iter()
            .enumerate()
            .map(|(i, x)| TypeParam {
                name: format!("T{i}"),
                bound_type: Some(x.clone()),
            })
            .collect();

        Self::new_with_args("tuple", params)
    }

    /// Create a `tuple<T, U>` type instance
    pub fn tuple2(first: Type, second: Type) -> Self {
        Self::tuple(vec![first, second])
    }

    /// Create a `tuple<T, U, V>` type instance
    pub fn tuple3(first: Type, second: Type, third: Type) -> Self {
        Self::tuple(vec![first, second, third])
    }

    /// Create an `identifier` type instance
    pub fn identifier() -> Self {
        Self::new("identifier")
    }

    /// Create an `unknown` type instance
    pub fn unknown() -> Self {
        Self::new("unknown")
    }

    /// Create an unbound `function` type instance
    pub fn unbound_function(params_count: usize) -> Self {
        let mut params: Vec<TypeParam> = vec![];
        // params.extend(params_types);

        // let mut params: Vec<TypeParam> = 0..params_count
        //     .iter()
        //     .enumerate()
        //     .map(|(i, x)| TypeParam {
        //         name: format!("T{i}"),
        //         bound_type: Some(x.clone()),
        //     })
        //     .collect();

        for n in 0..params_count - 1 {
            params.push(TypeParam {
                name: format!("T{n}"),
                bound_type: None,
            })
        }

        params.push(TypeParam {
            name: "R".to_owned(),
            bound_type: None,
        });

        Self::new_with_args("function", params)
    }

    /// Create a bound `function` type instance
    pub fn function(params_types: Vec<Type>, return_type: Type) -> Self {
        let mut params: Vec<Type> = vec![];
        params.extend(params_types);

        let mut params: Vec<TypeParam> = params
            .iter()
            .enumerate()
            .map(|(i, x)| TypeParam {
                name: format!("T{i}"),
                bound_type: Some(x.clone()),
            })
            .collect();

        params.push(TypeParam {
            name: "R".to_owned(),
            bound_type: Some(return_type),
        });

        Self::new_with_args("function", params)
    }

    /// Create a `type` type instance
    pub fn typed(value: Type) -> Self {
        let param = TypeParam {
            name: "T0".to_string(),
            bound_type: Some(value),
        };

        Self::new_with_args("type", vec![param])
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let base = if self.params().is_empty() {
            self.name().to_string()
        } else {
            format!(
                "{}<{}>",
                self.name(),
                self.params()
                    .iter()
                    .map(|param| {
                        param
                            .bound_type
                            .clone()
                            .map(|x| x.to_string())
                            .unwrap_or(param.name.clone())
                    })
                    .collect::<Vec<String>>()
                    .join(", ")
            )
        };

        f.write_fmt(format_args!("{}", base))
    }
}

#[cfg(test)]
mod is_tests {
    use pretty_assertions::assert_eq;

    use crate::{Type, TypeParam};

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

    type_is_test!(test_unit_type_macro, Type::unit(), is_unit, is_builtin);

    type_is_test!(test_unit_typeref, Type::unit(), is_unit, is_builtin);

    type_is_not_test!(test_unknown_type_macro, Type::unknown(), is_unit);
    type_is_not_test!(test_unknown_typeref, Type::unknown(), is_unit);

    type_is_test!(
        test_unknown_type_macro_is_builtin,
        Type::unknown(),
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
        Type::number(),
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
        Type::string(),
        is_string,
        is_builtin
    );

    type_is_test!(test_number_typeref, Type::string(), is_string, is_builtin);

    type_is_test!(
        test_unknown_list_type_macro,
        Type::list(Type::unknown()),
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
        Type::list(Type::unknown()),
        is_known_list
    );

    type_is_not_test!(
        test_unknown_list_typeref,
        Type::unknown_list(),
        is_known_list
    );

    type_is_test!(
        test_number_list_type_macro,
        Type::list(Type::number()),
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
        Type::list(Type::number()),
        is_unknown_list,
    );

    type_is_not_test!(
        test_number_list_typeref,
        Type::list(Type::number()),
        is_unknown_list,
    );

    type_is_not_test!(
        test_typeof_number_type_macro,
        Type::list(Type::number()),
        is_number
    );

    type_is_not_test!(
        test_typeof_number_typeref,
        Type::typed(Type::number()),
        is_number
    );

    type_is_test!(test_bool_type_macro, Type::bool(), is_bool, is_builtin);

    type_is_test!(test_bool_typeref, Type::bool(), is_bool, is_builtin);

    type_is_test!(
        test_identifier_type_macro,
        Type::identifier(),
        is_identifier,
        is_builtin
    );

    type_is_test!(
        test_identifier_typeref,
        Type::identifier(),
        is_identifier,
        is_builtin
    );

    type_is_test!(test_range_type_macro, Type::range(), is_range, is_builtin);

    type_is_test!(test_range_typeref, Type::range(), is_range, is_builtin);

    type_is_test!(
        test_tuple_type_macro,
        Type::tuple2(Type::string(), Type::number()),
        is_tuple,
        is_builtin
    );

    type_is_test!(
        test_tuple_typeref,
        Type::tuple2(Type::string(), Type::number()),
        is_tuple,
        is_builtin
    );

    // //--------------

    type_is_test!(
        test_empty_tuple_type_macro,
        Type::tuple(vec![]),
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
        test_function_typeref_with_no_params_or_return_value,
        Type::function(vec![], Type::unit()),
        is_function,
        is_builtin
    );

    type_is_test!(
        test_function_typeref_with_one_params_but_no_return_value,
        Type::function(vec![Type::number()], Type::unit()),
        is_function,
        is_builtin
    );

    type_is_test!(
        test_function_typeref_with_multiple_params_but_no_return_value,
        Type::function(
            vec![Type::number(), Type::string(), Type::bool()],
            Type::unit()
        ),
        is_function,
        is_builtin
    );

    type_is_test!(
        test_function_typeref_with_multiple_params_and_return_value,
        Type::function(vec![Type::number(), Type::number()], Type::number()),
        is_function,
        is_builtin
    );

    type_is_test!(
        test_function_typeref_curried,
        Type::function(
            vec![Type::number()],
            Type::function(vec![Type::number()], Type::number())
        ),
        is_function,
        is_builtin
    );

    #[test]
    fn test_get_param_from_function() {
        let fn_type = Type::function(vec![Type::number(), Type::number()], Type::number());

        assert_eq!(
            vec![
                &TypeParam {
                    name: "T0".to_string(),
                    bound_type: Some(Type::number())
                },
                &TypeParam {
                    name: "T1".to_string(),
                    bound_type: Some(Type::number())
                }
            ],
            fn_type.get_function_params()
        );
    }

    #[test]
    fn test_get_return_type_from_function() {
        let fn_type = Type::function(vec![Type::number()], Type::number());

        assert_eq!(
            &TypeParam {
                name: "R".to_string(),
                bound_type: Some(Type::number())
            },
            fn_type.get_function_return()
        );
    }

    #[test]
    fn test_get_params_from_unbound_function() {
        let fn_type = Type::unbound_function(2);

        assert_eq!(
            vec![
                &TypeParam {
                    name: "T0".to_string(),
                    bound_type: None
                },
                &TypeParam {
                    name: "T1".to_string(),
                    bound_type: None
                }
            ],
            fn_type.get_function_params()
        );
    }

    #[test]
    fn test_get_return_type_from_unbound_function() {
        let fn_type = Type::unbound_function(2);

        assert_eq!(
            &TypeParam {
                name: "R".to_string(),
                bound_type: None
            },
            fn_type.get_function_return()
        );
    }
}
