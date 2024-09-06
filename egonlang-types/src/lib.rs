pub mod type_env;

use std::{
    fmt::{self, Display, Formatter},
    rc::Rc,
};

use egonlang_errors::EgonResultMultiSpannedErr;
use serde::{Deserialize, Serialize};

pub trait T {
    type Param;

    fn name(&self) -> &str;

    fn params(&self) -> Vec<Self::Param>;

    /// Is this type a type?
    fn is_type(&self) -> bool;

    /// Is this type a list?
    fn is_list(&self) -> bool;

    /// Is this type a list with a known value type?
    fn is_known_list(&self) -> bool;

    /// Is this type unknown?
    fn is_unknown(&self) -> bool;

    /// Is this type a list with a unknown value type?
    fn is_unknown_list(&self) -> bool;

    fn is_builtin(&self) -> bool;

    /// Is this type a bool type?
    fn is_bool(&self) -> bool;

    /// Is this type a number type?
    fn is_number(&self) -> bool;

    /// Is this type a string type?
    fn is_string(&self) -> bool;

    /// Is this type a tuple type?
    fn is_tuple(&self) -> bool;

    /// Is this type a range type?
    fn is_range(&self) -> bool;

    /// Is this type a function type?
    fn is_function(&self) -> bool;

    fn get_function_params(&self) -> Vec<Self::Param>;

    fn get_function_return(&self) -> Self::Param;

    /// Is this type the unit type?
    fn is_unit(&self) -> bool;

    /// Is this type an identifier type?
    fn is_identifier(&self) -> bool;

    fn new(type_name: &str) -> Self;

    fn new_with_args(name: &str, params: Vec<Self::Param>) -> Self;

    /// Create a `string` type instance
    fn string() -> Self;

    /// Create a `number` type instance
    fn number() -> Self;

    /// Create a `bool` type instance
    fn bool() -> Self;

    /// Create a `()` type instance
    fn unit() -> Self;

    /// Create a `range` type instance
    fn range() -> Self;

    /// Create a `list<T>` type instance
    fn list(item_type: Self::Param) -> Self;

    /// Create a `list<unknown>` type instance
    fn unknown_list() -> Self;

    /// Create a `tuple<T, U...>` type instance
    fn tuple(item_types: Vec<Self::Param>) -> Self;

    /// Create a `tuple<T, U>` type instance
    fn tuple2(first: Self::Param, second: Self::Param) -> Self;

    /// Create a `tuple<T, U, V>` type instance
    fn tuple3(first: Self::Param, second: Self::Param, third: Self::Param) -> Self;

    /// Create an `identifier` type instance
    fn identifier() -> Self;

    /// Create an `unknown` type instance
    fn unknown() -> Self;

    /// Create a `function` type instance
    fn function(params_types: Self::Param, return_type: Self::Param) -> Self;

    /// Create a `type` type instance
    fn typed(value: Self::Param) -> Self;
}

/// In memory representation of a type in the Egon language
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize, Eq, Hash)]
pub enum EgonType {
    Unbound(UnboundType),
    Bound(Box<BoundType>),
}

impl EgonType {
    pub fn is_bound(&self) -> bool {
        matches!(self, Self::Bound(_))
    }

    pub fn is_unbound(&self) -> bool {
        matches!(self, Self::Unbound(_))
    }

    pub fn as_unbound(&self) -> EgonResultMultiSpannedErr<&UnboundType> {
        match self {
            Self::Unbound(t) => Ok(t),
            Self::Bound(_) => todo!(),
        }
    }

    pub fn as_bound(&self) -> EgonResultMultiSpannedErr<&BoundType> {
        match self {
            Self::Unbound(_) => todo!(),
            Self::Bound(t) => Ok(t),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum EgonTypeParam {
    Unbound(String),
    Bound(BoundType),
}

impl From<EgonTypeParam> for EgonType {
    fn from(value: EgonTypeParam) -> Self {
        match value {
            EgonTypeParam::Unbound(_) => todo!(),
            EgonTypeParam::Bound(t) => EgonType::Bound(Box::new(t)),
        }
    }
}

impl T for EgonType {
    type Param = EgonTypeParam;

    fn name(&self) -> &str {
        match self {
            Self::Unbound(t) => t.name(),
            Self::Bound(t) => t.name(),
        }
    }

    fn params(&self) -> Vec<Self::Param> {
        let r = match self {
            Self::Unbound(t) => t
                .params()
                .iter()
                .map(|param| EgonTypeParam::Unbound(param.to_string()))
                .collect::<Vec<Self::Param>>(),
            Self::Bound(t) => t
                .params()
                .iter()
                .map(|param| EgonTypeParam::Bound(param.clone()))
                .collect::<Vec<Self::Param>>(),
        };

        r
    }

    fn is_type(&self) -> bool {
        match self {
            Self::Unbound(t) => t.is_type(),
            Self::Bound(t) => t.is_type(),
        }
    }

    fn is_list(&self) -> bool {
        match self {
            Self::Unbound(t) => t.is_list(),
            Self::Bound(t) => t.is_list(),
        }
    }

    fn is_known_list(&self) -> bool {
        match self {
            Self::Unbound(t) => t.is_known_list(),
            Self::Bound(t) => t.is_known_list(),
        }
    }

    fn is_unknown(&self) -> bool {
        match self {
            Self::Unbound(t) => t.is_unknown(),
            Self::Bound(t) => t.is_unknown(),
        }
    }

    fn is_unknown_list(&self) -> bool {
        match self {
            Self::Unbound(t) => t.is_unknown_list(),
            Self::Bound(t) => t.is_unknown_list(),
        }
    }

    fn is_builtin(&self) -> bool {
        match self {
            Self::Unbound(t) => t.is_builtin(),
            Self::Bound(t) => t.is_builtin(),
        }
    }

    fn is_bool(&self) -> bool {
        match self {
            Self::Unbound(t) => t.is_bool(),
            Self::Bound(t) => t.is_bool(),
        }
    }

    fn is_number(&self) -> bool {
        match self {
            Self::Unbound(t) => t.is_number(),
            Self::Bound(t) => t.is_number(),
        }
    }

    fn is_string(&self) -> bool {
        match self {
            Self::Unbound(t) => t.is_string(),
            Self::Bound(t) => t.is_string(),
        }
    }

    fn is_tuple(&self) -> bool {
        match self {
            Self::Unbound(t) => t.is_tuple(),
            Self::Bound(t) => t.is_tuple(),
        }
    }

    fn is_range(&self) -> bool {
        match self {
            Self::Unbound(t) => t.is_range(),
            Self::Bound(t) => t.is_range(),
        }
    }

    fn is_function(&self) -> bool {
        match self {
            Self::Unbound(t) => t.is_function(),
            Self::Bound(t) => t.is_function(),
        }
    }

    fn get_function_params(&self) -> Vec<Self::Param> {
        todo!()
    }

    fn get_function_return(&self) -> Self::Param {
        todo!()
    }

    fn is_unit(&self) -> bool {
        match self {
            Self::Unbound(t) => t.is_unit(),
            Self::Bound(t) => t.is_unit(),
        }
    }

    fn is_identifier(&self) -> bool {
        match self {
            Self::Unbound(t) => t.is_identifier(),
            Self::Bound(t) => t.is_identifier(),
        }
    }

    fn new(type_name: &str) -> Self {
        todo!()
    }

    fn new_with_args(name: &str, params: Vec<Self::Param>) -> Self {
        todo!()
    }

    fn string() -> Self {
        Self::Bound(Box::new(BoundType::string()))
    }

    fn number() -> Self {
        Self::Bound(Box::new(BoundType::number()))
    }

    fn bool() -> Self {
        Self::Bound(Box::new(BoundType::bool()))
    }

    fn unit() -> Self {
        Self::Bound(Box::new(BoundType::unit()))
    }

    fn range() -> Self {
        Self::Bound(Box::new(BoundType::range()))
    }

    fn list(item_type: Self::Param) -> Self {
        match item_type {
            EgonTypeParam::Unbound(param) => Self::Unbound(UnboundType::list(param)),
            EgonTypeParam::Bound(param) => Self::Bound(Box::new(BoundType::list(param))),
        }
    }

    fn unknown_list() -> Self {
        Self::Bound(Box::new(BoundType::unknown_list()))
    }

    fn tuple(item_types: Vec<Self::Param>) -> Self {
        todo!()
    }

    fn tuple2(first: Self::Param, second: Self::Param) -> Self {
        todo!()
    }

    fn tuple3(first: Self::Param, second: Self::Param, third: Self::Param) -> Self {
        todo!()
    }

    fn identifier() -> Self {
        Self::Bound(Box::new(BoundType::identifier()))
    }

    fn unknown() -> Self {
        Self::Bound(Box::new(BoundType::unknown()))
    }

    fn function(params_types: Self::Param, return_type: Self::Param) -> Self {
        todo!()
    }

    fn typed(value: Self::Param) -> Self {
        todo!()
    }
}

impl Display for EgonType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Unbound(t) => f.write_fmt(format_args!("{}", t)),
            Self::Bound(t) => f.write_fmt(format_args!("{}", t)),
        }
    }
}

/// A type with unbound parameters
///
/// Example: `list<T>`
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize, Eq, Hash)]
pub struct UnboundType {
    name: String,
    params: Vec<String>,
}

impl T for UnboundType {
    type Param = String;

    fn name(&self) -> &str {
        &self.name
    }

    fn params(&self) -> Vec<Self::Param> {
        self.params.clone()
    }

    fn is_type(&self) -> bool {
        self.name() == "type"
    }

    fn is_list(&self) -> bool {
        self.name() == "list"
    }

    fn is_known_list(&self) -> bool {
        self.is_list() && self.params().first().unwrap_or(&Self::unknown().name) != "unknown"
    }

    fn is_unknown(&self) -> bool {
        self.name() == "unknown"
    }

    fn is_unknown_list(&self) -> bool {
        self.is_list() && self.params().first().unwrap_or(&Self::unknown().name) != "unknown"
    }

    fn is_builtin(&self) -> bool {
        todo!()
    }

    fn is_bool(&self) -> bool {
        self.name() == "bool"
    }

    fn is_number(&self) -> bool {
        self.name() == "number"
    }

    fn is_string(&self) -> bool {
        self.name() == "string"
    }

    fn is_tuple(&self) -> bool {
        self.name() == "tuple"
    }

    fn is_range(&self) -> bool {
        self.name() == "range"
    }

    fn is_function(&self) -> bool {
        self.name() == "function"
    }

    fn get_function_params(&self) -> Vec<Self::Param> {
        let fn_params: Vec<Self::Param> = self
            .params()
            .iter()
            .take(self.params().len() - 1)
            .cloned()
            .collect();

        fn_params
    }

    fn get_function_return(&self) -> Self::Param {
        let params = self.params();
        let fn_return = params.get(1).unwrap();

        fn_return.clone()
    }

    fn is_unit(&self) -> bool {
        self.name() == "()"
    }

    fn is_identifier(&self) -> bool {
        self.name() == "identifier"
    }

    fn new(type_name: &str) -> Self {
        Self::new_with_args(type_name, vec![])
    }

    fn new_with_args(name: &str, params: Vec<Self::Param>) -> Self {
        Self {
            name: name.to_owned(),
            params,
        }
    }

    fn string() -> Self {
        Self::new("string")
    }

    fn number() -> Self {
        Self::new("number")
    }

    fn bool() -> Self {
        Self::new("bool")
    }

    fn unit() -> Self {
        Self::new("()")
    }

    fn range() -> Self {
        Self::new("range")
    }

    fn list(item_type: Self::Param) -> Self {
        Self::new_with_args("list", vec![item_type])
    }

    fn unknown_list() -> Self {
        Self::list(Self::unknown().to_string())
    }

    fn tuple(item_types: Vec<Self::Param>) -> Self {
        Self::new_with_args("tuple", item_types)
    }

    fn tuple2(first: Self::Param, second: Self::Param) -> Self {
        Self::tuple(vec![first, second])
    }

    fn tuple3(first: Self::Param, second: Self::Param, third: Self::Param) -> Self {
        Self::tuple(vec![first, second, third])
    }

    fn identifier() -> Self {
        Self::new("identifier")
    }

    fn unknown() -> Self {
        Self::new("unknown")
    }

    fn function(params_types: Self::Param, return_type: Self::Param) -> Self {
        Self::new_with_args("function", vec![params_types, return_type])
    }

    fn typed(value: Self::Param) -> Self {
        Self::new_with_args("type", vec![value])
    }
}

impl From<UnboundType> for EgonType {
    fn from(value: UnboundType) -> Self {
        Self::Unbound(value)
    }
}

impl Display for UnboundType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let base = if self.params.is_empty() {
            self.name.to_string()
        } else if self.is_type() {
            self.params.clone().first().unwrap().to_string()
        } else {
            format!(
                "{}<{}>",
                self.name,
                self.params
                    .iter()
                    .map(|typeref| typeref.to_string())
                    .collect::<Vec<String>>()
                    .join(", ")
            )
        };

        f.write_fmt(format_args!("{}", base))
    }
}

/// A type with all it's parameters bound to a bound type
///
/// Example: `list<list<string>>`
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize, Eq, Hash)]
pub struct BoundType {
    name: String,
    params: Vec<BoundType>,
    unbound: Option<Rc<UnboundType>>,
}

impl T for BoundType {
    type Param = Self;

    fn name(&self) -> &str {
        &self.name
    }

    fn params(&self) -> Vec<Self::Param> {
        self.params.clone()
    }

    fn is_type(&self) -> bool {
        self.name() == "type"
    }

    fn is_list(&self) -> bool {
        self.name() == "list"
    }

    fn is_known_list(&self) -> bool {
        self.is_list()
            && !self
                .params()
                .first()
                .unwrap_or(&Self::unknown())
                .is_unknown()
    }

    fn is_unknown(&self) -> bool {
        self.name() == "unknown"
    }

    fn is_unknown_list(&self) -> bool {
        self.is_list()
            && self
                .params()
                .first()
                .unwrap_or(&Self::unknown())
                .is_unknown()
    }

    fn is_builtin(&self) -> bool {
        todo!()
    }

    fn is_bool(&self) -> bool {
        self.name() == "bool"
    }

    fn is_number(&self) -> bool {
        self.name() == "number"
    }

    fn is_string(&self) -> bool {
        self.name() == "string"
    }

    fn is_tuple(&self) -> bool {
        self.name() == "tuple"
    }

    fn is_range(&self) -> bool {
        self.name() == "range"
    }

    fn is_function(&self) -> bool {
        self.name() == "function"
    }

    fn get_function_params(&self) -> Vec<Self::Param> {
        let fn_params = self.params().first().unwrap().params();

        fn_params.clone()
    }

    fn get_function_return(&self) -> Self::Param {
        let binding = self.params();
        let fn_return = binding.get(1).unwrap();

        fn_return.clone()
    }

    fn is_unit(&self) -> bool {
        self.name() == "()"
    }

    fn is_identifier(&self) -> bool {
        self.name() == "identifier"
    }

    fn new(type_name: &str) -> Self {
        Self::new_with_args(type_name, vec![])
    }

    fn new_with_args(name: &str, params: Vec<Self::Param>) -> Self {
        Self {
            name: name.to_owned(),
            params,
            unbound: None,
        }
    }

    fn string() -> Self {
        Self::new("string")
    }

    fn number() -> Self {
        Self::new("number")
    }

    fn bool() -> Self {
        Self::new("bool")
    }

    fn unit() -> Self {
        Self::new("()")
    }

    fn range() -> Self {
        Self::new("range")
    }

    fn list(item_type: Self::Param) -> Self {
        Self::new_with_args("list", vec![item_type])
    }

    fn unknown_list() -> Self {
        Self::list(Self::unknown())
    }

    fn tuple(item_types: Vec<Self::Param>) -> Self {
        Self::new_with_args("tuple", item_types)
    }

    fn tuple2(first: Self::Param, second: Self::Param) -> Self {
        Self::tuple(vec![first, second])
    }

    fn tuple3(first: Self::Param, second: Self::Param, third: Self::Param) -> Self {
        Self::tuple(vec![first, second, third])
    }

    fn identifier() -> Self {
        Self::new("identifier")
    }

    fn unknown() -> Self {
        Self::new("unknown")
    }

    fn function(params_types: Self::Param, return_type: Self::Param) -> Self {
        Self::new_with_args("function", vec![params_types, return_type])
    }

    fn typed(value: Self::Param) -> Self {
        Self::new_with_args("type", vec![value])
    }
}

impl From<BoundType> for EgonType {
    fn from(value: BoundType) -> Self {
        EgonType::Bound(value.into())
    }
}

impl Display for BoundType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let base = if self.params.is_empty() {
            self.name.to_string()
        } else if self.is_type() {
            self.params.clone().first().unwrap().to_string()
        } else {
            format!(
                "{}<{}>",
                self.name,
                self.params
                    .iter()
                    .map(|typeref| typeref.to_string())
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

    use crate::{BoundType, T};

    macro_rules! boundtype_is_test {
        ($test_name:ident, $typeref:expr, $($is_fn:ident),+ $(,)?) => {
            $(
                ::paste::paste! {
                    #[test]
                    fn [<$test_name _ $is_fn>]() {
                        let expr: $crate::BoundType = $typeref;
                        let result: bool = <$crate::BoundType as $crate::T>::$is_fn(&expr);

                        assert_eq!(true, result);
                    }
                }
            )+

            ::paste::paste! {
                #[test]
                fn [<$test_name _ is_typed>]() {
                    let expr: $crate::BoundType = <$crate::BoundType as $crate::T>::typed($typeref);
                    let result: bool = <$crate::BoundType as $crate::T>::is_type(&expr);

                    assert_eq!(true, result);
                }
            }
        };
    }

    macro_rules! boundtype_is_not_test {
        ($test_name:ident, $typeref:expr, $($is_fn:ident),+ $(,)?) => {
            $(
                ::paste::paste! {
                    #[test]
                    fn [<$test_name _not_ $is_fn>]() {
                        let expr: $crate::BoundType = $typeref;
                        let result: bool = <$crate::BoundType as $crate::T>::$is_fn(&expr);

                        assert_eq!(false, result);
                    }
                }
            )+
        };
    }

    boundtype_is_test!(test_unit_typeref, BoundType::unit(), is_unit, is_builtin);

    boundtype_is_not_test!(test_unknown_typeref, BoundType::unknown(), is_unit);

    boundtype_is_test!(
        test_unknown_typeref_is_builtin,
        BoundType::unknown(),
        is_unknown,
        is_builtin
    );

    boundtype_is_test!(
        test_number_typeref_is_number,
        BoundType::number(),
        is_number,
        is_builtin
    );

    boundtype_is_test!(
        test_number_typeref,
        BoundType::string(),
        is_string,
        is_builtin
    );

    boundtype_is_test!(
        test_unknown_list_typeref,
        BoundType::unknown_list(),
        is_list,
        is_unknown_list
    );

    boundtype_is_not_test!(
        test_unknown_list_typeref,
        BoundType::unknown_list(),
        is_known_list
    );

    boundtype_is_test!(
        test_number_list_typeref,
        BoundType::list(BoundType::number()),
        is_list,
        is_known_list,
        is_builtin
    );

    boundtype_is_not_test!(
        test_number_list_typeref,
        BoundType::list(BoundType::number()),
        is_unknown_list,
    );

    boundtype_is_not_test!(
        test_typeof_number_typeref,
        BoundType::typed(BoundType::number()),
        is_number
    );

    boundtype_is_test!(test_bool_typeref, BoundType::bool(), is_bool, is_builtin);

    boundtype_is_test!(
        test_identifier_typeref,
        BoundType::identifier(),
        is_identifier,
        is_builtin
    );

    boundtype_is_test!(test_range_typeref, BoundType::range(), is_range, is_builtin);

    boundtype_is_test!(
        test_tuple_typeref,
        BoundType::tuple2(BoundType::string(), BoundType::number()),
        is_tuple,
        is_builtin
    );

    boundtype_is_test!(
        test_empty_tuple_typeref,
        BoundType::tuple(vec![]),
        is_tuple,
        is_builtin
    );

    boundtype_is_test!(
        test_function_typeref_with_no_params_or_return_value,
        BoundType::function(BoundType::tuple(vec![]), BoundType::unit()),
        is_function,
        is_builtin
    );

    boundtype_is_test!(
        test_function_typeref_with_one_params_but_no_return_value,
        BoundType::function(
            BoundType::tuple(vec![BoundType::number()]),
            BoundType::unit()
        ),
        is_function,
        is_builtin
    );

    boundtype_is_test!(
        test_function_typeref_with_multiple_params_but_no_return_value,
        BoundType::function(
            BoundType::tuple3(BoundType::number(), BoundType::string(), BoundType::bool()),
            BoundType::unit()
        ),
        is_function,
        is_builtin
    );

    boundtype_is_test!(
        test_function_typeref_with_multiple_params_and_return_value,
        BoundType::function(
            BoundType::tuple2(BoundType::number(), BoundType::number()),
            BoundType::number()
        ),
        is_function,
        is_builtin
    );

    boundtype_is_test!(
        test_function_typeref_curried,
        BoundType::function(
            BoundType::tuple(vec![BoundType::number()]),
            BoundType::function(
                BoundType::tuple(vec![BoundType::number()]),
                BoundType::number()
            )
        ),
        is_function,
        is_builtin
    );

    #[test]
    fn test_get_return_type_from_function() {
        let fn_type = BoundType::function(
            BoundType::tuple(vec![BoundType::number()]),
            BoundType::number(),
        );

        assert_eq!(BoundType::number(), fn_type.get_function_return());
    }
}
