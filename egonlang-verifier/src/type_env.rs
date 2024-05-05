use std::collections::HashMap;

use egonlang_core::ast::TypeRef;

/// Typing information stored about an identifier
#[derive(Debug, PartialEq, Clone)]
pub struct TypeEnvValue {
    pub typeref: TypeRef,
    pub is_const: bool,
}

impl TypeEnvValue {
    fn map_typeref(&self, typeref: TypeRef) -> TypeEnvValue {
        let mut env_var = self.clone();
        env_var.typeref = typeref;

        env_var
    }
}

#[derive(Default)]
pub struct TypeEnv<'a> {
    root: Option<&'a TypeEnv<'a>>,
    values: HashMap<String, TypeEnvValue>,
}

impl<'a> TypeEnv<'a> {
    pub fn new() -> Self {
        Default::default()
    }

    /// Create a new type environment by extending the current one
    pub fn extend(&'a self) -> Box<TypeEnv<'a>> {
        Box::from(TypeEnv {
            root: Some(self),
            values: HashMap::new(),
        })
    }

    /// Attempt to resolve an identifier's type
    pub fn get(&self, identifier: &str) -> Option<TypeEnvValue> {
        let result = match self.values.get(identifier) {
            Some(result) => Some(result.clone()),
            None => match self.root {
                Some(root) => root.get(identifier),
                _ => None,
            },
        };

        if result.is_some() {
            let result = result.clone().unwrap();

            if result.typeref.0 == *"type" {
                return Some(result.map_typeref(result.typeref.1.first().unwrap().clone()));
            }
        }

        result
    }

    pub fn set(&mut self, identifier: &str, value: TypeEnvValue) -> Option<TypeEnvValue> {
        // Is this a type alias?
        if value.typeref.0 == *"type" {
            // Grab the aliased type from the value's typeref
            //
            // type A = number;
            // ---------^ TypeRef::typed(TypeRef::number())
            //
            // This grabs the type `number` from the assigment value
            // new_type = TypeRef::number()
            let new_typeref = value.typeref.1.first().unwrap().clone();

            // Look up the new typeref (as a string) in the type environment
            // This flattens out type aliases aliasing type aliases
            //
            // type A = number;
            // type B = A;
            // ---------^ TypeRef::typed(TypeRef::number())
            return match self.get(&new_typeref.to_string()) {
                Some(aliased_type) => {
                    // Set the name to the flattened resolved type in the type env
                    //
                    // type A = number;
                    // type B = A;
                    //
                    // B is set to `number`
                    // println!("Set `{name}` from type env as {aliased_type:?}");
                    self.values.insert(identifier.to_string(), aliased_type)
                }
                None => {
                    // Set the name to the type
                    //
                    // type A = number;
                    //
                    // A is set to `number`
                    // println!("Set `{name}` from type env as {new_typeref:?}");
                    self.values
                        .insert(identifier.to_string(), value.map_typeref(new_typeref))
                }
            };
        }

        // Type isn't aliased, set name to the value's type
        // println!("Set `{name}` from type env as {value:?}");
        self.values.insert(identifier.to_string(), value)
    }
}

#[cfg(test)]
mod type_env_tests {
    use egonlang_core::ast::TypeRef;
    use pretty_assertions::assert_eq;

    use super::{TypeEnv, TypeEnvValue};

    #[test]
    fn get_returns_some_type_env_value_if_exists() {
        let mut env = TypeEnv::new();

        env.set(
            "a",
            TypeEnvValue {
                typeref: TypeRef::number(),
                is_const: false,
            },
        );

        assert_eq!(
            Some(TypeEnvValue {
                typeref: TypeRef::number(),
                is_const: false,
            }),
            env.get("a")
        );
    }

    #[test]
    fn get_returns_none_if_not_exists() {
        let env = TypeEnv::new();

        assert_eq!(None, env.get("a"));
    }

    #[test]
    fn extended_env_returns_some_type_value_if_exists() {
        let mut env = TypeEnv::new();

        env.set(
            "a",
            TypeEnvValue {
                typeref: TypeRef::number(),
                is_const: false,
            },
        );

        let env2 = env.extend();

        assert_eq!(
            Some(TypeEnvValue {
                typeref: TypeRef::number(),
                is_const: false,
            }),
            env2.get("a")
        );
    }

    #[test]
    fn extended_env_returns_some_type_value_if_new_value_exists() {
        let mut env = TypeEnv::new();

        env.set(
            "a",
            TypeEnvValue {
                typeref: TypeRef::number(),
                is_const: false,
            },
        );

        let mut env2 = env.extend();

        env2.set(
            "a",
            TypeEnvValue {
                typeref: TypeRef::bool(),
                is_const: false,
            },
        );

        assert_eq!(
            Some(TypeEnvValue {
                typeref: TypeRef::bool(),
                is_const: false,
            }),
            env2.get("a")
        );
    }

    #[test]
    fn type_alias_returns_aliased_type() {
        let mut env = TypeEnv::new();

        // Alias type `Int` to type `number`
        env.set(
            "Int",
            TypeEnvValue {
                typeref: TypeRef::typed(TypeRef::number()),
                is_const: true,
            },
        );

        assert_eq!(
            Some(TypeEnvValue {
                typeref: TypeRef::number(),
                is_const: true,
            }),
            env.get("Int")
        );
    }

    #[test]
    fn type_alias_returns_aliased_type_2() {
        let mut env = TypeEnv::new();

        // Alias type `Int` to type `number`
        env.set(
            "Int",
            TypeEnvValue {
                typeref: TypeRef::typed(TypeRef::number()),
                is_const: true,
            },
        );

        // Alias type `Int2` to type `Int`
        env.set(
            "Int2",
            TypeEnvValue {
                typeref: TypeRef::typed(TypeRef("Int".to_string(), vec![])),
                is_const: true,
            },
        );

        // Alias type `Int2` resolves to the root type of `number`
        // `Int2` -> `Int` -> `number`
        assert_eq!(
            Some(TypeEnvValue {
                typeref: TypeRef::number(),
                is_const: true,
            }),
            env.get("Int2")
        );
    }
}
