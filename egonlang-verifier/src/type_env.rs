use std::collections::HashMap;

use egonlang_types::Type;

use crate::verify_trace;

/// Typing information stored about an identifier
#[derive(Debug, PartialEq, Clone)]
pub struct TypeEnvValue {
    pub typeref: Type,
    pub is_const: bool,
}

impl TypeEnvValue {
    pub fn new(typeref: Type) -> Self {
        Self {
            typeref,
            is_const: false,
        }
    }

    pub fn new_const(typeref: Type) -> Self {
        Self {
            typeref,
            is_const: true,
        }
    }

    fn map_typeref(&self, typeref: Type) -> TypeEnvValue {
        let mut env_var = self.clone();
        env_var.typeref = typeref;

        env_var
    }
}

/// Type environment that tracks string identifiers to typing info ([`TypeEnvValue`])
///
/// [`Verifier`](crate::Verifier) uses a stack of type environments to represent lexical scopes
#[derive(Default)]
pub struct TypeEnv {
    /// Index of where this type env is in the [`Verifier`](crate::Verifier)'s type env stack
    level: usize,
    values: HashMap<String, TypeEnvValue>,
}

impl TypeEnv {
    pub fn new(level: usize) -> Self {
        TypeEnv {
            level,
            ..Default::default()
        }
    }

    /// Get this type environment's index in the [`Verifier`](crate::Verifier)'s type env stack
    pub fn level(&self) -> usize {
        self.level
    }

    /// Attempt to resolve an identifier's type
    pub fn get(&self, identifier: &str) -> Option<TypeEnvValue> {
        verify_trace!(
            type_env get: "(level: {}) Looking up identifier {}",
            self.level(),
            identifier.cyan()
        );

        let result = self.values.get(identifier);

        if result.is_some() {
            let result = result.unwrap();

            if result.typeref.is_type() {
                verify_trace!(
                    "(level:{}) Got type alias {} for {}",
                    self.level(),
                    result.typeref.to_string().italic().yellow(),
                    identifier.cyan()
                );

                return Some(result.map_typeref(result.typeref.1.first().unwrap().clone()));
            }
        }

        match result {
            Some(result) => {
                verify_trace!(
                    type_env get: "(level:{}) Got type {} for {}",
                    self.level(),
                    result.typeref.to_string().italic().yellow(),
                    identifier.cyan()
                );
            }
            None => {
                verify_trace!(
                    type_env get error:
                    "(level: {}) Unable to find type for {} in type env",
                    self.level(),
                    identifier.cyan()
                );
            }
        }

        result.cloned()
    }

    /// Set an identifier's type
    pub fn set(&mut self, identifier: &str, value: TypeEnvValue) -> Option<TypeEnvValue> {
        // Is this a type alias?
        if value.typeref.0 == *"type" {
            // Grab the aliased type from the value's typeref
            //
            // type A = number;
            // ---------^ Type::typed(Type::number())
            //
            // This grabs the type `number` from the assigment value
            // new_type = Type::number()
            let new_typeref = value.typeref.1.first().unwrap().clone();

            // Look up the new typeref (as a string) in the type environment
            // This flattens out type aliases aliasing type aliases
            //
            // type A = number;
            // type B = A;
            // ---------^ Type::typed(Type::number())
            return match self.get(&new_typeref.to_string()) {
                Some(aliased_type) => {
                    verify_trace!(
                        type_env set type_alias:
                        "(level:{}) Setting {} to type alias {}",
                        self.level(),
                        identifier.cyan(),
                        aliased_type.typeref.to_string().italic().yellow()
                    );

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
                    verify_trace!(
                        type_env set:
                        "(level: {}) Setting {} to the type alias {}",
                        self.level(),
                        identifier.cyan(),
                        new_typeref.to_string().italic().yellow()
                    );
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

        verify_trace!(
            type_env set:
            "(level: {}) Setting {} to the type {}",
            self.level(),
            identifier.cyan(),
            value.typeref.to_string().italic().yellow()
        );

        // Type isn't aliased, set name to the value's type
        // println!("Set `{name}` from type env as {value:?}");
        self.values.insert(identifier.to_string(), value)
    }
}

#[cfg(test)]
mod type_env_tests {
    use egonlang_types::Type;
    use pretty_assertions::assert_eq;

    use super::{TypeEnv, TypeEnvValue};

    #[test]
    fn get_returns_some_type_env_value_if_exists() {
        let mut env = TypeEnv::new(0);

        env.set(
            "a",
            TypeEnvValue {
                typeref: Type::number(),
                is_const: false,
            },
        );

        assert_eq!(
            Some(TypeEnvValue {
                typeref: Type::number(),
                is_const: false,
            }),
            env.get("a")
        );
    }

    #[test]
    fn get_returns_none_if_not_exists() {
        let env = TypeEnv::new(0);

        assert_eq!(None, env.get("a"));
    }

    #[test]
    fn type_alias_returns_aliased_type() {
        let mut env = TypeEnv::new(0);

        // Alias type `Int` to type `number`
        env.set(
            "Int",
            TypeEnvValue {
                typeref: Type::typed(Type::number()),
                is_const: true,
            },
        );

        assert_eq!(
            Some(TypeEnvValue {
                typeref: Type::number(),
                is_const: true,
            }),
            env.get("Int")
        );
    }

    #[test]
    fn type_alias_returns_aliased_type_2() {
        let mut env = TypeEnv::new(0);

        // Alias type `Int` to type `number`
        env.set(
            "Int",
            TypeEnvValue {
                typeref: Type::typed(Type::number()),
                is_const: true,
            },
        );

        // Alias type `Int2` to type `Int`
        env.set(
            "Int2",
            TypeEnvValue {
                typeref: Type::typed(Type("Int".to_string(), vec![])),
                is_const: true,
            },
        );

        // Alias type `Int2` resolves to the root type of `number`
        // `Int2` -> `Int` -> `number`
        assert_eq!(
            Some(TypeEnvValue {
                typeref: Type::number(),
                is_const: true,
            }),
            env.get("Int2")
        );
    }
}
