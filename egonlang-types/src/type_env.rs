use std::{collections::HashMap, fmt};

use crate::{egon_bool, egon_number, egon_range, egon_string, Type};

/// Store and retreive type information for string identifiers
#[derive(Default)]
pub struct TypeEnv {
    scopes: Vec<HashMap<String, TypeEnvValue>>,
}

impl TypeEnv {
    pub fn new() -> Self {
        let mut type_env = TypeEnv {
            ..Default::default()
        };

        type_env.start_scope();

        type_env.set("number", egon_number!().into());
        type_env.set("bool", egon_bool!().into());
        type_env.set("string", egon_string!().into());
        type_env.set("range", egon_range!().into());

        type_env
    }

    /// Resolve string to possible typing information
    pub fn get(&self, identifier: &str) -> Option<TypeEnvValue> {
        tracelog::tracelog!(
            label = type_env,get;
            "(level: {}) Looking up identifier {}",
            self.get_scope_depth(),
            identifier.cyan()
        );

        let mut result: Option<&TypeEnvValue> = None;

        for scope in self.scopes.iter().rev() {
            if let Some(scope_result) = scope.get(identifier) {
                result = Some(scope_result);
                break;
            }
        }

        if result.is_some() {
            let result = result.unwrap();

            if result.of_type.is_type() {
                tracelog::tracelog!(
                    label = type_env,get;
                    "(level:{}) Got type alias {} for {}",
                    self.get_scope_depth(),
                    result.of_type.to_string().italic().yellow(),
                    identifier.cyan()
                );

                return Some(result.map(result.of_type.type_args().first().unwrap()));
            }
        }

        match result {
            Some(result) => {
                tracelog::tracelog!(
                    label = type_env,get;
                    "(level:{}) Got type {} for {}",
                    self.get_scope_depth(),
                    result.of_type.to_string().italic().yellow(),
                    identifier.cyan()
                );
            }
            None => {
                tracelog::tracelog!(
                    label = type_env,get,error;
                    "(level: {}) Unable to find type for {} in type env",
                    self.get_scope_depth(),
                    identifier.cyan()
                );
            }
        }

        result.cloned()
    }

    /// Set string to have typing information
    pub fn set(&mut self, identifier: &str, type_env_value: TypeEnvValue) -> Option<TypeEnvValue> {
        // Is this a type alias?
        if type_env_value.of_type.is_type() {
            // Grab the aliased type from the value's typeref
            //
            // type A = number;
            // ---------^ Type::typed(Type::number())
            //
            // This grabs the type `number` from the assigment value
            // new_type = Type::number()
            let new_typeref = type_env_value.of_type.type_args().first().unwrap().clone();

            // Look up the new typeref (as a string) in the type environment
            // This flattens out type aliases aliasing type aliases
            //
            // type A = number;
            // type B = A;
            // ---------^ Type::typed(Type::number())
            let get = self.get(&new_typeref.to_string());

            return match get {
                Some(aliased_type) => {
                    tracelog::tracelog!(
                        label = type_env,set,type_alias;
                        "(level:{}) Setting {} to type alias {}",
                        self.get_scope_depth(),
                        identifier.cyan(),
                        aliased_type.of_type.to_string().italic().yellow()
                    );

                    // Set the name to the flattened resolved type in the type env
                    //
                    // type A = number;
                    // type B = A;
                    //
                    // B is set to `number`
                    // println!("Set `{name}` from type env as {aliased_type:?}");
                    let aliased_type = aliased_type.clone();
                    let scope = self.get_mut_scope().unwrap();
                    scope.insert(identifier.to_string(), aliased_type)
                }
                None => {
                    tracelog::tracelog!(
                        label = type_env,set;
                        "(level: {}) Setting {} to the type alias {}",
                        self.get_scope_depth(),
                        identifier.cyan(),
                        new_typeref.to_string().italic().yellow()
                    );
                    // Set the name to the type
                    //
                    // type A = number;
                    //
                    // A is set to `number`
                    // println!("Set `{name}` from type env as {new_typeref:?}");
                    let scope = self.get_mut_scope().unwrap();
                    scope.insert(
                        identifier.to_string(),
                        TypeEnvValue {
                            of_type: new_typeref,
                            is_const: true,
                        },
                    )
                }
            };
        }

        tracelog::tracelog!(
            label = type_env,set;
            "(level: {}) Setting {} to the type {}",
            self.get_scope_depth(),
            identifier.cyan(),
            type_env_value.of_type.to_string().italic().yellow()
        );

        // Type isn't aliased, set name to the value's type
        // println!("Set `{name}` from type env as {value:?}");
        let scope = self.get_mut_scope().unwrap();
        scope.insert(identifier.to_string(), type_env_value)
    }

    /// Start a new scope
    ///
    /// New values will shadow lower scopes while the new scope is active
    pub fn start_scope(&mut self) -> usize {
        let new_scope = HashMap::new();
        self.scopes.push(new_scope);

        let new_depth = self.get_scope_depth();

        tracelog::tracelog!(
            label = type_env,start_scope;
            "Starting new scope (new level: {new_depth})"
        );

        new_depth
    }

    /// End the current scope
    ///
    /// All values defined in the current scope will be removed
    ///
    /// Values that were shadowed by the current scope will be restored to
    /// their previous value
    pub fn end_scope(&mut self) -> Result<usize, RootScopeEndedError> {
        let depth = self.get_scope_depth();

        if depth == 1 {
            return Err(RootScopeEndedError);
        }

        self.scopes.pop();

        let new_depth = self.get_scope_depth();

        tracelog::tracelog!(
            label = type_env,start_scope;
            "Starting new scope (new level: {new_depth})"
        );

        Ok(new_depth)
    }

    /// Get the depth of current scope
    pub fn get_scope_depth(&self) -> usize {
        self.scopes.len()
    }

    /// Get a mutable reference to the current scope
    fn get_mut_scope(&mut self) -> Option<&mut HashMap<String, TypeEnvValue>> {
        let depth = self.get_scope_depth();
        self.scopes
            .get_mut(depth.checked_sub(1).unwrap_or_default())
    }
}

/// Typing information stored about an identifier
#[derive(Debug, PartialEq, Clone)]
pub struct TypeEnvValue {
    pub of_type: Type,
    pub is_const: bool,
}

impl TypeEnvValue {
    pub fn new(of_type: Type) -> Self {
        Self {
            of_type,
            is_const: false,
        }
    }

    pub fn new_const(of_type: Type) -> Self {
        Self {
            of_type,
            is_const: true,
        }
    }

    fn map(&self, of_type: &Type) -> TypeEnvValue {
        TypeEnvValue {
            of_type: of_type.clone(),
            is_const: self.is_const,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct RootScopeEndedError;

impl fmt::Display for RootScopeEndedError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Root scope for type environment was ended")
    }
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use crate::{
        egon_bool, egon_number,
        type_env::{RootScopeEndedError, TypeEnv, TypeEnvValue},
        Type,
    };

    #[test]
    fn get_depth_on_new() {
        let env = TypeEnv::new();

        assert_eq!(1, env.get_scope_depth());
    }

    #[test]
    fn get_depth_while_starting_and_stopping_scopes() {
        let mut env = TypeEnv::new();

        env.start_scope();
        env.start_scope();

        assert_eq!(3, env.get_scope_depth());

        let _ = env.end_scope();

        assert_eq!(2, env.get_scope_depth());

        let _ = env.end_scope();

        assert_eq!(1, env.get_scope_depth());
    }

    #[test]
    fn end_root_scope() {
        let mut env = TypeEnv::new();

        let result = env.end_scope().err().unwrap();

        assert_eq!(RootScopeEndedError, result);
    }

    #[test]
    fn get_returns_some_type_env_value_if_exists() {
        let mut env = TypeEnv::new();

        env.set("a", TypeEnvValue::new_const(egon_number!()));

        let result = env.get("a");

        assert_eq!(Some(TypeEnvValue::new_const(egon_number!())), result);
    }

    #[test]
    fn get_returns_none_if_not_exists() {
        let env = TypeEnv::new();

        assert_eq!(None, env.get("a"));
    }

    #[test]
    fn type_alias_returns_aliased_type() {
        let mut env = TypeEnv::new();

        // Alias type `Int` to type `number`
        env.set("Int", Type::typed(Type::number()).into());

        assert_eq!(Some(egon_number!().into()), env.get("Int"));
    }

    #[test]
    fn type_alias_returns_aliased_type_2() {
        let mut env = TypeEnv::new();

        // Alias type `Int` to type `number`
        env.set("Int", Type::typed(Type::number()).into());

        // Alias type `Int2` to type `Int`
        env.set("Int2", Type::typed(Type("Int".to_string(), vec![])).into());

        // Alias type `Int2` resolves to the root type of `number`
        // `Int2` -> `Int` -> `number`
        assert_eq!(Some(egon_number!().into()), env.get("Int2"));
    }

    #[test]
    fn override_type_in_new_scope() {
        let mut env = TypeEnv::new();

        env.set("a", egon_number!().into());

        let t = env.get("a");
        assert_eq!(Some(egon_number!().into()), t);

        env.start_scope();

        env.set("a", egon_bool!().into());

        let t = env.get("a");
        assert_eq!(Some(egon_bool!().into()), t);

        let _ = env.end_scope();

        let t = env.get("a");
        assert_eq!(Some(egon_number!().into()), t);
    }
}
