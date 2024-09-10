use std::{collections::HashMap, fmt};

use egonlang_errors::EgonError;
use serde::{Deserialize, Serialize};
use tracelog::{log_identifier, log_type};

use crate::Type;

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize, Default)]
pub struct Scope {
    pub variables: HashMap<String, Type>,
    pub constants: HashMap<String, Type>,
    pub type_aliases: HashMap<Type, Type>,
}

impl Scope {
    pub fn get_variable(&self, key: &str) -> Option<&Type> {
        self.variables.get(key)
    }

    pub fn set_variable(&mut self, key: &str, value: Type) -> Result<(), EgonError> {
        if let Some(_) = self.get_const(key) {
            return Err(EgonError::TypeError(
                egonlang_errors::EgonTypeError::AlreadyDefined(key.to_owned()),
            ));
        }

        self.variables.insert(key.to_owned(), value);

        Ok(())
    }

    pub fn get_const(&self, key: &str) -> Option<&Type> {
        self.constants.get(key)
    }

    pub fn set_const(&mut self, key: &str, value: Type) -> Result<(), EgonError> {
        if let Some(_) = self.get_const(key) {
            return Err(EgonError::TypeError(
                egonlang_errors::EgonTypeError::AlreadyDefined(key.to_owned()),
            ));
        }

        if let Some(_) = self.get_variable(key) {
            return Err(EgonError::TypeError(
                egonlang_errors::EgonTypeError::AlreadyDefined(key.to_owned()),
            ));
        }

        self.constants.insert(key.to_owned(), value);

        Ok(())
    }

    pub fn get_type_alias(&self, key: &Type) -> Option<&Type> {
        self.get_type_alias(key)
    }

    pub fn set_type_alias(&mut self, key: Type, value: Type) -> Result<(), EgonError> {
        self.type_aliases.insert(key, value);

        Ok(())
    }
}

/// Store and retreive type information for string identifiers
#[derive(Default, Debug, PartialEq)]
pub struct TypeEnv {
    scopes: Vec<Scope>,
}

impl TypeEnv {
    pub fn new() -> Self {
        let mut type_env = TypeEnv {
            ..Default::default()
        };

        type_env.start_scope();

        type_env
    }

    pub fn get_variable(&self, key: &str) -> Option<&Type> {
        tracelog::tracelog!(
            label = type_env,get_variable;
            "(level: {}) Looking up variable {}",
            self.get_scope_depth(),
            log_identifier(key)
        );

        let mut value: Option<&Type> = None;

        for scope in &self.scopes {
            if let Some(scope_value) = scope.get_variable(key) {
                value = Some(scope_value);
            }
        }

        value
    }

    pub fn set_variable(&mut self, key: &str, value: Type) -> Result<(), EgonError> {
        tracelog::tracelog!(
            label = type_env,set_variable;
            "(level: {}) Setting variable {} with type {}",
            self.get_scope_depth(),
            log_identifier(key),
            log_type(&value.to_string())
        );

        let scope = self.get_mut_scope().expect("scope should be avaliable");
        scope.set_variable(key, value)?;

        Ok(())
    }

    pub fn get_const(&self, key: &str) -> Option<&Type> {
        tracelog::tracelog!(
            label = type_env,get_const;
            "(level: {}) Looking up constant {}",
            self.get_scope_depth(),
            log_identifier(key)
        );

        let mut value: Option<&Type> = None;

        for scope in &self.scopes {
            if let Some(scope_value) = scope.get_const(key) {
                value = Some(scope_value);
            }
        }

        value
    }

    pub fn set_const(&mut self, key: &str, value: Type) -> Result<(), EgonError> {
        tracelog::tracelog!(
            label = type_env,set_const;
            "(level: {}) Setting constant {} with type {}",
            self.get_scope_depth(),
            log_identifier(key),
            log_type(&value.to_string())
        );

        let scope = self.get_mut_scope().expect("scope should be avaliable");
        scope.set_const(key, value)?;

        Ok(())
    }

    pub fn get_type_alias(&self, key: Type) -> Option<&Type> {
        tracelog::tracelog!(
            label = type_env,get_type_alias;
            "(level: {}) Looking up type alias {}",
            self.get_scope_depth(),
            log_type(&key.to_string())
        );

        let mut value: Option<&Type> = None;

        for scope in &self.scopes {
            if let Some(scope_value) = scope.get_type_alias(&key) {
                value = Some(scope_value);
            }
        }

        value
    }

    pub fn set_type_alias(&mut self, key: Type, value: Type) -> Result<(), EgonError> {
        tracelog::tracelog!(
            label = type_env,set_const;
            "(level: {}) Setting constant {} with type {}",
            self.get_scope_depth(),
            log_type(&key),
            log_type(&value)
        );

        let scope = self.get_mut_scope().expect("scope should be avaliable");
        scope.set_type_alias(key, value)?;

        Ok(())
    }

    // /// Resolve string to possible typing information
    // pub fn get(&self, identifier: &str) -> Option<TypeEnvValue> {
    //     tracelog::tracelog!(
    //         label = type_env,get;
    //         "(level: {}) Looking up identifier {}",
    //         self.get_scope_depth(),
    //         log_identifier(identifier)
    //     );

    //     let mut result: Option<&TypeEnvValue> = None;

    //     for scope in self.scopes.iter().rev() {
    //         if let Some(scope_result) = scope.get(identifier) {
    //             result = Some(scope_result);
    //             break;
    //         }
    //     }

    //     if result.is_some() {
    //         let result = result.unwrap();

    //         if result.of_type.is_type() {
    //             tracelog::tracelog!(
    //                 label = type_env,get;
    //                 "(level:{}) Got type alias {} for {}",
    //                 self.get_scope_depth(),
    //                 log_type(&result.of_type),
    //                 log_identifier(identifier)
    //             );

    //             return Some(result.map(result.of_type.params().first().unwrap()));
    //         }
    //     }

    //     match result {
    //         Some(result) => {
    //             tracelog::tracelog!(
    //                 label = type_env,get;
    //                 "(level:{}) Got type {} for {}",
    //                 self.get_scope_depth(),
    //                 log_type(&result.of_type),
    //                 log_identifier(identifier)
    //             );
    //         }
    //         None => {
    //             tracelog::tracelog!(
    //                 label = type_env,get,error;
    //                 "(level: {}) Unable to find type for {} in type env",
    //                 self.get_scope_depth(),
    //                 log_identifier(identifier)
    //             );
    //         }
    //     }

    //     result.cloned()
    // }

    // /// Set string to have typing information
    // pub fn set(&mut self, identifier: &str, type_env_value: TypeEnvValue) -> Option<TypeEnvValue> {
    //     // Is this a type alias?
    //     if type_env_value.of_type.is_type() {
    //         // Grab the aliased type from the value's typeref
    //         //
    //         // type A = number;
    //         // ---------^ Type::typed(Type::number())
    //         //
    //         // This grabs the type `number` from the assigment value
    //         // new_type = Type::number()
    //         let new_typeref = type_env_value.of_type.params().first().unwrap().clone();

    //         // Look up the new typeref (as a string) in the type environment
    //         // This flattens out type aliases aliasing type aliases
    //         //
    //         // type A = number;
    //         // type B = A;
    //         // ---------^ Type::typed(Type::number())
    //         let get = self.get(&new_typeref.to_string());

    //         return match get {
    //             Some(aliased_type) => {
    //                 tracelog::tracelog!(
    //                     label = type_env,set,type_alias;
    //                     "(level:{}) Setting {} to type alias {}",
    //                     self.get_scope_depth(),
    //                     log_identifier(identifier),
    //                     log_type(&aliased_type.of_type)
    //                 );

    //                 // Set the name to the flattened resolved type in the type env
    //                 //
    //                 // type A = number;
    //                 // type B = A;
    //                 //
    //                 // B is set to `number`
    //                 // println!("Set `{name}` from type env as {aliased_type:?}");
    //                 let aliased_type = aliased_type.clone();
    //                 let scope = self.get_mut_scope().unwrap();
    //                 scope.insert(identifier.to_string(), aliased_type)
    //             }
    //             None => {
    //                 tracelog::tracelog!(
    //                     label = type_env,set;
    //                     "(level: {}) Setting {} to the type alias {}",
    //                     self.get_scope_depth(),
    //                     log_identifier(identifier),
    //                     log_type(&new_typeref)
    //                 );
    //                 // Set the name to the type
    //                 //
    //                 // type A = number;
    //                 //
    //                 // A is set to `number`
    //                 // println!("Set `{name}` from type env as {new_typeref:?}");
    //                 let scope = self.get_mut_scope().unwrap();
    //                 scope.insert(
    //                     identifier.to_string(),
    //                     TypeEnvValue {
    //                         of_type: new_typeref,
    //                         is_const: true,
    //                     },
    //                 )
    //             }
    //         };
    //     }

    //     tracelog::tracelog!(
    //         label = type_env,set;
    //         "(level: {}) Setting {} to the type {}",
    //         self.get_scope_depth(),
    //         log_identifier(identifier),
    //         log_type(&type_env_value.of_type)
    //     );

    //     // Type isn't aliased, set name to the value's type
    //     // println!("Set `{name}` from type env as {value:?}");
    //     let scope = self.get_mut_scope().unwrap();
    //     scope.insert(identifier.to_string(), type_env_value)
    // }

    /// Start a new scope
    ///
    /// New values will shadow lower scopes while the new scope is active
    pub fn start_scope(&mut self) -> usize {
        let new_scope = Scope::default();
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
    fn get_mut_scope(&mut self) -> Option<&mut Scope> {
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
    use egonlang_errors::EgonError;
    use pretty_assertions::assert_eq;

    use crate::{
        type_env::{RootScopeEndedError, TypeEnv},
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
    fn set_const_and_get_const_returns_some() {
        let mut env = TypeEnv::new();

        let _ = env.set_const("a", Type::number());

        assert_eq!(Some(&Type::number()), env.get_const("a"));
    }

    #[test]
    fn set_const_and_get_variable_returns_none() {
        let mut env = TypeEnv::new();

        let _ = env.set_const("a", Type::number());

        assert_eq!(None, env.get_variable("a"));
    }

    #[test]
    fn set_variable_and_get_variable_returns_some() {
        let mut env = TypeEnv::new();

        let _ = env.set_variable("a", Type::number());

        assert_eq!(Some(&Type::number()), env.get_variable("a"));
    }

    #[test]
    fn set_variable_and_get_const_returns_none() {
        let mut env = TypeEnv::new();

        let _ = env.set_variable("a", Type::number());

        assert_eq!(None, env.get_const("a"));
    }

    //

    #[test]
    fn set_const_set_variable_returns_err() {
        let mut env = TypeEnv::new();

        let _ = env.set_const("a", Type::number());

        assert_eq!(
            Err(EgonError::TypeError(
                egonlang_errors::EgonTypeError::AlreadyDefined("a".to_owned())
            )),
            env.set_variable("a", Type::string())
        );
    }

    #[test]
    fn set_const_set_const_returns_err() {
        let mut env = TypeEnv::new();

        let _ = env.set_const("a", Type::number());

        assert_eq!(
            Err(EgonError::TypeError(
                egonlang_errors::EgonTypeError::AlreadyDefined("a".to_owned())
            )),
            env.set_const("a", Type::string())
        );
    }

    #[test]
    fn set_variable_and_set_const_returns_err() {
        let mut env = TypeEnv::new();

        let _ = env.set_variable("a", Type::number());

        assert_eq!(
            Err(EgonError::TypeError(
                egonlang_errors::EgonTypeError::AlreadyDefined("a".to_owned())
            )),
            env.set_const("a", Type::number())
        );
    }

    #[test]
    fn set_variable_and_set_variable_returns_ok() {
        let mut env = TypeEnv::new();

        let _ = env.set_variable("a", Type::number());

        assert_eq!(Ok(()), env.set_variable("a", Type::number()));
    }

    // #[test]
    // fn get_returns_some_type_env_value_if_exists() {
    //     let mut env = TypeEnv::new();

    //     env.set("a", TypeEnvValue::new_const(egon_number!()));

    //     let result = env.get("a");

    //     assert_eq!(Some(TypeEnvValue::new_const(egon_number!())), result);
    // }

    // #[test]
    // fn get_returns_none_if_not_exists() {
    //     let env = TypeEnv::new();

    //     assert_eq!(None, env.get("a"));
    // }

    // #[test]
    // fn type_alias_returns_aliased_type() {
    //     let mut env = TypeEnv::new();

    //     // Alias type `Int` to type `number`
    //     env.set("Int", Type::typed(Type::number()).into());

    //     assert_eq!(Some(egon_number!().into()), env.get("Int"));
    // }

    // #[test]
    // fn type_alias_returns_aliased_type_2() {
    //     let mut env = TypeEnv::new();

    //     // Alias type `Int` to type `number`
    //     env.set("Int", Type::typed(Type::number()).into());

    //     // Alias type `Int2` to type `Int`
    //     env.set("Int2", Type::typed(Type::new("Int")).into());

    //     // Alias type `Int2` resolves to the root type of `number`
    //     // `Int2` -> `Int` -> `number`
    //     assert_eq!(Some(egon_number!().into()), env.get("Int2"));
    // }

    // #[test]
    // fn override_type_in_new_scope() {
    //     let mut env = TypeEnv::new();

    //     env.set("a", egon_number!().into());

    //     let t = env.get("a");
    //     assert_eq!(Some(egon_number!().into()), t);

    //     env.start_scope();

    //     env.set("a", egon_bool!().into());

    //     let t = env.get("a");
    //     assert_eq!(Some(egon_bool!().into()), t);

    //     let _ = env.end_scope();

    //     let t = env.get("a");
    //     assert_eq!(Some(egon_number!().into()), t);
    // }
}
