use std::{collections::HashMap, fmt};

use serde::{Deserialize, Serialize};
use tracelog::{log_identifier, log_type};

use crate::{BoundType, EgonType, T};

#[derive(Default, Debug, PartialEq, Serialize, Deserialize)]
pub struct Scope {
    variables: HashMap<String, EgonType>,
    consts: HashMap<String, EgonType>,
    types: HashMap<EgonType, EgonType>,
}

/// Store and retreive type information for string identifiers
#[derive(Default)]
pub struct TypeEnv {
    scopes: Vec<Scope>,
}

impl TypeEnv {
    pub fn new() -> Self {
        let mut type_env = TypeEnv {
            ..Default::default()
        };

        type_env.start_scope();

        type_env.set_const("number", EgonType::number());
        type_env.set_const("bool", EgonType::bool());
        type_env.set_const("string", EgonType::string());
        type_env.set_const("range", EgonType::range());

        type_env
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

    //             return Some(result.map(&result.of_type.params().first().cloned().unwrap().into()));
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

    pub fn set_variable(&mut self, name: &str, value: EgonType) -> Option<EgonType> {
        todo!()
    }

    pub fn set_const(&mut self, name: &str, value: EgonType) -> Option<EgonType> {
        todo!()
    }

    pub fn set_type_alias(&mut self, alias: EgonType, value: EgonType) -> Option<EgonType> {
        todo!()
    }

    pub fn get_variable(&self, name: &str) -> Option<EgonType> {
        todo!()
    }

    pub fn get_const(&self, name: &str) -> Option<EgonType> {
        todo!()
    }

    pub fn get_type_alias(&self, alias: EgonType) -> Option<EgonType> {
        todo!()
    }

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
    //         let new_typeref = match type_env_value.of_type.params().first().unwrap().clone() {
    //             crate::EgonTypeParam::Unbound(t) => todo!(),
    //             crate::EgonTypeParam::Bound(t) => t,
    //         };

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
    //                         of_type: new_typeref.into(),
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
    pub of_type: EgonType,
    pub is_const: bool,
}

impl TypeEnvValue {
    pub fn new(of_type: EgonType) -> Self {
        Self {
            of_type,
            is_const: false,
        }
    }

    pub fn new_const(of_type: EgonType) -> Self {
        Self {
            of_type,
            is_const: true,
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
        type_env::{RootScopeEndedError, TypeEnv},
        BoundType, EgonType, EgonTypeParam, UnboundType, T,
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
    fn set_and_get_variable_returns_type() {
        let mut env = TypeEnv::new();

        env.set_variable("a", EgonType::number());

        let result = env.get_variable("a");

        assert_eq!(Some(EgonType::number()), result);
    }

    #[test]
    fn get_undefined_variable_returns_none() {
        let env = TypeEnv::new();

        assert_eq!(None, env.get_variable("a"));
    }

    #[test]
    fn set_and_get_const_returns_type() {
        let mut env = TypeEnv::new();

        env.set_const("a", EgonType::number());

        let result = env.get_const("a");

        assert_eq!(Some(EgonType::number()), result);
    }

    #[test]
    fn get_undefined_const_returns_none() {
        let env = TypeEnv::new();

        assert_eq!(None, env.get_const("a"));
    }

    #[test]
    fn type_alias_returns_aliased_type() {
        let mut env = TypeEnv::new();

        // Alias type `Int` to type `number`
        env.set_type_alias(
            EgonType::Unbound(UnboundType::new("Int")),
            EgonType::typed(EgonTypeParam::Bound(BoundType::number())).into(),
        );

        assert_eq!(
            Some(EgonType::typed(EgonTypeParam::Bound(BoundType::number())).into()),
            env.get_type_alias(EgonType::Unbound(UnboundType::new("Int")))
        );
    }

    #[test]
    fn type_alias_returns_aliased_type_2() {
        let mut env = TypeEnv::new();

        // Alias type `Int` to type `number`
        env.set_type_alias(
            EgonType::Unbound(UnboundType::new("Int")),
            EgonType::typed(EgonTypeParam::Bound(BoundType::number())),
        );

        // Alias type `Int2` to type `Int`
        env.set_type_alias(
            EgonType::Unbound(UnboundType::new("Int2")),
            EgonType::typed(EgonTypeParam::Bound(BoundType::new("Int"))),
        );

        // Alias type `Int2` resolves to the root type of `number`
        // `Int2` -> `Int` -> `number`
        assert_eq!(
            Some(EgonType::number()),
            env.get_type_alias(EgonType::Unbound(UnboundType::new("Int2")))
        );
    }

    #[test]
    fn override_type_in_new_scope() {
        let mut env = TypeEnv::new();

        env.set_variable("a", EgonType::number());

        let t = env.get_variable("a");
        assert_eq!(Some(EgonType::number()), t);

        env.start_scope();

        env.set_variable("a", EgonType::bool());

        let t = env.get_variable("a");
        assert_eq!(Some(EgonType::bool()), t);

        let _ = env.end_scope();

        let t = env.get_variable("a");
        assert_eq!(Some(EgonType::number()), t);
    }
}
