use std::{collections::HashMap, fmt};

use egonlang_errors::EgonError;
use serde::{Deserialize, Serialize};
use tracelog::{log_identifier, log_type};

use crate::Type;

/// Stores type information for constants, variables, and type aliases in a scope
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize, Default)]
struct Scope {
    pub variables: HashMap<String, Type>,
    pub constants: HashMap<String, Type>,
    pub type_aliases: HashMap<Type, Type>,
}

impl Scope {
    /// Search the [`Scope`] for a variable's [`Type`] by identifier name
    pub fn get_variable(&self, identifier: &str) -> Option<&Type> {
        self.variables.get(identifier)
    }

    /// Declare or set a variable with a [`Type`]
    ///
    /// This will fail if a constant is already defined or reassigned value's type mismatches
    pub fn set_variable(&mut self, key: &str, value: Type) -> Result<(), EgonError> {
        self.variables.insert(key.to_owned(), value);

        Ok(())
    }

    /// Search the [`Scope`] for a constant's type by identifier name
    pub fn get_const(&self, key: &str) -> Option<&Type> {
        self.constants.get(key)
    }

    /// Declare or set a constant with a [`Type`]
    ///
    /// This will fail if a constant or variable is already defined
    pub fn set_const(&mut self, key: &str, value: Type) -> Result<(), EgonError> {
        self.constants.insert(key.to_owned(), value);

        Ok(())
    }

    /// Search the [`Scope`] for a type alias
    pub fn get_type_alias(&self, key: &Type) -> Option<&Type> {
        self.type_aliases.get(key)
    }

    /// Declare a [`Type`] alias
    ///
    /// This will fail if a constant or variable is already defined
    pub fn set_type_alias(&mut self, key: Type, value: Type) -> Result<(), EgonError> {
        self.type_aliases.insert(key, value);

        Ok(())
    }
}

/// Returned from [`TypeEnv`] method `get_by_identifier`
#[derive(Debug)]
pub struct TypeEnvGetByIdent {
    /// Identifier's type
    pub of_type: Type,
    /// If the identifier's is bound to a constant vlue
    pub is_const: bool,
}

/// Store and retreive type information for string identifiers
#[derive(Default, Debug, PartialEq)]
pub struct TypeEnv {
    /// Scope stack for the type environment
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

    /// Search through the [`Scope`] stack to find a constant or variable [`Type`] by name
    pub fn get_by_identifier(&self, name: &str) -> Option<TypeEnvGetByIdent> {
        tracelog::tracelog!(
            label = type_env,get_variable;
            "(level: {}) Looking up constant or variable by identifier {}",
            self.get_scope_depth(),
            log_identifier(name)
        );

        let mut of_type: Option<&Type> = None;
        let mut is_const = true;

        for scope in self.scopes.iter().rev() {
            // TODO: Should be reversed?
            if let Some(scope_value) =
                scope
                    .get_const(name)
                    .or_else(|| match self.get_variable(name) {
                        Some(t) => {
                            is_const = false;
                            Some(t)
                        }
                        None => None,
                    })
            {
                of_type = Some(scope_value);
                break;
            }
        }

        of_type
            .cloned()
            .map(|of_type| TypeEnvGetByIdent { of_type, is_const })
    }

    /// Search through the [`Scope`] stack to find a variable [`Type`] by name
    pub fn get_variable(&self, name: &str) -> Option<&Type> {
        tracelog::tracelog!(
            label = type_env,get_variable;
            "(level: {}) Looking up variable {}",
            self.get_scope_depth(),
            log_identifier(name)
        );

        let mut value: Option<&Type> = None;

        for scope in &self.scopes {
            // TODO: Should be reversed?
            if let Some(scope_value) = scope.get_variable(name) {
                value = Some(scope_value);
            }
        }

        value
    }

    /// Set a variable [`Type`] by name in the current [`Scope`]
    pub fn set_variable(&mut self, name: &str, value: Type) -> Result<(), EgonError> {
        tracelog::tracelog!(
            label = type_env,set_variable;
            "(level: {}) Setting variable {} with type {}",
            self.get_scope_depth(),
            log_identifier(name),
            log_type(&value.to_string())
        );

        if self.get_type_alias(&Type::new(name)).is_some() {
            tracelog::tracelog!(
                level = ERROR;
                label = type_env,set_variable;
                "(level: {}) Unable to set variable. Type alias {} already exists",
                self.get_scope_depth(),
                log_identifier(name)
            );

            return Err(EgonError::TypeError(
                egonlang_errors::EgonTypeError::AlreadyDefined(name.to_string()),
            ));
        }

        if self.get_constant(name).is_some() {
            tracelog::tracelog!(
                level = ERROR;
                label = type_env,set_variable;
                "(level: {}) Unable to set variable. Constant {} already exists",
                self.get_scope_depth(),
                log_identifier(name)
            );

            return Err(EgonError::TypeError(
                egonlang_errors::EgonTypeError::AlreadyDefined(name.to_string()),
            ));
        }

        let scope = self.get_mut_scope().expect("scope should be avaliable");
        scope.set_variable(name, value)?;

        Ok(())
    }

    /// Search through the [`Scope`] stack to find a constant [`Type`] by name
    pub fn get_constant(&self, name: &str) -> Option<&Type> {
        tracelog::tracelog!(
            label = type_env,get_constant;
            "(level: {}) Looking up constant {}",
            self.get_scope_depth(),
            log_identifier(name)
        );

        let mut value: Option<&Type> = None;

        for scope in &self.scopes {
            // TODO: Should be reversed?
            if let Some(scope_value) = scope.get_const(name) {
                value = Some(scope_value);
            }
        }

        value
    }

    /// Set a constant [`Type`] by name in the current [`Scope`]
    pub fn set_const(&mut self, name: &str, value: Type) -> Result<(), EgonError> {
        tracelog::tracelog!(
            label = type_env,set_const;
            "(level: {}) Setting constant {} with type {}",
            self.get_scope_depth(),
            log_identifier(name),
            log_type(&value.to_string())
        );

        if self.get_type_alias(&Type::new(name)).is_some() {
            tracelog::tracelog!(
                level = ERROR;
                label = type_env,set_const;
                "(level: {}) Unable to set constant. Type alias {} already exists",
                self.get_scope_depth(),
                log_identifier(name)
            );

            return Err(EgonError::TypeError(
                egonlang_errors::EgonTypeError::AlreadyDefined(name.to_string()),
            ));
        }

        if self.get_constant(name).is_some() {
            tracelog::tracelog!(
                level = ERROR;
                label = type_env,set_const;
                "(level: {}) Unable to set constant. Constant {} already exists",
                self.get_scope_depth(),
                log_identifier(name)
            );

            return Err(EgonError::TypeError(
                egonlang_errors::EgonTypeError::AlreadyDefined(name.to_string()),
            ));
        }

        if self.get_variable(name).is_some() {
            tracelog::tracelog!(
                level = ERROR;
                label = type_env,set_const;
                "(level: {}) Unable to set constant. Variable {} already exists",
                self.get_scope_depth(),
                log_identifier(name)
            );

            return Err(EgonError::TypeError(
                egonlang_errors::EgonTypeError::AlreadyDefined(name.to_string()),
            ));
        }

        let scope = self.get_mut_scope().expect("scope should be avaliable");
        scope.set_const(name, value)?;

        Ok(())
    }

    /// Search through the [`Scope`] stack to find an aliased [`Type`] by [`Type`]
    pub fn get_type_alias(&self, alias: &Type) -> Option<&Type> {
        tracelog::tracelog!(
            label = type_env,get_type_alias;
            "(level: {}) Looking up type alias {}",
            self.get_scope_depth(),
            log_type(&alias.to_string())
        );

        let mut value: Option<&Type> = None;

        for scope in &self.scopes {
            if let Some(scope_value) = scope.get_type_alias(alias) {
                value = Some(scope_value);
            }
        }

        value
    }

    /// Set a [`Type`] alias in the current [`Scope`]
    pub fn set_type_alias(&mut self, key: Type, value: Type) -> Result<(), EgonError> {
        let scope_depth = self.get_scope_depth();

        if self.get_type_alias(&key).is_some() {
            tracelog::tracelog!(
                level = ERROR;
                label = type_env,set_type_alias;
                "(level: {}) Unable to set type alias. Type alias {} already exists",
                self.get_scope_depth(),
                log_type(&key)
            );

            return Err(EgonError::TypeError(
                egonlang_errors::EgonTypeError::AlreadyDefined(key.to_string()),
            ));
        }

        if self.get_constant(&key.name).is_some() {
            tracelog::tracelog!(
                level = ERROR;
                label = type_env,set_type_alias;
                "(level: {}) Unable to set type alias. Constant {} already exists",
                self.get_scope_depth(),
                log_identifier(&key.name)
            );

            return Err(EgonError::TypeError(
                egonlang_errors::EgonTypeError::AlreadyDefined(key.to_string()),
            ));
        }

        if self.get_variable(&key.name).is_some() {
            tracelog::tracelog!(
                level = ERROR;
                label = type_env,set_type_alias;
                "(level: {}) Unable to set type alias. Variable {} already exists",
                self.get_scope_depth(),
                log_identifier(&key.name)
            );

            return Err(EgonError::TypeError(
                egonlang_errors::EgonTypeError::AlreadyDefined(key.to_string()),
            ));
        }

        let new_alias = self.get_type_alias(&value).cloned().unwrap_or(value);

        tracelog::tracelog!(
            label = type_env,set_type_alias;
            "(level: {}) Creating type alias of {} with type {}",
            scope_depth,
            log_type(&key),
            log_type(&new_alias)
        );

        let scope = self.get_mut_scope().expect("scope should be avaliable");
        let _ = scope.set_type_alias(key, new_alias);

        Ok(())
    }

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
        egon_bool, egon_number,
        type_env::{RootScopeEndedError, TypeEnv},
        Type,
    };

    #[test]
    fn set_then_get_const_returns_some() {
        let mut env = TypeEnv::new();

        let _ = env.set_const("a", Type::number());

        assert_eq!(Some(&Type::number()), env.get_constant("a"));
    }

    #[test]
    fn set_then_get_const_in_new_scope_returns_some() {
        let mut env = TypeEnv::new();

        let _ = env.set_const("a", Type::number());

        env.start_scope();

        assert_eq!(Some(&Type::number()), env.get_constant("a"));
    }

    #[test]
    fn set_in_new_scope_then_get_const_in_old_scope_returns_none() {
        let mut env = TypeEnv::new();

        env.start_scope();
        let _ = env.set_const("a", Type::number());
        let _ = env.end_scope();

        assert_eq!(None, env.get_constant("a"));
    }

    #[test]
    fn set_const_then_get_variable_returns_none() {
        let mut env = TypeEnv::new();

        let _ = env.set_const("a", Type::number());

        assert_eq!(None, env.get_variable("a"));
    }

    #[test]
    fn set_then_get_variable_returns_some() {
        let mut env = TypeEnv::new();

        let _ = env.set_variable("a", Type::number());

        assert_eq!(Some(&Type::number()), env.get_variable("a"));
    }

    #[test]
    fn set_then_get_variable_in_new_scope_returns_some() {
        let mut env = TypeEnv::new();

        let _ = env.set_variable("a", Type::number());

        env.start_scope();

        assert_eq!(Some(&Type::number()), env.get_variable("a"));
    }

    #[test]
    fn set_in_new_scope_then_get_variable_in_old_scope_returns_none() {
        let mut env = TypeEnv::new();

        env.start_scope();
        let _ = env.set_variable("a", Type::number());
        let _ = env.end_scope();

        assert_eq!(None, env.get_variable("a"));
    }

    #[test]
    fn set_variable_and_get_const_returns_none() {
        let mut env = TypeEnv::new();

        let _ = env.set_variable("a", Type::number());

        assert_eq!(None, env.get_constant("a"));
    }

    #[test]
    fn set_then_get_type_alias_returns_some() {
        let mut env = TypeEnv::new();

        let _ = env.set_type_alias(Type::new("Int"), Type::number());

        assert_eq!(Some(&Type::number()), env.get_type_alias(&Type::new("Int")));
    }

    #[test]
    fn set_then_get_type_alias_in_new_scope_returns_some() {
        let mut env = TypeEnv::new();

        let _ = env.set_type_alias(Type::new("Int"), Type::number());

        env.start_scope();

        assert_eq!(Some(&Type::number()), env.get_type_alias(&Type::new("Int")));
    }

    #[test]
    fn set_in_new_scope_then_get_type_alias_in_old_scope_returns_none() {
        let mut env = TypeEnv::new();

        env.start_scope();
        let _ = env.set_type_alias(Type::new("Int"), Type::number());
        let _ = env.end_scope();

        assert_eq!(None, env.get_type_alias(&Type::new("Int")));
    }

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

    #[test]
    fn set_variable_already_defined_type_alias_returns_err() {
        let mut env = TypeEnv::new();

        // Alias type `Int` to type `number`
        let _ = env.set_type_alias(Type::new("A"), Type::number());

        assert_eq!(
            Err(EgonError::TypeError(
                egonlang_errors::EgonTypeError::AlreadyDefined("A".to_string())
            )),
            env.set_variable("A", Type::number())
        );
    }

    #[test]
    fn set_const_already_defined_type_alias_returns_err() {
        let mut env = TypeEnv::new();

        // Alias type `Int` to type `number`
        let _ = env.set_type_alias(Type::new("A"), Type::number());

        assert_eq!(
            Err(EgonError::TypeError(
                egonlang_errors::EgonTypeError::AlreadyDefined("A".to_string())
            )),
            env.set_const("A", Type::number())
        );
    }

    #[test]
    fn set_type_alias_already_defined_variable_returns_err() {
        let mut env = TypeEnv::new();

        // Alias type `Int` to type `number`
        let _ = env.set_variable("Int", Type::number());

        assert_eq!(
            Err(EgonError::TypeError(
                egonlang_errors::EgonTypeError::AlreadyDefined("Int".to_string())
            )),
            env.set_type_alias(Type::new("Int"), Type::number())
        );
    }

    #[test]
    fn set_type_alias_already_defined_const_returns_err() {
        let mut env = TypeEnv::new();

        // Alias type `Int` to type `number`
        let _ = env.set_const("Int", Type::number());

        assert_eq!(
            Err(EgonError::TypeError(
                egonlang_errors::EgonTypeError::AlreadyDefined("Int".to_string())
            )),
            env.set_type_alias(Type::new("Int"), Type::number())
        );
    }

    #[test]
    fn set_type_alias_already_defined_returns_err() {
        let mut env = TypeEnv::new();

        // Alias type `Int` to type `number`
        let _ = env.set_type_alias(Type::new("Int"), Type::number());

        assert_eq!(
            Err(EgonError::TypeError(
                egonlang_errors::EgonTypeError::AlreadyDefined("Int".to_string())
            )),
            env.set_type_alias(Type::new("Int"), Type::number())
        );
    }

    #[test]
    fn set_const_in_new_scope_set_const_returns_err() {
        let mut env = TypeEnv::new();

        let _ = env.set_const("a", Type::number());

        env.start_scope();

        assert_eq!(
            Err(EgonError::TypeError(
                egonlang_errors::EgonTypeError::AlreadyDefined("a".to_owned())
            )),
            env.set_const("a", Type::string())
        );
    }

    #[test]
    fn set_variable_in_new_scope_set_const_returns_err() {
        let mut env = TypeEnv::new();

        let _ = env.set_const("a", Type::number());

        env.start_scope();

        assert_eq!(
            Err(EgonError::TypeError(
                egonlang_errors::EgonTypeError::AlreadyDefined("a".to_owned())
            )),
            env.set_variable("a", Type::string())
        );
    }

    #[test]
    fn set_type_alias_in_new_scope_with_already_defined_const_returns_err() {
        let mut env = TypeEnv::new();

        // Alias type `Int` to type `number`
        let _ = env.set_const("Int", Type::number());

        env.start_scope();

        assert_eq!(
            Err(EgonError::TypeError(
                egonlang_errors::EgonTypeError::AlreadyDefined("Int".to_string())
            )),
            env.set_type_alias(Type::new("Int"), Type::number())
        );
    }

    #[test]
    fn set_type_alias_in_new_scope_with_already_defined_variable_returns_err() {
        let mut env = TypeEnv::new();

        // Alias type `Int` to type `number`
        let _ = env.set_variable("Int", Type::number());

        env.start_scope();

        assert_eq!(
            Err(EgonError::TypeError(
                egonlang_errors::EgonTypeError::AlreadyDefined("Int".to_string())
            )),
            env.set_type_alias(Type::new("Int"), Type::number())
        );
    }

    #[test]
    fn set_type_alias_in_new_scope_with_already_defined_type_alias_returns_err() {
        let mut env = TypeEnv::new();

        // Alias type `Int` to type `number`
        let _ = env.set_type_alias(Type::new("Int"), Type::number());

        env.start_scope();

        assert_eq!(
            Err(EgonError::TypeError(
                egonlang_errors::EgonTypeError::AlreadyDefined("Int".to_string())
            )),
            env.set_type_alias(Type::new("Int"), Type::number())
        );
    }

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
    fn type_alias_returns_aliased_type() {
        let mut env = TypeEnv::new();

        // Alias type `Int` to type `number`
        let _ = env.set_type_alias(Type::new("Int"), Type::number());

        assert_eq!(Some(&Type::number()), env.get_type_alias(&Type::new("Int")));
    }

    #[test]
    fn type_alias_returns_aliased_type_2() {
        let mut env = TypeEnv::new();

        // Alias type `Int` to type `number`
        let _ = env.set_type_alias(Type::new("Int"), Type::number());

        // Alias type `Int2` to type `Int`
        // This sets `Int2` to alias `number` under the surface
        let _ = env.set_type_alias(Type::new("Int2"), Type::new("Int"));

        // Alias type `Int2` resolves to the root type of `number`
        // `Int2` -> `Int` -> `number`
        assert_eq!(
            Some(&Type::number()),
            env.get_type_alias(&Type::new("Int2"))
        );
    }

    #[test]
    fn override_type_in_new_scope() {
        let mut env = TypeEnv::new();

        let _ = env.set_variable("a", egon_number!());

        let t = env.get_variable("a");
        assert_eq!(Some(&egon_number!()), t);

        env.start_scope();

        let _ = env.set_variable("a", egon_bool!());

        let t = env.get_variable("a");
        assert_eq!(Some(&egon_bool!()), t);

        let _ = env.end_scope();

        let t = env.get_variable("a");
        assert_eq!(Some(&egon_number!()), t);
    }
}
