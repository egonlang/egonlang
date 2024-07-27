use std::collections::HashMap;

use egonlang_core::prelude::*;

use crate::verify_trace;

/// Typing information stored about an identifier
#[derive(Debug, PartialEq, Clone)]
pub struct TypeEnvValue {
    pub typeref: ast::TypeRef,
    pub is_const: bool,
}

impl TypeEnvValue {
    fn map_typeref(&self, typeref: ast::TypeRef) -> TypeEnvValue {
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

    pub fn level(&self) -> usize {
        match self.root {
            Some(root_type_env) => root_type_env.level() + 1,
            None => 0,
        }
    }

    /// Create a new type environment by extending the current one
    pub fn extend(&self) -> Box<TypeEnv> {
        let extended_type_env = TypeEnv {
            root: Some(self),
            values: HashMap::new(),
        };

        verify_trace!(type_env extend: "(level: {}) Extending type env... New level: {}", self.level(), extended_type_env.level());

        Box::from(extended_type_env)
    }

    /// Attempt to resolve an identifier's type
    pub fn get(&self, identifier: &str) -> Option<TypeEnvValue> {
        verify_trace!(
            type_env get: "(level:{}) Looking up identifier {}",
            self.level(),
            identifier.cyan()
        );

        let result = match self.values.get(identifier) {
            Some(result) => Some(result.clone()),
            None => match &self.root {
                Some(root) => {
                    verify_trace!(
                        type_env get error: "(level:{}) Not finding {}, looking in higher type env",
                        self.level(),
                        identifier.cyan()
                    );
                    root.get(identifier)
                }
                _ => None,
            },
        };

        if result.is_some() {
            let result = result.clone().unwrap();

            if result.typeref.0 == *"type" {
                verify_trace!(
                    "(level:{}) Got type alias {} for {}",
                    self.level(),
                    result.typeref.to_string().italic().yellow(),
                    identifier.cyan()
                );

                return Some(result.map_typeref(result.typeref.1.first().unwrap().clone()));
            }
        }

        match &result {
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
                    "(level:{}) Unable to find type for {} in type env",
                    self.level(),
                    identifier.cyan()
                );
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
                    verify_trace!(
                        type_env set:
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

    /// Resolve an expression's type recursively.
    /// This will resolve value identifiers types as well.
    ///
    /// Example
    ///
    /// let a = 123;
    ///
    /// let b = false;
    ///
    /// (a, b,); // This expression's type resolves to (number, bool,)
    pub fn resolve_expr_type(
        &self,
        expr: &ast::Expr,
        span: &Span,
    ) -> Result<ast::TypeRef, Vec<EgonErrorS>> {
        verify_trace!(
            type_env resolve_expr_type: "(level: {}) Resolving expression's type: {}",
            self.level(),
            expr.to_string().cyan()
        );

        let resolved_type = match expr {
            ast::Expr::Identifier(ident_expr) => self.resolve_identifier_type(ident_expr, span),
            ast::Expr::List(list_expr) => self.resolve_list_type(list_expr),
            ast::Expr::Tuple(tuple_expr) => self.resolve_tuple_type(tuple_expr),
            ast::Expr::Block(block_expr) => {
                verify_trace!(
                    type_env resolve_expr_type: "(level: {}) Resolving type for block {}",
                    self.level(),
                    block_expr.to_string().cyan()
                );

                let block_expr_return_expr = block_expr.return_expr.clone();

                if let Some((block_expr_return_expr, block_expr_return_span)) =
                    block_expr_return_expr
                {
                    let resolved_type =
                        self.resolve_expr_type(&block_expr_return_expr, &block_expr_return_span);

                    verify_trace!(
                        type_env resolve_expr_type block: "(level: {}) Resolved block expression {} to type",
                        self.level(),
                        block_expr.to_string().cyan()
                    );

                    resolved_type
                } else {
                    verify_trace!(
                        type_env resolve_expr_type block: "(level: {}) Resolved block expression {} to type {}",
                        self.level(),
                        block_expr.to_string().cyan(),
                        ast::TypeRef::unit().to_string().italic().yellow()
                    );

                    return Ok(ast::TypeRef::unit());
                }
            }
            ast::Expr::Type(type_expr) => {
                verify_trace!(
                    type_env resolve_expr_type: "(level: {}) Resolving type expression {}",
                    self.level(),
                    type_expr.to_string().cyan()
                );

                let type_string = type_expr.0.to_string();
                if let Some(type_env_value) = &self.get(type_string.as_str()) {
                    verify_trace!(
                        type_env resolve_expr_type: "(level: {}) Resolved type expression {} to type {}",
                        self.level(),
                        type_expr.to_string().cyan(),
                        type_env_value.typeref.to_string().italic().yellow()
                    );

                    return Ok(type_env_value.typeref.clone());
                }

                verify_trace!(
                    type_env resolve_expr_type: "(level: {}) Resolved type expression {} to type {}",
                    self.level(),
                    type_expr.to_string().cyan(),
                    type_expr.0.to_string().italic().yellow()
                );

                Ok(type_expr.0.clone())
            }
            ast::Expr::Assign(assign_expr) => {
                let name = &assign_expr.identifier.name;

                Ok(self.get(name).map(|x| x.typeref).unwrap_or_else(|| {
                    let (value_expr, value_span) = &assign_expr.value;

                    self.resolve_expr_type(value_expr, value_span).unwrap()
                }))
            }
            ast::Expr::If(if_expr) => {
                let (then_expr, then_span) = &if_expr.then;
                let then_typeref = self.resolve_expr_type(then_expr, then_span)?;

                Ok(then_typeref)
            }
            _ => Ok(expr.clone().get_type_expr()),
        };

        match &resolved_type {
            Ok(resolved_type) => {
                let resolved_type_string = format!("{resolved_type}");
                verify_trace!(
                    type_env resolve_expr_type:
                    "(level: {}) Resolved expression {} to the type {}",
                    self.level(),
                    expr.to_string().cyan(),
                    resolved_type_string.italic().yellow()
                );
            }
            Err(err) => {
                let err_string = err
                    .iter()
                    .map(|(e, e_span)| format!("{e} @ {e_span:?}"))
                    .collect::<Vec<String>>()
                    .join("; ");

                verify_trace!(
                    type_env resolve_expr_type error: "(level: {}) Unable to resolve {} to a type: {}",
                    self.level(),
                    expr.to_string().cyan(),
                    err_string.italic().red()
                );
            }
        };

        resolved_type
    }

    fn resolve_identifier_type(
        &self,
        ident_expr: &ast::ExprIdentifier,
        span: &Span,
    ) -> Result<ast::TypeRef, Vec<EgonErrorS>> {
        let name = &ident_expr.identifier.name;

        verify_trace!(
            type_env resolve_identifier_type: "(level: {}) Resolving type for identifier {}",
            self.level(),
            ident_expr.to_string().cyan()
        );

        match self.get(name) {
            Some(env_var) => {
                verify_trace!(
                    type_env resolve_identifier_type: "(level: {}) Resolved type for identifier {} to type {}",
                    self.level(),
                    ident_expr.to_string().cyan(),
                    env_var.typeref.to_string().italic().yellow()
                );

                Ok(env_var.typeref)
            }
            None => {
                verify_trace!(
                    type_env resolve_identifier_type error: "(level: {}) Unable to resolve type for identifier {}",
                    self.level(),
                    ident_expr.to_string().cyan()
                );

                Err(vec![(
                    EgonTypeError::Undefined(name.to_string()).into(),
                    span.clone(),
                )])
            }
        }
    }

    fn resolve_list_type(
        &self,
        list_expr: &ast::ExprList,
    ) -> Result<ast::TypeRef, Vec<EgonErrorS>> {
        verify_trace!(
            type_env resolve_list_type: "(level: {}) Resolve type for list {}",
            self.level(),
            list_expr.to_string().cyan()
        );

        if list_expr.items.is_empty() {
            verify_trace!(
                type_env resolve_list_type: "(level: {}) Resolved type for list {} to type {}",
                self.level(),
                list_expr.to_string().cyan(),
                ast::TypeRef::list(ast::TypeRef::unknown()).to_string().italic().yellow()
            );

            return Ok(ast::TypeRef::list(ast::TypeRef::unknown()));
        }

        let (first_item_expr, first_item_span) = list_expr.items.first().unwrap().clone();
        let first_item_type = self.resolve_expr_type(&first_item_expr, &first_item_span)?;

        verify_trace!(
            type_env resolve_list_type: "(level: {}) Resolved type for identifier {} to type {}",
            self.level(),
            list_expr.to_string().cyan(),
            first_item_type.to_string().italic().yellow()
        );

        Ok(ast::TypeRef::list(first_item_type))
    }

    fn resolve_tuple_type(
        &self,
        tuple_expr: &ast::ExprTuple,
    ) -> Result<ast::TypeRef, Vec<EgonErrorS>> {
        verify_trace!(
            type_env resolve_tuple_type: "(level: {}) Resolve type for tuple {}",
            self.level(),
            tuple_expr.to_string().cyan()
        );

        if tuple_expr.items.is_empty() {
            verify_trace!(
                type_env resolve_tuple_type: "(level: {}) Resolved type for tuple {} to type {}",
                self.level(),
                tuple_expr.to_string().cyan(),
                ast::TypeRef::tuple(vec![]).to_string().italic().yellow()
            );

            return Ok(ast::TypeRef::tuple(vec![]));
        }

        let item_types: Vec<ast::TypeRef> = tuple_expr
            .items
            .clone()
            .into_iter()
            .map(|(x_expr, x_span)| self.resolve_expr_type(&x_expr, &x_span).unwrap())
            .collect();

        verify_trace!(
            type_env resolve_tuple_type: "(level: {}) Resolved type for tuple {} to type {}",
            self.level(),
            tuple_expr.to_string().cyan(),
            ast::TypeRef::tuple(item_types.clone()).to_string().italic().yellow()
        );

        Ok(ast::TypeRef::tuple(item_types))
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
