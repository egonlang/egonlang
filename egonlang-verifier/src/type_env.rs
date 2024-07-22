use std::collections::HashMap;

use egonlang_core::{
    ast::{Expr, ExprIdentifier, ExprList, ExprTuple, TypeRef},
    errors::{ErrorS, TypeError},
    span::Span,
};

use crate::verify_trace;

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
    pub fn extend(&self) -> Box<TypeEnv> {
        verify_trace!("Extending type env...");

        Box::from(TypeEnv {
            root: Some(self),
            values: HashMap::new(),
        })
    }

    /// Attempt to resolve an identifier's type
    pub fn get(&self, identifier: &str) -> Option<TypeEnvValue> {
        let result = match self.values.get(identifier) {
            Some(result) => Some(result.clone()),
            None => match &self.root {
                Some(root) => {
                    verify_trace!(
                        "Not finding {}, looking in higher type env",
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
                    "Got type alias {} for {}",
                    result.typeref.to_string().italic().yellow(),
                    identifier.cyan()
                );

                return Some(result.map_typeref(result.typeref.1.first().unwrap().clone()));
            }
        }

        match &result {
            Some(result) => {
                verify_trace!(
                    "Got type {} for {}",
                    result.typeref.to_string().italic().yellow(),
                    identifier.cyan()
                );
            }
            None => {
                verify_trace!(error:
                    "Unable to find type for {} in type env",
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
                        "Setting {} to type alias {}",
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
                        "Setting {} to the type alias {}",
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
            "Setting {} to the type {}",
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
    pub fn resolve_expr_type(&self, expr: &Expr, span: &Span) -> Result<TypeRef, Vec<ErrorS>> {
        let resolved_type = match expr {
            Expr::Identifier(ident_expr) => self.resolve_identifier_type(ident_expr, span),
            Expr::List(list_expr) => self.resolve_list_type(list_expr),
            Expr::Tuple(tuple_expr) => self.resolve_tuple_type(tuple_expr),
            Expr::Block(block_expr) => {
                let a = block_expr.return_expr.clone();

                if let Some((a, a_span)) = a {
                    let result = self.resolve_expr_type(&a, &a_span);

                    return result;
                } else {
                    return Ok(TypeRef::unit());
                }
            }
            Expr::Type(type_expr) => {
                let type_string = type_expr.0.to_string();
                let a = self.get(type_string.as_str());

                if let Some(a) = &a {
                    return Ok(a.typeref.clone());
                }

                Ok(type_expr.0.clone())
            }
            Expr::Assign(assign_expr) => {
                let name = &assign_expr.identifier.name;

                Ok(self.get(name).map(|x| x.typeref).unwrap_or_else(|| {
                    let (value_expr, value_span) = &assign_expr.value;

                    self.resolve_expr_type(value_expr, value_span).unwrap()
                }))
            }
            Expr::If(if_expr) => {
                let (then_expr, then_span) = &if_expr.then;
                let then_typeref = self.resolve_expr_type(then_expr, then_span)?;

                Ok(then_typeref)
            }
            _ => Ok(expr.clone().get_type_expr()),
        };

        match &resolved_type {
            Ok(resolved_type) => {
                let resolved_type_string = format!("{resolved_type}");
                verify_trace!(resolve_type:
                    "{} to the type {}",
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
                    "Error resolving {} to a type: {}",
                    expr.to_string().cyan(),
                    err_string.italic().red()
                );
            }
        };

        resolved_type
    }

    fn resolve_identifier_type(
        &self,
        ident_expr: &ExprIdentifier,
        span: &Span,
    ) -> Result<TypeRef, Vec<ErrorS>> {
        let name = &ident_expr.identifier.name;

        match self.get(name) {
            Some(env_var) => Ok(env_var.typeref),
            None => Err(vec![(
                TypeError::Undefined(name.to_string()).into(),
                span.clone(),
            )]),
        }
    }

    fn resolve_list_type(&self, list_expr: &ExprList) -> Result<TypeRef, Vec<ErrorS>> {
        if list_expr.items.is_empty() {
            return Ok(TypeRef::list(TypeRef::unknown()));
        }

        let (first_item_expr, first_item_span) = list_expr.items.first().unwrap().clone();
        let first_item_type = self.resolve_expr_type(&first_item_expr, &first_item_span)?;

        Ok(TypeRef::list(first_item_type))
    }

    fn resolve_tuple_type(&self, tuple_expr: &ExprTuple) -> Result<TypeRef, Vec<ErrorS>> {
        if tuple_expr.items.is_empty() {
            return Ok(TypeRef::list(TypeRef::unknown()));
        }

        let item_types = tuple_expr
            .items
            .clone()
            .into_iter()
            .map(|(x_expr, x_span)| self.resolve_expr_type(&x_expr, &x_span).unwrap())
            .collect();

        Ok(TypeRef::tuple(item_types))
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
