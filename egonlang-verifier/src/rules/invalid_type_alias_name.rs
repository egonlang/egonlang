use egonlang_core::{ast::Stmt, errors::SyntaxError, span::Span};
use regex::Regex;

use crate::prelude::*;

stmt_rule!(
    InvalidTypeAliasNameRule,
    fn (stmt: &Stmt, span: &Span, types: &mut TypeEnv) {
        let mut errs = vec![];

        if let Stmt::Assign(assign_stmt) = &stmt {
            if let Some((value_expr, value_span)) = &assign_stmt.value {
                let value_typeref = types.resolve_expr_type(value_expr, value_span).unwrap();

                if value_typeref.is_type() {
                    let name = &assign_stmt.identifier.name;
                    let pattern = Regex::new("^[A-Z][A-Za-z0-9]*$").unwrap();

                    if !pattern.is_match(name) {
                        errs.push((
                            SyntaxError::InvalidTypeAlias {
                                name: name.to_string(),
                            }
                            .into(),
                            span.clone(),
                        ));
                    }
                }
            }
        }

        errs
    }
);
