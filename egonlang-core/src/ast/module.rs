use serde::{Deserialize, Serialize};

use super::StmtS;

/// A collection of [`Stmt`] representing an Egon code file.
#[derive(Debug, Default, PartialEq, Serialize, Deserialize)]
pub struct Module {
    pub stmts: Vec<StmtS>,
}

impl Module {
    pub fn new() -> Self {
        Module::default()
    }

    pub fn from(stmts: Vec<StmtS>) -> Self {
        Module { stmts }
    }

    /// Get AST nodes by span index
    pub fn get_by_index(&self, idx: usize) -> Vec<AstNode> {
        let mut nodes = vec![];

        for (stmt, span) in &self.stmts {
            if span.contains(&idx) {
                nodes.push(AstNode::Stmt(stmt));

                match stmt {
                    super::Stmt::Expr(stmt_expr) => {
                        if stmt_expr.expr.1.contains(&idx) {
                            nodes.push(AstNode::Expr(&stmt_expr.expr.0));
                        }
                    }
                    super::Stmt::Assign(stmt_assign) => {
                        nodes.push(AstNode::Identifier(&stmt_assign.identifier));

                        if let Some(x) = &stmt_assign.value {
                            if x.1.contains(&idx) {
                                nodes.push(AstNode::Expr(&x.0));
                            }
                        };

                        if let Some(x) = &stmt_assign.type_expr {
                            if x.1.contains(&idx) {
                                nodes.push(AstNode::Expr(&x.0));
                            }
                        };
                    }
                    super::Stmt::TypeAlias(_) => todo!(),
                    super::Stmt::Fn(_) => todo!(),
                    super::Stmt::AssertType(_) => todo!(),
                    super::Stmt::Return(_) => todo!(),
                    super::Stmt::Error => todo!(),
                };
            }
        }

        nodes
    }
}

/// AST nodes contained in the [`Module`]
#[derive(Debug, PartialEq)]
pub enum AstNode<'a> {
    Stmt(&'a super::Stmt),
    Expr(&'a super::Expr),
    Identifier(&'a super::Identifier),
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::{self, module::AstNode, StmtAssign, TypeRef},
        parser::parse,
    };
    use pretty_assertions::assert_eq;

    #[test]
    fn module_get_ast_nodes_by_index_let_decl_identifier() {
        let source = "let a;";
        let module = parse(source, 0).unwrap();

        let nodes = module.get_by_index(4);

        assert_eq!(
            vec![
                AstNode::Stmt(&ast::Stmt::Assign(StmtAssign {
                    identifier: ast::Identifier {
                        name: "a".to_string()
                    },
                    type_expr: None,
                    is_const: false,
                    value: None
                })),
                AstNode::Identifier(&ast::Identifier {
                    name: "a".to_string()
                })
            ],
            nodes
        );
    }

    #[test]
    fn module_get_ast_nodes_by_index_let_decl_identifier_b() {
        let source = "let a = 123;";
        let module = parse(source, 0).unwrap();

        let nodes = module.get_by_index(4);

        assert_eq!(
            vec![
                AstNode::Stmt(&ast::Stmt::Assign(StmtAssign {
                    identifier: ast::Identifier {
                        name: "a".to_string()
                    },
                    type_expr: None,
                    is_const: false,
                    value: Some((123f64.into(), 8..11))
                })),
                AstNode::Identifier(&ast::Identifier {
                    name: "a".to_string()
                })
            ],
            nodes
        );
    }

    #[test]
    fn module_get_ast_nodes_by_index_let_decl_value() {
        let source = "let a = 123;";
        let module = parse(source, 0).unwrap();

        let nodes = module.get_by_index(9);

        assert_eq!(
            vec![
                AstNode::Stmt(&ast::Stmt::Assign(StmtAssign {
                    identifier: ast::Identifier {
                        name: "a".to_string()
                    },
                    type_expr: None,
                    is_const: false,
                    value: Some((123f64.into(), 8..11))
                })),
                AstNode::Identifier(&ast::Identifier {
                    name: "a".to_string()
                }),
                AstNode::Expr(&123f64.into())
            ],
            nodes
        );
    }

    #[test]
    fn module_get_ast_nodes_by_index_let_decl_type() {
        let source = "let a: number = 123;";
        let module = parse(source, 0).unwrap();

        let nodes = module.get_by_index(10);

        assert_eq!(
            vec![
                AstNode::Stmt(&ast::Stmt::Assign(StmtAssign {
                    identifier: ast::Identifier {
                        name: "a".to_string()
                    },
                    type_expr: Some((ast::Expr::Type(ast::ExprType(TypeRef::number())), 7..13)),
                    is_const: false,
                    value: Some((123f64.into(), 16..19))
                })),
                AstNode::Identifier(&ast::Identifier {
                    name: "a".to_string()
                }),
                AstNode::Expr(&ast::Expr::Type(ast::ExprType(TypeRef::number())))
            ],
            nodes
        );
    }

    #[test]
    fn module_get_ast_nodes_by_index_const_decl_identifier() {
        let source = "const a;";
        let module = parse(source, 0).unwrap();

        let nodes = module.get_by_index(6);

        assert_eq!(
            vec![
                AstNode::Stmt(&ast::Stmt::Assign(StmtAssign {
                    identifier: ast::Identifier {
                        name: "a".to_string()
                    },
                    type_expr: None,
                    is_const: true,
                    value: None
                })),
                AstNode::Identifier(&ast::Identifier {
                    name: "a".to_string()
                })
            ],
            nodes
        );
    }

    #[test]
    fn module_get_ast_nodes_by_index_const_decl_identifier_b() {
        let source = "const a = 123;";
        let module = parse(source, 0).unwrap();

        let nodes = module.get_by_index(6);

        assert_eq!(
            vec![
                AstNode::Stmt(&ast::Stmt::Assign(StmtAssign {
                    identifier: ast::Identifier {
                        name: "a".to_string()
                    },
                    type_expr: None,
                    is_const: true,
                    value: Some((123f64.into(), 10..13))
                })),
                AstNode::Identifier(&ast::Identifier {
                    name: "a".to_string()
                })
            ],
            nodes
        );
    }

    #[test]
    fn module_get_ast_nodes_by_index_const_decl_value() {
        let source = "const a = 123;";
        let module = parse(source, 0).unwrap();

        let nodes = module.get_by_index(11);

        assert_eq!(
            vec![
                AstNode::Stmt(&ast::Stmt::Assign(StmtAssign {
                    identifier: ast::Identifier {
                        name: "a".to_string()
                    },
                    type_expr: None,
                    is_const: true,
                    value: Some((123f64.into(), 10..13))
                })),
                AstNode::Identifier(&ast::Identifier {
                    name: "a".to_string()
                }),
                AstNode::Expr(&123f64.into())
            ],
            nodes
        );
    }

    #[test]
    fn module_get_ast_nodes_by_index_const_decl_type() {
        let source = "const a: number = 123;";
        let module = parse(source, 0).unwrap();

        let nodes = module.get_by_index(12);

        assert_eq!(
            vec![
                AstNode::Stmt(&ast::Stmt::Assign(StmtAssign {
                    identifier: ast::Identifier {
                        name: "a".to_string()
                    },
                    type_expr: Some((ast::Expr::Type(ast::ExprType(TypeRef::number())), 9..15)),
                    is_const: true,
                    value: Some((123f64.into(), 18..21))
                })),
                AstNode::Identifier(&ast::Identifier {
                    name: "a".to_string()
                }),
                AstNode::Expr(&ast::Expr::Type(ast::ExprType(TypeRef::number())))
            ],
            nodes
        );
    }
}
