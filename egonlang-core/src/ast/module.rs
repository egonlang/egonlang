use std::{fmt::Debug, sync::Arc};

use egonlang_types::Type;
use serde::{Deserialize, Serialize};

use span::{Span, Spanned};

use super::{Expr, Stmt, StmtS};

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

    fn get_nodes_from_stmt<'a>(
        &self,
        stmt: &'a Stmt,
        span: &'a Span,
        index: usize,
    ) -> Vec<AstNode> {
        let mut nodes = vec![];

        if !span.contains(&index) {
            return vec![];
        }

        nodes.push(AstNode::Stmt((stmt.clone(), span.clone())));

        match &stmt {
            Stmt::Expr(stmt_expr) => {
                let (expr_expr, expr_span) = stmt_expr.expr.clone();

                let expr_nodes = self.get_nodes_from_expr(expr_expr, &expr_span, index);
                nodes.extend(expr_nodes);
            }
            Stmt::Assign(stmt_assign) => {
                if let Some((type_expr, type_span)) = stmt_assign.type_expr.clone() {
                    let type_nodes = self.get_nodes_from_expr(type_expr, &type_span, index);
                    nodes.extend(type_nodes);
                }

                if let Some((value_expr, value_span)) = stmt_assign.value.clone() {
                    let value_nodes = self.get_nodes_from_expr(value_expr, &value_span, index);
                    nodes.extend(value_nodes);
                }

                let (ident, ident_span) = &stmt_assign.identifier;
                if ident_span.contains(&index) {
                    nodes.push(AstNode::Identifier((ident.clone(), ident_span.clone())));
                }
            }
            Stmt::TypeAlias(stmt_type_alias) => {
                let (value_typeref, value_span) = &stmt_type_alias.value;
                if value_span.contains(&index) {
                    nodes.push(AstNode::Type((value_typeref.clone(), value_span.clone())));
                }

                let (ident, ident_span) = &stmt_type_alias.alias;
                if ident_span.contains(&index) {
                    nodes.push(AstNode::Identifier((ident.clone(), ident_span.clone())));
                }
            }
            Stmt::Fn(stmt_fn) => {
                let (fn_expr_expr, fn_expr_span) = stmt_fn.fn_expr.clone();
                let fn_expr_nodes = self.get_nodes_from_expr(fn_expr_expr, &fn_expr_span, index);

                nodes.extend(fn_expr_nodes);

                let (ident, ident_span) = &stmt_fn.name;
                if ident_span.contains(&index) {
                    nodes.push(AstNode::Identifier((ident.clone(), ident_span.clone())));
                }
            }
            Stmt::AssertType(stmt_assert_type) => {
                let (value_expr, value_span) = stmt_assert_type.value.clone();
                let value_nodes = self.get_nodes_from_expr(value_expr, &value_span, index);
                nodes.extend(value_nodes);

                let (expected_type_expr, expected_type_span) =
                    stmt_assert_type.expected_type.clone();
                let expected_type_nodes =
                    self.get_nodes_from_expr(expected_type_expr, &expected_type_span, index);
                nodes.extend(expected_type_nodes);
            }
            Stmt::Return(stmt_return) => {
                let (value_expr, value_span) = stmt_return.value.clone();
                let value_nodes = self.get_nodes_from_expr(value_expr, &value_span, index);
                nodes.extend(value_nodes);
            }
            Stmt::Error => {}
        };

        nodes
    }

    fn get_nodes_from_expr(&self, expr: Arc<Expr>, span: &'_ Span, index: usize) -> Vec<AstNode> {
        let mut nodes = vec![];

        if !span.contains(&index) {
            return nodes;
        }

        nodes.push(AstNode::Expr((expr.clone(), span.clone())));

        match &*expr {
            Expr::Unit => {}
            Expr::Literal(_) => {}
            Expr::Identifier(_) => {}
            Expr::Block(expr_block) => {
                for (stmt, stmt_span) in &expr_block.stmts {
                    let stmt_nodes = self.get_nodes_from_stmt(stmt, stmt_span, index);
                    nodes.extend(stmt_nodes);
                }

                if let Some((return_expr_expr, return_expr_span)) = &expr_block.return_expr {
                    let return_expr_nodes =
                        self.get_nodes_from_expr(return_expr_expr.clone(), return_expr_span, index);
                    nodes.extend(return_expr_nodes);
                }
            }
            Expr::List(expr_list) => {
                for (item_expr, item_span) in &expr_list.items {
                    let item_nodes = self.get_nodes_from_expr(item_expr.clone(), item_span, index);
                    nodes.extend(item_nodes);
                }
            }
            Expr::Tuple(expr_tuple) => {
                for (item_expr, item_span) in &expr_tuple.items {
                    let item_nodes = self.get_nodes_from_expr(item_expr.clone(), item_span, index);
                    nodes.extend(item_nodes);
                }
            }
            Expr::Infix(expr_infix) => {
                let (lt_value_expr, lt_value_span) = &expr_infix.lt;
                let lt_value_nodes =
                    self.get_nodes_from_expr(lt_value_expr.clone(), lt_value_span, index);
                nodes.extend(lt_value_nodes);

                let (rt_value_expr, rt_value_span) = &expr_infix.rt;
                let rt_value_nodes =
                    self.get_nodes_from_expr(rt_value_expr.clone(), rt_value_span, index);
                nodes.extend(rt_value_nodes);
            }
            Expr::Prefix(expr_prefix) => {
                let (rt_value_expr, rt_value_span) = &expr_prefix.rt;
                let rt_value_nodes =
                    self.get_nodes_from_expr(rt_value_expr.clone(), rt_value_span, index);
                nodes.extend(rt_value_nodes);
            }
            Expr::Assign(expr_assign) => {
                let (value_expr, value_span) = &expr_assign.value;
                let value_nodes = self.get_nodes_from_expr(value_expr.clone(), value_span, index);
                nodes.extend(value_nodes);

                nodes.push(AstNode::Identifier(expr_assign.identifier.clone()));
            }
            Expr::If(expr_if) => {
                let (value_expr, value_span) = &expr_if.cond;
                let value_nodes = self.get_nodes_from_expr(value_expr.clone(), value_span, index);
                nodes.extend(value_nodes);

                let (value_expr, value_span) = &expr_if.then;
                let value_nodes = self.get_nodes_from_expr(value_expr.clone(), value_span, index);
                nodes.extend(value_nodes);

                if let Some((else_expr, else_span)) = &expr_if.else_ {
                    let value_nodes = self.get_nodes_from_expr(else_expr.clone(), else_span, index);
                    nodes.extend(value_nodes);
                }
            }
            Expr::Fn(expr_fn) => {
                for ((param_identifier, param_type), param_span) in &expr_fn.params {
                    if param_span.contains(&index) {
                        nodes.push(AstNode::Type((param_type.clone(), param_span.clone())));
                        nodes.push(AstNode::Identifier((
                            param_identifier.clone(),
                            param_span.clone(),
                        )));
                    }
                }

                let (return_type_type, return_type_span) = &expr_fn.return_type;
                if return_type_span.contains(&index) {
                    nodes.push(AstNode::Type((
                        return_type_type.clone(),
                        return_type_span.clone(),
                    )));
                }

                let (body_expr, body_span) = &expr_fn.body;
                let body_nodes = self.get_nodes_from_expr(body_expr.clone(), body_span, index);
                nodes.extend(body_nodes);

                if let Some(expr_fn_name) = &expr_fn.name {
                    nodes.push(AstNode::Identifier((expr_fn_name.clone(), span.clone())));
                }
            }
            Expr::Range(_) => {}
            Expr::Type(expr_type) => {
                nodes.push(AstNode::Type((expr_type.0.clone(), span.clone())));
            }
            Expr::Call(expr_call) => {
                let callee_nodes = self.get_nodes_from_expr(
                    expr_call.callee.0.clone(),
                    &expr_call.callee.1,
                    index,
                );
                nodes.extend(callee_nodes);

                for arg in &expr_call.args {
                    let arg_nodes = self.get_nodes_from_expr(arg.0.clone(), &arg.1, index);
                    nodes.extend(arg_nodes);
                }
            }
        };

        nodes
    }

    /// Get AST nodes by span index
    pub fn get_by_index(&self, idx: usize) -> Vec<AstNode> {
        let mut nodes = vec![];

        for (stmt, span) in &self.stmts {
            let stmt_nodes = self.get_nodes_from_stmt(stmt, span, idx);
            nodes.extend(stmt_nodes);
        }

        nodes
    }
}

/// AST nodes contained in the [`Module`]
#[derive(PartialEq)]
pub enum AstNode {
    Stmt(super::StmtS),
    Expr(super::ExprS),
    Identifier(Spanned<super::Identifier>),
    Type(Spanned<Type>),
}

impl AstNode {
    pub fn node_type(&self) -> String {
        match self {
            AstNode::Stmt(_) => "Statement",
            AstNode::Expr(_) => "Expression",
            AstNode::Identifier(_) => "Identifier",
            AstNode::Type(_) => "Type",
        }
        .to_string()
    }
}

impl Debug for AstNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Stmt(arg0) => f.write_fmt(format_args!("{:#?}", arg0)),
            Self::Expr(arg0) => f.write_fmt(format_args!("{:#?}", arg0)),
            Self::Identifier(arg0) => f.write_fmt(format_args!("{:#?}", arg0)),
            Self::Type(arg0) => f.write_fmt(format_args!("{:#?}", arg0)),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::vec;

    use crate::prelude::*;
    use ast::*;
    use egonlang_types::Type;
    use pretty_assertions::assert_eq;

    #[test]
    fn module_get_ast_nodes_by_index_let_decl_identifier() {
        let source = "let a;";
        let module = parse(source, 0).unwrap();

        let nodes = module.get_by_index(4);

        assert_eq!(
            vec![
                AstNode::Stmt((
                    Stmt::Assign(
                        StmtAssign {
                            identifier: (
                                Identifier {
                                    name: "a".to_string()
                                },
                                4..5
                            ),
                            type_expr: None,
                            is_const: false,
                            value: None
                        }
                        .into()
                    ),
                    0..6
                )),
                AstNode::Identifier((
                    Identifier {
                        name: "a".to_string()
                    },
                    4..5
                ))
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
                AstNode::Stmt((
                    Stmt::Assign(
                        StmtAssign {
                            identifier: (
                                Identifier {
                                    name: "a".to_string()
                                },
                                4..5
                            ),
                            type_expr: None,
                            is_const: false,
                            value: Some((Expr::Literal(ExprLiteral::Number(123f64)).into(), 8..11))
                        }
                        .into()
                    ),
                    0..12
                )),
                AstNode::Identifier((
                    Identifier {
                        name: "a".to_string()
                    },
                    4..5
                ))
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
                AstNode::Stmt((
                    Stmt::Assign(
                        StmtAssign {
                            identifier: (
                                Identifier {
                                    name: "a".to_string()
                                },
                                4..5
                            ),
                            type_expr: None,
                            is_const: false,
                            value: Some((Expr::Literal(ExprLiteral::Number(123f64)).into(), 8..11))
                        }
                        .into()
                    ),
                    0..12
                )),
                AstNode::Expr((Expr::Literal(ExprLiteral::Number(123f64)).into(), 8..11)),
            ],
            nodes
        );
    }

    #[test]
    fn module_get_ast_nodes_by_index_let_decl_value_b() {
        let source = "let a = 123;";
        let module = parse(source, 0).unwrap();

        let nodes = module.get_by_index(4);

        assert_eq!(
            vec![
                AstNode::Stmt((
                    Stmt::Assign(
                        StmtAssign {
                            identifier: (
                                Identifier {
                                    name: "a".to_string()
                                },
                                4..5
                            ),
                            type_expr: None,
                            is_const: false,
                            value: Some((Expr::Literal(ExprLiteral::Number(123f64)).into(), 8..11))
                        }
                        .into()
                    ),
                    0..12
                )),
                AstNode::Identifier((
                    Identifier {
                        name: "a".to_string()
                    },
                    4..5
                )),
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
                AstNode::Stmt((
                    Stmt::Assign(
                        StmtAssign {
                            identifier: (
                                Identifier {
                                    name: "a".to_string()
                                },
                                4..5
                            ),
                            type_expr: Some((Expr::Type(ExprType(Type::number())).into(), 7..13)),
                            is_const: false,
                            value: Some((
                                Expr::Literal(ExprLiteral::Number(123f64)).into(),
                                16..19
                            ))
                        }
                        .into()
                    ),
                    0..20
                )),
                AstNode::Expr((Expr::Type(ExprType(Type::number())).into(), 7..13)),
                AstNode::Type((Type::number(), 7..13)),
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
                AstNode::Stmt((
                    Stmt::Assign(
                        StmtAssign {
                            identifier: (
                                Identifier {
                                    name: "a".to_string()
                                },
                                6..7
                            ),
                            type_expr: None,
                            is_const: true,
                            value: None
                        }
                        .into()
                    ),
                    0..8
                )),
                AstNode::Identifier((
                    Identifier {
                        name: "a".to_string()
                    },
                    6..7
                ))
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
                AstNode::Stmt((
                    Stmt::Assign(
                        StmtAssign {
                            identifier: (
                                Identifier {
                                    name: "a".to_string()
                                },
                                6..7
                            ),
                            type_expr: None,
                            is_const: true,
                            value: Some((
                                Expr::Literal(ExprLiteral::Number(123f64)).into(),
                                10..13
                            ))
                        }
                        .into()
                    ),
                    0..14
                )),
                AstNode::Identifier((
                    Identifier {
                        name: "a".to_string()
                    },
                    6..7
                ))
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
                AstNode::Stmt((
                    Stmt::Assign(
                        StmtAssign {
                            identifier: (
                                Identifier {
                                    name: "a".to_string()
                                },
                                6..7
                            ),
                            type_expr: None,
                            is_const: true,
                            value: Some((
                                Expr::Literal(ExprLiteral::Number(123f64)).into(),
                                10..13
                            ))
                        }
                        .into()
                    ),
                    0..14
                )),
                AstNode::Expr((Expr::Literal(ExprLiteral::Number(123f64)).into(), 10..13)),
            ],
            nodes
        );
    }

    #[test]
    fn module_get_ast_nodes_by_index_const_decl_ident() {
        let source = "const a = 123;";
        let module = parse(source, 0).unwrap();

        let nodes = module.get_by_index(6);

        assert_eq!(
            vec![
                AstNode::Stmt((
                    Stmt::Assign(
                        StmtAssign {
                            identifier: (
                                Identifier {
                                    name: "a".to_string()
                                },
                                6..7
                            ),
                            type_expr: None,
                            is_const: true,
                            value: Some((
                                Expr::Literal(ExprLiteral::Number(123f64)).into(),
                                10..13
                            ))
                        }
                        .into()
                    ),
                    0..14
                )),
                AstNode::Identifier((
                    Identifier {
                        name: "a".to_string()
                    },
                    6..7
                )),
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
                AstNode::Stmt((
                    Stmt::Assign(
                        StmtAssign {
                            identifier: (
                                Identifier {
                                    name: "a".to_string()
                                },
                                6..7
                            ),
                            type_expr: Some((Expr::Type(ExprType(Type::number())).into(), 9..15)),
                            is_const: true,
                            value: Some((
                                Expr::Literal(ExprLiteral::Number(123f64)).into(),
                                18..21
                            ))
                        }
                        .into()
                    ),
                    0..22
                )),
                AstNode::Expr((Expr::Type(ExprType(Type::number())).into(), 9..15)),
                AstNode::Type((Type::number(), 9..15)),
            ],
            nodes
        );
    }

    #[test]
    fn module_get_ast_nodes_by_index_if_expr() {
        let source = "if 10 < 0 { false };";
        let module = parse(source, 0).unwrap();

        let nodes = module.get_by_index(3);

        assert_eq!(
            vec![
                AstNode::Stmt((
                    Stmt::Expr(
                        StmtExpr {
                            expr: (
                                Expr::If(Box::new(ExprIf {
                                    cond: (
                                        Expr::Infix(Box::new(ExprInfix {
                                            lt: (
                                                Expr::Literal(ExprLiteral::Number(10f64)).into(),
                                                3..5
                                            ),
                                            op: OpInfix::Less,
                                            rt: (
                                                Expr::Literal(ExprLiteral::Number(0f64)).into(),
                                                8..9
                                            )
                                        }))
                                        .into(),
                                        3..9
                                    ),
                                    then: (
                                        Expr::Block(Box::new(ExprBlock {
                                            stmts: vec![],
                                            return_expr: Some((
                                                Expr::Literal(ExprLiteral::Bool(false)).into(),
                                                12..17
                                            )),
                                            typeref: None
                                        }))
                                        .into(),
                                        10..19
                                    ),
                                    else_: None
                                }))
                                .into(),
                                0..19
                            )
                        }
                        .into()
                    ),
                    0..20
                )),
                AstNode::Expr((
                    Expr::If(Box::new(ExprIf {
                        cond: (
                            Expr::Infix(Box::new(ExprInfix {
                                lt: (Expr::Literal(ExprLiteral::Number(10f64)).into(), 3..5),
                                op: OpInfix::Less,
                                rt: (Expr::Literal(ExprLiteral::Number(0f64)).into(), 8..9)
                            }))
                            .into(),
                            3..9
                        ),
                        then: (
                            Expr::Block(Box::new(ExprBlock {
                                stmts: vec![],
                                return_expr: Some((
                                    Expr::Literal(ExprLiteral::Bool(false)).into(),
                                    12..17
                                )),
                                typeref: None
                            }))
                            .into(),
                            10..19
                        ),
                        else_: None
                    }))
                    .into(),
                    0..19
                )),
                AstNode::Expr((
                    Expr::Infix(Box::new(ExprInfix {
                        lt: (Expr::Literal(ExprLiteral::Number(10f64)).into(), 3..5),
                        op: OpInfix::Less,
                        rt: (Expr::Literal(ExprLiteral::Number(0f64)).into(), 8..9)
                    }))
                    .into(),
                    3..9
                )),
                AstNode::Expr((Expr::Literal(ExprLiteral::Number(10f64)).into(), 3..5)),
            ],
            nodes
        );
    }

    #[test]
    fn module_get_ast_nodes_by_index_if_else_expr() {
        let source = "if 10 != 0 { false } else { true };";
        let module = parse(source, 0).unwrap();

        let nodes = module.get_by_index(9);

        assert_eq!(
            vec![
                AstNode::Stmt((
                    Stmt::Expr(
                        StmtExpr {
                            expr: (
                                Expr::If(Box::new(ExprIf {
                                    cond: (
                                        Expr::Infix(Box::new(ExprInfix {
                                            lt: (
                                                Expr::Literal(ExprLiteral::Number(10f64)).into(),
                                                3..5
                                            ),
                                            op: OpInfix::NotEqual,
                                            rt: (
                                                Expr::Literal(ExprLiteral::Number(0f64)).into(),
                                                9..10
                                            )
                                        }))
                                        .into(),
                                        3..10
                                    ),
                                    then: (
                                        Expr::Block(Box::new(ExprBlock {
                                            stmts: vec![],
                                            return_expr: Some((
                                                Expr::Literal(ExprLiteral::Bool(false)).into(),
                                                13..18
                                            )),
                                            typeref: None
                                        }))
                                        .into(),
                                        11..20
                                    ),
                                    else_: Some((
                                        Expr::Block(Box::new(ExprBlock {
                                            stmts: vec![],
                                            return_expr: Some((
                                                Expr::Literal(ExprLiteral::Bool(true)).into(),
                                                28..32
                                            )),
                                            typeref: None
                                        }))
                                        .into(),
                                        26..34
                                    ))
                                }))
                                .into(),
                                0..34
                            )
                        }
                        .into()
                    ),
                    0..35
                )),
                AstNode::Expr((
                    Expr::If(Box::new(ExprIf {
                        cond: (
                            Expr::Infix(Box::new(ExprInfix {
                                lt: (Expr::Literal(ExprLiteral::Number(10f64)).into(), 3..5),
                                op: OpInfix::NotEqual,
                                rt: (Expr::Literal(ExprLiteral::Number(0f64)).into(), 9..10)
                            }))
                            .into(),
                            3..10
                        ),
                        then: (
                            Expr::Block(Box::new(ExprBlock {
                                stmts: vec![],
                                return_expr: Some((
                                    Expr::Literal(ExprLiteral::Bool(false)).into(),
                                    13..18
                                )),
                                typeref: None
                            }))
                            .into(),
                            11..20
                        ),
                        else_: Some((
                            Expr::Block(Box::new(ExprBlock {
                                stmts: vec![],
                                return_expr: Some((
                                    Expr::Literal(ExprLiteral::Bool(true)).into(),
                                    28..32
                                )),
                                typeref: None
                            }))
                            .into(),
                            26..34
                        ))
                    }))
                    .into(),
                    0..34
                )),
                AstNode::Expr((
                    Expr::Infix(Box::new(ExprInfix {
                        lt: (Expr::Literal(ExprLiteral::Number(10f64)).into(), 3..5),
                        op: OpInfix::NotEqual,
                        rt: (Expr::Literal(ExprLiteral::Number(0f64)).into(), 9..10)
                    }))
                    .into(),
                    3..10
                )),
                AstNode::Expr((Expr::Literal(ExprLiteral::Number(0f64)).into(), 9..10)),
            ],
            nodes
        );
    }

    #[test]
    fn s() {
        let source = "(a: string): bool => { !a };";
        let module = parse(source, 0).unwrap();

        let nodes = module.get_by_index(24);

        assert_eq!(
            vec![
                AstNode::Stmt((
                    Stmt::Expr(
                        StmtExpr {
                            expr: (
                                Expr::Fn(Box::new(ExprFn {
                                    name: None,
                                    params: vec![(
                                        (
                                            Identifier {
                                                name: "a".to_string()
                                            },
                                            Type::string()
                                        ),
                                        1..10
                                    )],
                                    return_type: (Type::bool(), 13..17),
                                    body: (
                                        Expr::Block(Box::new(ExprBlock {
                                            stmts: vec![],
                                            return_expr: Some((
                                                Expr::Prefix(Box::new(ExprPrefix {
                                                    op: OpPrefix::Not,
                                                    rt: (
                                                        Expr::Identifier(ExprIdentifier {
                                                            identifier: Identifier {
                                                                name: "a".to_string()
                                                            }
                                                        })
                                                        .into(),
                                                        24..25
                                                    )
                                                }))
                                                .into(),
                                                23..25
                                            )),
                                            typeref: None
                                        }))
                                        .into(),
                                        21..27
                                    )
                                }))
                                .into(),
                                0..27
                            )
                        }
                        .into()
                    ),
                    0..28
                )),
                AstNode::Expr((
                    Expr::Fn(Box::new(ExprFn {
                        name: None,
                        params: vec![(
                            (
                                Identifier {
                                    name: "a".to_string()
                                },
                                Type::string()
                            ),
                            1..10
                        )],
                        return_type: (Type::bool(), 13..17),
                        body: (
                            Expr::Block(Box::new(ExprBlock {
                                stmts: vec![],
                                return_expr: Some((
                                    Expr::Prefix(Box::new(ExprPrefix {
                                        op: OpPrefix::Not,
                                        rt: (
                                            Expr::Identifier(ExprIdentifier {
                                                identifier: Identifier {
                                                    name: "a".to_string()
                                                }
                                            })
                                            .into(),
                                            24..25
                                        )
                                    }))
                                    .into(),
                                    23..25
                                )),
                                typeref: None
                            }))
                            .into(),
                            21..27
                        )
                    }))
                    .into(),
                    0..27
                )),
                AstNode::Expr((
                    Expr::Block(Box::new(ExprBlock {
                        stmts: vec![],
                        return_expr: Some((
                            Expr::Prefix(Box::new(ExprPrefix {
                                op: OpPrefix::Not,
                                rt: (
                                    Expr::Identifier(ExprIdentifier {
                                        identifier: Identifier {
                                            name: "a".to_string()
                                        }
                                    })
                                    .into(),
                                    24..25
                                )
                            }))
                            .into(),
                            23..25
                        )),
                        typeref: None
                    }))
                    .into(),
                    21..27
                )),
                AstNode::Expr((
                    Expr::Prefix(Box::new(ExprPrefix {
                        op: OpPrefix::Not,
                        rt: (
                            Expr::Identifier(ExprIdentifier {
                                identifier: Identifier {
                                    name: "a".to_string()
                                }
                            })
                            .into(),
                            24..25
                        )
                    }))
                    .into(),
                    23..25
                )),
                AstNode::Expr((
                    Expr::Identifier(ExprIdentifier {
                        identifier: Identifier {
                            name: "a".to_string()
                        }
                    })
                    .into(),
                    24..25
                )),
            ],
            nodes
        );
    }

    #[test]
    fn module_get_ast_nodes_by_index_assert_type() {
        let source = "assert_type a, bool;";
        let module = parse(source, 0).unwrap();

        let nodes = module.get_by_index(16);

        assert_eq!(
            vec![
                AstNode::Stmt((
                    Stmt::AssertType(
                        StmtAssertType {
                            value: (
                                Expr::Identifier(ExprIdentifier {
                                    identifier: Identifier {
                                        name: "a".to_string()
                                    }
                                })
                                .into(),
                                12..13
                            ),
                            expected_type: (Expr::Type(ExprType(Type::bool())).into(), 15..19)
                        }
                        .into()
                    ),
                    0..20
                )),
                AstNode::Expr((Expr::Type(ExprType(Type::bool())).into(), 15..19)),
                AstNode::Type((Type::bool(), 15..19)),
            ],
            nodes
        );
    }

    #[test]
    fn module_get_ast_nodes_by_index_call_expr_callee() {
        let source = "foo(1, 2, 3);";
        let module = parse(source, 0).unwrap();

        let nodes = module.get_by_index(2);

        assert_eq!(
            vec![
                AstNode::Stmt((
                    StmtExpr {
                        expr: (
                            Expr::Call(
                                ExprCall {
                                    callee: (
                                        Expr::Identifier(ExprIdentifier {
                                            identifier: Identifier {
                                                name: "foo".to_string()
                                            }
                                        })
                                        .into(),
                                        0..3
                                    ),
                                    args: vec![
                                        (Expr::Literal(ExprLiteral::Number(1f64)).into(), 4..5),
                                        (Expr::Literal(ExprLiteral::Number(2f64)).into(), 7..8),
                                        (Expr::Literal(ExprLiteral::Number(3f64)).into(), 10..11)
                                    ]
                                }
                                .into()
                            )
                            .into(),
                            0..12
                        )
                    }
                    .into(),
                    0..13
                )),
                AstNode::Expr((
                    Expr::Call(
                        ExprCall {
                            callee: (
                                Expr::Identifier(ExprIdentifier {
                                    identifier: Identifier {
                                        name: "foo".to_string()
                                    }
                                })
                                .into(),
                                0..3
                            ),
                            args: vec![
                                (Expr::Literal(ExprLiteral::Number(1f64)).into(), 4..5),
                                (Expr::Literal(ExprLiteral::Number(2f64)).into(), 7..8),
                                (Expr::Literal(ExprLiteral::Number(3f64)).into(), 10..11)
                            ]
                        }
                        .into()
                    )
                    .into(),
                    0..12
                )),
                AstNode::Expr((
                    Expr::Identifier(ExprIdentifier {
                        identifier: Identifier {
                            name: "foo".to_string()
                        }
                    })
                    .into(),
                    0..3
                ))
            ],
            nodes
        );
    }

    #[test]
    fn module_get_ast_nodes_by_index_call_expr_arg0() {
        let source = "foo(1, 2, 3);";
        let module = parse(source, 0).unwrap();

        let nodes = module.get_by_index(4);

        assert_eq!(
            vec![
                AstNode::Stmt((
                    StmtExpr {
                        expr: (
                            Expr::Call(
                                ExprCall {
                                    callee: (
                                        Expr::Identifier(ExprIdentifier {
                                            identifier: Identifier {
                                                name: "foo".to_string()
                                            }
                                        })
                                        .into(),
                                        0..3
                                    ),
                                    args: vec![
                                        (Expr::Literal(ExprLiteral::Number(1f64)).into(), 4..5),
                                        (Expr::Literal(ExprLiteral::Number(2f64)).into(), 7..8),
                                        (Expr::Literal(ExprLiteral::Number(3f64)).into(), 10..11)
                                    ]
                                }
                                .into()
                            )
                            .into(),
                            0..12
                        )
                    }
                    .into(),
                    0..13
                )),
                AstNode::Expr((
                    Expr::Call(
                        ExprCall {
                            callee: (
                                Expr::Identifier(ExprIdentifier {
                                    identifier: Identifier {
                                        name: "foo".to_string()
                                    }
                                })
                                .into(),
                                0..3
                            ),
                            args: vec![
                                (Expr::Literal(ExprLiteral::Number(1f64)).into(), 4..5),
                                (Expr::Literal(ExprLiteral::Number(2f64)).into(), 7..8),
                                (Expr::Literal(ExprLiteral::Number(3f64)).into(), 10..11)
                            ]
                        }
                        .into()
                    )
                    .into(),
                    0..12
                )),
                AstNode::Expr((Expr::Literal(ExprLiteral::Number(1f64)).into(), 4..5))
            ],
            nodes
        );
    }

    #[test]
    fn module_get_ast_nodes_by_index_call_expr_arg1() {
        let source = "foo(1, 2, 3);";
        let module = parse(source, 0).unwrap();

        let nodes = module.get_by_index(7);

        assert_eq!(
            vec![
                AstNode::Stmt((
                    StmtExpr {
                        expr: (
                            Expr::Call(
                                ExprCall {
                                    callee: (
                                        Expr::Identifier(ExprIdentifier {
                                            identifier: Identifier {
                                                name: "foo".to_string()
                                            }
                                        })
                                        .into(),
                                        0..3
                                    ),
                                    args: vec![
                                        (Expr::Literal(ExprLiteral::Number(1f64)).into(), 4..5),
                                        (Expr::Literal(ExprLiteral::Number(2f64)).into(), 7..8),
                                        (Expr::Literal(ExprLiteral::Number(3f64)).into(), 10..11)
                                    ]
                                }
                                .into()
                            )
                            .into(),
                            0..12
                        )
                    }
                    .into(),
                    0..13
                )),
                AstNode::Expr((
                    Expr::Call(
                        ExprCall {
                            callee: (
                                Expr::Identifier(ExprIdentifier {
                                    identifier: Identifier {
                                        name: "foo".to_string()
                                    }
                                })
                                .into(),
                                0..3
                            ),
                            args: vec![
                                (Expr::Literal(ExprLiteral::Number(1f64)).into(), 4..5),
                                (Expr::Literal(ExprLiteral::Number(2f64)).into(), 7..8),
                                (Expr::Literal(ExprLiteral::Number(3f64)).into(), 10..11)
                            ]
                        }
                        .into()
                    )
                    .into(),
                    0..12
                )),
                AstNode::Expr((Expr::Literal(ExprLiteral::Number(2f64)).into(), 7..8))
            ],
            nodes
        );
    }

    #[test]
    fn module_get_ast_nodes_by_index_call_expr_arg2() {
        let source = "foo(1, 2, 3);";
        let module = parse(source, 0).unwrap();

        let nodes = module.get_by_index(10);

        assert_eq!(
            vec![
                AstNode::Stmt((
                    StmtExpr {
                        expr: (
                            Expr::Call(
                                ExprCall {
                                    callee: (
                                        Expr::Identifier(ExprIdentifier {
                                            identifier: Identifier {
                                                name: "foo".to_string()
                                            }
                                        })
                                        .into(),
                                        0..3
                                    ),
                                    args: vec![
                                        (Expr::Literal(ExprLiteral::Number(1f64)).into(), 4..5),
                                        (Expr::Literal(ExprLiteral::Number(2f64)).into(), 7..8),
                                        (Expr::Literal(ExprLiteral::Number(3f64)).into(), 10..11)
                                    ]
                                }
                                .into()
                            )
                            .into(),
                            0..12
                        )
                    }
                    .into(),
                    0..13
                )),
                AstNode::Expr((
                    Expr::Call(
                        ExprCall {
                            callee: (
                                Expr::Identifier(ExprIdentifier {
                                    identifier: Identifier {
                                        name: "foo".to_string()
                                    }
                                })
                                .into(),
                                0..3
                            ),
                            args: vec![
                                (Expr::Literal(ExprLiteral::Number(1f64)).into(), 4..5),
                                (Expr::Literal(ExprLiteral::Number(2f64)).into(), 7..8),
                                (Expr::Literal(ExprLiteral::Number(3f64)).into(), 10..11)
                            ]
                        }
                        .into()
                    )
                    .into(),
                    0..12
                )),
                AstNode::Expr((Expr::Literal(ExprLiteral::Number(3f64)).into(), 10..11))
            ],
            nodes
        );
    }
}
