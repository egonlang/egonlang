use lalrpop_util::lalrpop_mod;

use crate::lexer::Lexer;

use lalrpop_util::ParseError;

use crate::ast::Module;
use crate::errors::{Error, ErrorS, SyntaxError};

lalrpop_mod!(
    #[allow(clippy::all)]
    grammar,
    "/grammar.rs"
);

pub type Parser = grammar::ModuleParser;

/// Return if a string source has a valid EOF
pub fn is_complete(source: &str) -> bool {
    let lexer = Lexer::new(source);
    let parser = Parser::new();
    let mut errors = Vec::new();
    if let Err(e) = parser.parse(&mut errors, lexer) {
        errors.push(e);
    };
    !errors
        .iter()
        .any(|e| matches!(e, ParseError::UnrecognizedEOF { .. }))
}

/// Parse a string source in to an AST [`Module`]
pub fn parse(source: &str, offset: usize) -> Result<Module, Vec<ErrorS>> {
    let lexer = Lexer::new(source).map(|token| match token {
        Ok((l, token, r)) => Ok((l + offset, token, r + offset)),
        Err((e, span)) => Err((e, span.start + offset..span.end + offset)),
    });
    let parser = Parser::new();
    let mut errors = Vec::new();

    let mut parser_errors = Vec::new();

    let module = match parser.parse(&mut parser_errors, lexer) {
        Ok(module) => module,
        Err(err) => {
            parser_errors.push(err);
            Module::default()
        }
    };

    errors.extend(parser_errors.into_iter().map(|err| match err {
        ParseError::ExtraToken {
            token: (start, _, end),
        } => (
            Error::SyntaxError(SyntaxError::ExtraToken {
                token: source[start..end].to_string(),
            }),
            start..end,
        ),
        ParseError::InvalidToken { location } => (
            Error::SyntaxError(SyntaxError::InvalidToken),
            location..location,
        ),
        ParseError::UnrecognizedEOF { location, expected } => (
            Error::SyntaxError(SyntaxError::UnrecognizedEOF { expected }),
            location..location,
        ),
        ParseError::UnrecognizedToken {
            token: (start, _, end),
            expected,
        } => (
            Error::SyntaxError(SyntaxError::UnrecognizedToken {
                token: source[start - offset..end - offset].to_string(),
                expected,
            }),
            start..end,
        ),
        ParseError::User { error } => error,
    }));

    if errors.is_empty() {
        Ok(module)
    } else {
        Err(errors)
    }
}

#[cfg(test)]
mod parser_tests {
    use std::vec;

    use pretty_assertions::assert_eq;

    use crate::ast::{
        Expr, ExprAssign, ExprBlock, ExprIdentifier, ExprIf, ExprInfix, ExprList, ExprLiteral,
        ExprTuple, Identifier, Module, OpInfix, Stmt, StmtExpr,
    };

    use crate::errors::{Error, SyntaxError};

    macro_rules! parser_test {
        ($test_name:ident, $input:expr, $expected:expr) => {
            #[test]
            fn $test_name() {
                let module = crate::parser::parse($input, 0);

                assert_eq!($expected, module);
            }
        };
    }

    parser_test!(
        parse_error_with_bare_number_expression,
        "123",
        Err(vec![(
            Error::SyntaxError(SyntaxError::UnrecognizedEOF {
                expected: vec![
                    "\"!=\"".to_string(),
                    "\"%\"".to_string(),
                    "\")\"".to_string(),
                    "\"*\"".to_string(),
                    "\"+\"".to_string(),
                    "\",\"".to_string(),
                    "\"-\"".to_string(),
                    "\"/\"".to_string(),
                    "\";\"".to_string(),
                    "\"<\"".to_string(),
                    "\"<=\"".to_string(),
                    "\"==\"".to_string(),
                    "\">\"".to_string(),
                    "\">=\"".to_string(),
                    "\"]\"".to_string(),
                    "\"and\"".to_string(),
                    "\"or\"".to_string(),
                    "\"}\"".to_string(),
                ]
            }),
            3..3
        )])
    );

    parser_test!(
        parse_error_with_bare_string_expression,
        r#""foo""#,
        Err(vec![(
            Error::SyntaxError(SyntaxError::UnrecognizedEOF {
                expected: vec![
                    "\"!=\"".to_string(),
                    "\"%\"".to_string(),
                    "\")\"".to_string(),
                    "\"*\"".to_string(),
                    "\"+\"".to_string(),
                    "\",\"".to_string(),
                    "\"-\"".to_string(),
                    "\"/\"".to_string(),
                    "\";\"".to_string(),
                    "\"<\"".to_string(),
                    "\"<=\"".to_string(),
                    "\"==\"".to_string(),
                    "\">\"".to_string(),
                    "\">=\"".to_string(),
                    "\"]\"".to_string(),
                    "\"and\"".to_string(),
                    "\"or\"".to_string(),
                    "\"}\"".to_string(),
                ]
            }),
            5..5
        )])
    );

    parser_test!(
        parse_error_with_bare_identifier_expression,
        r#"foo"#,
        Err(vec![(
            Error::SyntaxError(SyntaxError::UnrecognizedEOF {
                expected: vec![
                    "\"!=\"".to_string(),
                    "\"%\"".to_string(),
                    "\")\"".to_string(),
                    "\"*\"".to_string(),
                    "\"+\"".to_string(),
                    "\",\"".to_string(),
                    "\"-\"".to_string(),
                    "\"/\"".to_string(),
                    "\";\"".to_string(),
                    "\"<\"".to_string(),
                    "\"<=\"".to_string(),
                    "\"=\"".to_string(),
                    "\"==\"".to_string(),
                    "\">\"".to_string(),
                    "\">=\"".to_string(),
                    "\"]\"".to_string(),
                    "\"and\"".to_string(),
                    "\"or\"".to_string(),
                    "\"}\"".to_string(),
                ]
            }),
            3..3
        )])
    );

    parser_test!(
        parse_error_with_bare_true_expression,
        r#"true"#,
        Err(vec![(
            Error::SyntaxError(SyntaxError::UnrecognizedEOF {
                expected: vec![
                    "\"!=\"".to_string(),
                    "\"%\"".to_string(),
                    "\")\"".to_string(),
                    "\"*\"".to_string(),
                    "\"+\"".to_string(),
                    "\",\"".to_string(),
                    "\"-\"".to_string(),
                    "\"/\"".to_string(),
                    "\";\"".to_string(),
                    "\"<\"".to_string(),
                    "\"<=\"".to_string(),
                    "\"==\"".to_string(),
                    "\">\"".to_string(),
                    "\">=\"".to_string(),
                    "\"]\"".to_string(),
                    "\"and\"".to_string(),
                    "\"or\"".to_string(),
                    "\"}\"".to_string(),
                ]
            }),
            4..4
        )])
    );

    parser_test!(
        parse_error_with_bare_false_expression,
        r#"false"#,
        Err(vec![(
            Error::SyntaxError(SyntaxError::UnrecognizedEOF {
                expected: vec![
                    "\"!=\"".to_string(),
                    "\"%\"".to_string(),
                    "\")\"".to_string(),
                    "\"*\"".to_string(),
                    "\"+\"".to_string(),
                    "\",\"".to_string(),
                    "\"-\"".to_string(),
                    "\"/\"".to_string(),
                    "\";\"".to_string(),
                    "\"<\"".to_string(),
                    "\"<=\"".to_string(),
                    "\"==\"".to_string(),
                    "\">\"".to_string(),
                    "\">=\"".to_string(),
                    "\"]\"".to_string(),
                    "\"and\"".to_string(),
                    "\"or\"".to_string(),
                    "\"}\"".to_string(),
                ]
            }),
            5..5
        )])
    );

    parser_test!(
        parse_error_with_bare_block_expression,
        r#"{}"#,
        Err(vec![(
            Error::SyntaxError(SyntaxError::UnrecognizedEOF {
                expected: vec![
                    "\")\"".to_string(),
                    "\",\"".to_string(),
                    "\";\"".to_string(),
                    "\"]\"".to_string(),
                    "\"else\"".to_string(),
                    "\"}\"".to_string(),
                ]
            }),
            2..2
        )])
    );

    parser_test!(
        parse_error_with_bare_list_expression,
        r#"[]"#,
        Err(vec![(
            Error::SyntaxError(SyntaxError::UnrecognizedEOF {
                expected: vec![
                    "\"!=\"".to_string(),
                    "\"%\"".to_string(),
                    "\")\"".to_string(),
                    "\"*\"".to_string(),
                    "\"+\"".to_string(),
                    "\",\"".to_string(),
                    "\"-\"".to_string(),
                    "\"/\"".to_string(),
                    "\";\"".to_string(),
                    "\"<\"".to_string(),
                    "\"<=\"".to_string(),
                    "\"==\"".to_string(),
                    "\">\"".to_string(),
                    "\">=\"".to_string(),
                    "\"]\"".to_string(),
                    "\"and\"".to_string(),
                    "\"or\"".to_string(),
                    "\"}\"".to_string(),
                ]
            }),
            2..2
        )])
    );

    parser_test!(
        parse_number_statement,
        "123;",
        Ok(Module {
            stmts: vec![(
                Stmt::Expr(StmtExpr {
                    expr: (Expr::Literal(ExprLiteral::Number(123f64)), 0..3),
                }),
                0..4
            )]
        })
    );

    parser_test!(
        parse_string_statement,
        r#""foo";"#,
        Ok(Module {
            stmts: vec![(
                Stmt::Expr(StmtExpr {
                    expr: (Expr::Literal(ExprLiteral::String("foo".to_string())), 0..5),
                }),
                0..6
            )]
        })
    );

    parser_test!(
        parse_identifier_statement,
        r#"foo;"#,
        Ok(Module {
            stmts: vec![(
                Stmt::Expr(StmtExpr {
                    expr: (
                        Expr::Identifier(ExprIdentifier {
                            identifier: Identifier {
                                name: "foo".to_string()
                            }
                        }),
                        0..3
                    ),
                }),
                0..4
            )]
        })
    );

    parser_test!(
        parse_true_statement,
        "true;",
        Ok(Module {
            stmts: vec![(
                Stmt::Expr(StmtExpr {
                    expr: (Expr::Literal(ExprLiteral::Bool(true)), 0..4),
                }),
                0..5
            )]
        })
    );

    parser_test!(
        parse_false_statement,
        "false;",
        Ok(Module {
            stmts: vec![(
                Stmt::Expr(StmtExpr {
                    expr: (Expr::Literal(ExprLiteral::Bool(false)), 0..5),
                }),
                0..6
            )]
        })
    );

    parser_test!(
        parse_multiple_statements,
        r#"foo;123;"bar";true;false;"#,
        Ok(Module {
            stmts: vec![
                (
                    Stmt::Expr(StmtExpr {
                        expr: (
                            Expr::Identifier(ExprIdentifier {
                                identifier: Identifier {
                                    name: "foo".to_string()
                                }
                            }),
                            0..3
                        ),
                    }),
                    0..4
                ),
                (
                    Stmt::Expr(StmtExpr {
                        expr: (Expr::Literal(ExprLiteral::Number(123f64)), 4..7),
                    }),
                    4..8
                ),
                (
                    Stmt::Expr(StmtExpr {
                        expr: (Expr::Literal(ExprLiteral::String("bar".to_string())), 8..13),
                    }),
                    8..14
                ),
                (
                    Stmt::Expr(StmtExpr {
                        expr: (Expr::Literal(ExprLiteral::Bool(true)), 14..18),
                    }),
                    14..19
                ),
                (
                    Stmt::Expr(StmtExpr {
                        expr: (Expr::Literal(ExprLiteral::Bool(false)), 19..24),
                    }),
                    19..25
                ),
            ]
        })
    );

    parser_test!(
        parse_blocks_empty,
        "{};",
        Ok(Module {
            stmts: vec![(
                Stmt::Expr(StmtExpr {
                    expr: (
                        Expr::Block(Box::from(ExprBlock {
                            stmts: vec![],
                            return_expr: None
                        })),
                        0..2
                    )
                }),
                0..3
            )]
        })
    );

    parser_test!(
        parse_blocks_with_single_number_expression,
        "{123};",
        Ok(Module {
            stmts: vec![(
                Stmt::Expr(StmtExpr {
                    expr: (
                        Expr::Block(Box::new(ExprBlock {
                            stmts: vec![],
                            return_expr: Some((Expr::Literal(ExprLiteral::Number(123f64)), 1..4))
                        })),
                        0..5
                    )
                }),
                0..6
            )]
        })
    );

    parser_test!(
        parse_blocks_with_single_string_expression,
        r#"{"foo"};"#,
        Ok(Module {
            stmts: vec![(
                Stmt::Expr(StmtExpr {
                    expr: (
                        Expr::Block(Box::new(ExprBlock {
                            stmts: vec![],
                            return_expr: Some((
                                Expr::Literal(ExprLiteral::String("foo".to_string())),
                                1..6
                            ))
                        })),
                        0..7
                    )
                }),
                0..8
            )]
        })
    );

    parser_test!(
        parse_blocks_with_single_identifier_expression,
        r#"{foo};"#,
        Ok(Module {
            stmts: vec![(
                Stmt::Expr(StmtExpr {
                    expr: (
                        Expr::Block(Box::new(ExprBlock {
                            stmts: vec![],
                            return_expr: Some((
                                Expr::Identifier(ExprIdentifier {
                                    identifier: Identifier {
                                        name: "foo".to_string()
                                    }
                                }),
                                1..4
                            ))
                        })),
                        0..5
                    )
                }),
                0..6
            )]
        })
    );

    parser_test!(
        parse_blocks_with_single_statement,
        r#"{foo;};"#,
        Ok(Module {
            stmts: vec![(
                Stmt::Expr(StmtExpr {
                    expr: (
                        Expr::Block(Box::new(ExprBlock {
                            stmts: vec![(
                                Stmt::Expr(StmtExpr {
                                    expr: (
                                        Expr::Identifier(ExprIdentifier {
                                            identifier: Identifier {
                                                name: "foo".to_string()
                                            }
                                        }),
                                        1..4
                                    )
                                }),
                                1..5
                            )],
                            return_expr: None
                        })),
                        0..6
                    )
                }),
                0..7
            )]
        })
    );

    parser_test!(
        parse_blocks_with_block_statement,
        r#"{{};};"#,
        Ok(Module {
            stmts: vec![(
                Stmt::Expr(StmtExpr {
                    expr: (
                        Expr::Block(Box::new(ExprBlock {
                            stmts: vec![(
                                Stmt::Expr(StmtExpr {
                                    expr: (
                                        Expr::Block(Box::new(ExprBlock {
                                            stmts: vec![],
                                            return_expr: None
                                        })),
                                        1..3
                                    )
                                }),
                                1..4
                            )],
                            return_expr: None
                        })),
                        0..5
                    )
                }),
                0..6
            )]
        })
    );

    parser_test!(
        parse_blocks_with_block_expression,
        r#"{{}};"#,
        Ok(Module {
            stmts: vec![(
                Stmt::Expr(StmtExpr {
                    expr: (
                        Expr::Block(Box::new(ExprBlock {
                            stmts: vec![],
                            return_expr: Some((
                                Expr::Block(Box::new(ExprBlock {
                                    stmts: vec![],
                                    return_expr: None
                                })),
                                1..3
                            ))
                        })),
                        0..4
                    )
                }),
                0..5
            )]
        })
    );

    parser_test!(
        parse_blocks_with_block_statement_and_block_expression,
        r#"{{};{}};"#,
        Ok(Module {
            stmts: vec![(
                Stmt::Expr(StmtExpr {
                    expr: (
                        Expr::Block(Box::new(ExprBlock {
                            stmts: vec![(
                                Stmt::Expr(StmtExpr {
                                    expr: (
                                        Expr::Block(Box::new(ExprBlock {
                                            stmts: vec![],
                                            return_expr: None
                                        })),
                                        1..3
                                    )
                                }),
                                1..4
                            )],
                            return_expr: Some((
                                Expr::Block(Box::new(ExprBlock {
                                    stmts: vec![],
                                    return_expr: None
                                })),
                                4..6
                            ))
                        })),
                        0..7
                    )
                }),
                0..8
            )]
        })
    );

    parser_test!(
        parse_blocks_with_multiple_statements_and_return_expression,
        r#"{foo;123;void;"bar"};"#,
        Ok(Module {
            stmts: vec![(
                Stmt::Expr(StmtExpr {
                    expr: (
                        Expr::Block(Box::new(ExprBlock {
                            stmts: vec![
                                (
                                    Stmt::Expr(StmtExpr {
                                        expr: (
                                            Expr::Identifier(ExprIdentifier {
                                                identifier: Identifier {
                                                    name: "foo".to_string()
                                                }
                                            }),
                                            1..4
                                        )
                                    }),
                                    1..5
                                ),
                                (
                                    Stmt::Expr(StmtExpr {
                                        expr: (Expr::Literal(ExprLiteral::Number(123f64)), 5..8)
                                    }),
                                    5..9
                                ),
                                (
                                    Stmt::Expr(StmtExpr {
                                        expr: (Expr::Unit, 9..13)
                                    }),
                                    9..14
                                )
                            ],
                            return_expr: Some((
                                Expr::Literal(ExprLiteral::String("bar".to_string())),
                                14..19
                            ))
                        })),
                        0..20
                    )
                }),
                0..21
            )]
        })
    );

    parser_test!(
        parse_list_empty,
        "[];",
        Ok(Module {
            stmts: vec![(
                Stmt::Expr(StmtExpr {
                    expr: (Expr::List(ExprList { items: vec![] }), 0..2)
                }),
                0..3
            )]
        })
    );

    parser_test!(
        parse_list_empty_with_comma,
        "[,];",
        Err(vec![(
            Error::SyntaxError(SyntaxError::UnrecognizedToken {
                token: ",".to_string(),
                expected: vec![
                    "\"!\"".to_string(),
                    "\"(\"".to_string(),
                    "\"-\"".to_string(),
                    "\"[\"".to_string(),
                    "\"]\"".to_string(),
                    "\"false\"".to_string(),
                    "\"if\"".to_string(),
                    "\"true\"".to_string(),
                    "\"void\"".to_string(),
                    "\"{\"".to_string(),
                    "identifier".to_string(),
                    "number".to_string(),
                    "string".to_string(),
                ]
            }),
            1..2
        )])
    );

    parser_test!(
        parse_list_with_items,
        "[a, b, c];",
        Ok(Module {
            stmts: vec![(
                Stmt::Expr(StmtExpr {
                    expr: (
                        Expr::List(ExprList {
                            items: vec![
                                (
                                    Expr::Identifier(ExprIdentifier {
                                        identifier: Identifier {
                                            name: "a".to_string()
                                        }
                                    }),
                                    1..2
                                ),
                                (
                                    Expr::Identifier(ExprIdentifier {
                                        identifier: Identifier {
                                            name: "b".to_string()
                                        }
                                    }),
                                    4..5
                                ),
                                (
                                    Expr::Identifier(ExprIdentifier {
                                        identifier: Identifier {
                                            name: "c".to_string()
                                        }
                                    }),
                                    7..8
                                )
                            ]
                        }),
                        0..9
                    )
                }),
                0..10
            )]
        })
    );

    parser_test!(
        parse_list_with_items_and_trailing_commas,
        "[a, b, c,];",
        Err(vec![(
            Error::SyntaxError(SyntaxError::UnrecognizedToken {
                token: "]".to_string(),
                expected: vec![
                    "\"!\"".to_string(),
                    "\"(\"".to_string(),
                    "\"-\"".to_string(),
                    "\"[\"".to_string(),
                    "\"false\"".to_string(),
                    "\"if\"".to_string(),
                    "\"true\"".to_string(),
                    "\"void\"".to_string(),
                    "\"{\"".to_string(),
                    "identifier".to_string(),
                    "number".to_string(),
                    "string".to_string(),
                ]
            }),
            9..10
        )])
    );

    parser_test!(
        parse_list_with_nested_list,
        "[a, [b], c];",
        Ok(Module {
            stmts: vec![(
                Stmt::Expr(StmtExpr {
                    expr: (
                        Expr::List(ExprList {
                            items: vec![
                                (
                                    Expr::Identifier(ExprIdentifier {
                                        identifier: Identifier {
                                            name: "a".to_string()
                                        }
                                    }),
                                    1..2
                                ),
                                (
                                    Expr::List(ExprList {
                                        items: vec![(
                                            Expr::Identifier(ExprIdentifier {
                                                identifier: Identifier {
                                                    name: "b".to_string()
                                                }
                                            }),
                                            5..6
                                        )]
                                    }),
                                    4..7
                                ),
                                (
                                    Expr::Identifier(ExprIdentifier {
                                        identifier: Identifier {
                                            name: "c".to_string()
                                        }
                                    }),
                                    9..10
                                )
                            ]
                        }),
                        0..11
                    )
                }),
                0..12
            )]
        })
    );

    parser_test!(
        parse_unit,
        "void;",
        Ok(Module {
            stmts: vec![(
                Stmt::Expr(StmtExpr {
                    expr: (Expr::Unit, 0..4)
                }),
                0..5
            )]
        })
    );

    parser_test!(
        parse_tuple_empty,
        "(,);",
        Err(vec![(
            Error::SyntaxError(SyntaxError::UnrecognizedToken {
                token: ",".to_string(),
                expected: vec![
                    "\"!\"".to_string(),
                    "\"(\"".to_string(),
                    "\"-\"".to_string(),
                    "\"[\"".to_string(),
                    "\"false\"".to_string(),
                    "\"if\"".to_string(),
                    "\"true\"".to_string(),
                    "\"void\"".to_string(),
                    "\"{\"".to_string(),
                    "identifier".to_string(),
                    "number".to_string(),
                    "string".to_string(),
                ]
            }),
            1..2
        )])
    );

    parser_test!(
        parse_tuple_one_item,
        "(1,);",
        Ok(Module {
            stmts: vec![(
                Stmt::Expr(StmtExpr {
                    expr: (
                        Expr::Tuple(ExprTuple {
                            items: vec![(Expr::Literal(ExprLiteral::Number(1f64)), 1..2)]
                        }),
                        0..4
                    )
                }),
                0..5
            )]
        })
    );

    parser_test!(
        parse_tuple_two_items,
        "(1, 2,);",
        Ok(Module {
            stmts: vec![(
                Stmt::Expr(StmtExpr {
                    expr: (
                        Expr::Tuple(ExprTuple {
                            items: vec![
                                (Expr::Literal(ExprLiteral::Number(1f64)), 1..2),
                                (Expr::Literal(ExprLiteral::Number(2f64)), 4..5)
                            ]
                        }),
                        0..7
                    )
                }),
                0..8
            )]
        })
    );

    parser_test!(
        parse_grouping_expression,
        "(1);",
        Ok(Module {
            stmts: vec![(
                Stmt::Expr(StmtExpr {
                    expr: (Expr::Literal(ExprLiteral::Number(1f64)), 0..3)
                }),
                0..4
            )]
        })
    );

    macro_rules! parser_infix_test {
        ($test_name:ident, $input:expr, $expected:expr) => {
            parser_test!(
                $test_name,
                $input,
                Ok(Module {
                    stmts: vec![(
                        Stmt::Expr(StmtExpr {
                            expr: (
                                Expr::Infix(Box::new(ExprInfix {
                                    lt: (
                                        Expr::Identifier(ExprIdentifier {
                                            identifier: Identifier {
                                                name: "a".to_string()
                                            }
                                        }),
                                        0..1
                                    ),
                                    op: $expected,
                                    rt: (
                                        Expr::Identifier(ExprIdentifier {
                                            identifier: Identifier {
                                                name: "b".to_string()
                                            }
                                        }),
                                        4..5
                                    )
                                })),
                                0..5
                            )
                        }),
                        0..6
                    )]
                })
            );
        };
    }

    parser_infix_test!(parse_infix_expression_add, "a + b;", OpInfix::Add);

    parser_infix_test!(parse_infix_expression_subtract, "a - b;", OpInfix::Subtract);

    parser_infix_test!(parse_infix_expression_multiply, "a * b;", OpInfix::Multiply);

    parser_infix_test!(parse_infix_expression_divide, "a / b;", OpInfix::Divide);

    parser_infix_test!(parse_infix_expression_modulus, "a % b;", OpInfix::Modulus);

    parser_infix_test!(parse_infix_expression_gt, "a > b;", OpInfix::Greater);

    parser_infix_test!(parse_infix_expression_lt, "a < b;", OpInfix::Less);

    parser_test!(
        parse_assign_expression,
        "a = b;",
        Ok(Module {
            stmts: vec![(
                Stmt::Expr(StmtExpr {
                    expr: (
                        Expr::Assign(Box::new(ExprAssign {
                            identifier: Identifier {
                                name: "a".to_string()
                            },
                            value: (
                                Expr::Identifier(ExprIdentifier {
                                    identifier: Identifier {
                                        name: "b".to_string()
                                    }
                                }),
                                4..5
                            )
                        })),
                        0..5
                    )
                }),
                0..6
            )]
        })
    );

    parser_test!(
        parse_infix_expression_equals,
        "a == b;",
        Ok(Module {
            stmts: vec![(
                Stmt::Expr(StmtExpr {
                    expr: (
                        Expr::Infix(Box::new(ExprInfix {
                            lt: (
                                Expr::Identifier(ExprIdentifier {
                                    identifier: Identifier {
                                        name: "a".to_string()
                                    }
                                }),
                                0..1
                            ),
                            op: OpInfix::Equal,
                            rt: (
                                Expr::Identifier(ExprIdentifier {
                                    identifier: Identifier {
                                        name: "b".to_string()
                                    }
                                }),
                                5..6
                            )
                        })),
                        0..6
                    )
                }),
                0..7
            )]
        })
    );

    parser_test!(
        parse_infix_expression_not_equals,
        "a != b;",
        Ok(Module {
            stmts: vec![(
                Stmt::Expr(StmtExpr {
                    expr: (
                        Expr::Infix(Box::new(ExprInfix {
                            lt: (
                                Expr::Identifier(ExprIdentifier {
                                    identifier: Identifier {
                                        name: "a".to_string()
                                    }
                                }),
                                0..1
                            ),
                            op: OpInfix::NotEqual,
                            rt: (
                                Expr::Identifier(ExprIdentifier {
                                    identifier: Identifier {
                                        name: "b".to_string()
                                    }
                                }),
                                5..6
                            )
                        })),
                        0..6
                    )
                }),
                0..7
            )]
        })
    );

    parser_test!(
        parse_infix_expression_gte,
        "a >= b;",
        Ok(Module {
            stmts: vec![(
                Stmt::Expr(StmtExpr {
                    expr: (
                        Expr::Infix(Box::new(ExprInfix {
                            lt: (
                                Expr::Identifier(ExprIdentifier {
                                    identifier: Identifier {
                                        name: "a".to_string()
                                    }
                                }),
                                0..1
                            ),
                            op: OpInfix::GreaterEqual,
                            rt: (
                                Expr::Identifier(ExprIdentifier {
                                    identifier: Identifier {
                                        name: "b".to_string()
                                    }
                                }),
                                5..6
                            )
                        })),
                        0..6
                    )
                }),
                0..7
            )]
        })
    );

    parser_test!(
        parse_infix_expression_lte,
        "a <= b;",
        Ok(Module {
            stmts: vec![(
                Stmt::Expr(StmtExpr {
                    expr: (
                        Expr::Infix(Box::new(ExprInfix {
                            lt: (
                                Expr::Identifier(ExprIdentifier {
                                    identifier: Identifier {
                                        name: "a".to_string()
                                    }
                                }),
                                0..1
                            ),
                            op: OpInfix::LessEqual,
                            rt: (
                                Expr::Identifier(ExprIdentifier {
                                    identifier: Identifier {
                                        name: "b".to_string()
                                    }
                                }),
                                5..6
                            )
                        })),
                        0..6
                    )
                }),
                0..7
            )]
        })
    );

    parser_test!(
        parse_infix_expression_and,
        "a and b;",
        Ok(Module {
            stmts: vec![(
                Stmt::Expr(StmtExpr {
                    expr: (
                        Expr::Infix(Box::new(ExprInfix {
                            lt: (
                                Expr::Identifier(ExprIdentifier {
                                    identifier: Identifier {
                                        name: "a".to_string()
                                    }
                                }),
                                0..1
                            ),
                            op: OpInfix::LogicAnd,
                            rt: (
                                Expr::Identifier(ExprIdentifier {
                                    identifier: Identifier {
                                        name: "b".to_string()
                                    }
                                }),
                                6..7
                            )
                        })),
                        0..7
                    )
                }),
                0..8
            )]
        })
    );

    parser_test!(
        parse_infix_expression_or,
        "a or b;",
        Ok(Module {
            stmts: vec![(
                Stmt::Expr(StmtExpr {
                    expr: (
                        Expr::Infix(Box::new(ExprInfix {
                            lt: (
                                Expr::Identifier(ExprIdentifier {
                                    identifier: Identifier {
                                        name: "a".to_string()
                                    }
                                }),
                                0..1
                            ),
                            op: OpInfix::LogicOr,
                            rt: (
                                Expr::Identifier(ExprIdentifier {
                                    identifier: Identifier {
                                        name: "b".to_string()
                                    }
                                }),
                                5..6
                            )
                        })),
                        0..6
                    )
                }),
                0..7
            )]
        })
    );

    parser_test!(
        parse_if_cond_no_params,
        "if true {};",
        Err(vec![
            (
                Error::SyntaxError(SyntaxError::UnrecognizedToken {
                    token: "true".to_string(),
                    expected: vec!["\"(\"".to_string()]
                }),
                3..7
            ),
            (
                Error::SyntaxError(SyntaxError::UnrecognizedToken {
                    token: "{".to_string(),
                    expected: vec![
                        "\"!=\"".to_string(),
                        "\"%\"".to_string(),
                        "\")\"".to_string(),
                        "\"*\"".to_string(),
                        "\"+\"".to_string(),
                        "\",\"".to_string(),
                        "\"-\"".to_string(),
                        "\"/\"".to_string(),
                        "\";\"".to_string(),
                        "\"<\"".to_string(),
                        "\"<=\"".to_string(),
                        "\"==\"".to_string(),
                        "\">\"".to_string(),
                        "\">=\"".to_string(),
                        "\"]\"".to_string(),
                        "\"and\"".to_string(),
                        "\"or\"".to_string(),
                        "\"}\"".to_string(),
                    ]
                }),
                8..9
            )
        ])
    );

    parser_test!(
        parse_if_cond_params,
        "if (true) {};",
        Ok(Module {
            stmts: vec![(
                Stmt::Expr(StmtExpr {
                    expr: (
                        Expr::If(Box::new(ExprIf {
                            cond: (Expr::Literal(ExprLiteral::Bool(true)), 4..8),
                            then: (
                                Expr::Block(Box::new(ExprBlock {
                                    stmts: vec![],
                                    return_expr: None
                                })),
                                10..12
                            ),
                            else_: None
                        })),
                        0..12
                    )
                }),
                0..13
            )]
        })
    );

    parser_test!(
        parse_if_else,
        "if (true) {} else {};",
        Ok(Module {
            stmts: vec![(
                Stmt::Expr(StmtExpr {
                    expr: (
                        Expr::If(Box::new(ExprIf {
                            cond: (Expr::Literal(ExprLiteral::Bool(true)), 4..8),
                            then: (
                                Expr::Block(Box::new(ExprBlock {
                                    stmts: vec![],
                                    return_expr: None
                                })),
                                10..12
                            ),
                            else_: Some((
                                Expr::Block(Box::new(ExprBlock {
                                    stmts: vec![],
                                    return_expr: None
                                })),
                                18..20
                            ))
                        })),
                        0..20
                    )
                }),
                0..21
            )]
        })
    );

    parser_test!(
        parse_if_elif,
        "if (true) {} else if (true) {};",
        Ok(Module {
            stmts: vec![(
                Stmt::Expr(StmtExpr {
                    expr: (
                        Expr::If(Box::new(ExprIf {
                            cond: (Expr::Literal(ExprLiteral::Bool(true)), 4..8),
                            then: (
                                Expr::Block(Box::new(ExprBlock {
                                    stmts: vec![],
                                    return_expr: None
                                })),
                                10..12
                            ),
                            else_: Some((
                                Expr::If(Box::new(ExprIf {
                                    cond: (Expr::Literal(ExprLiteral::Bool(true)), 22..26),
                                    then: (
                                        Expr::Block(Box::new(ExprBlock {
                                            stmts: vec![],
                                            return_expr: None
                                        })),
                                        28..30
                                    ),
                                    else_: None
                                })),
                                18..30
                            ))
                        })),
                        0..30
                    )
                }),
                0..31
            )]
        })
    );

    parser_test!(
        parse_if_elif_else,
        "if (true) {} else if (true) {} else {};",
        Ok(Module {
            stmts: vec![(
                Stmt::Expr(StmtExpr {
                    expr: (
                        Expr::If(Box::new(ExprIf {
                            cond: (Expr::Literal(ExprLiteral::Bool(true)), 4..8),
                            then: (
                                Expr::Block(Box::new(ExprBlock {
                                    stmts: vec![],
                                    return_expr: None
                                })),
                                10..12
                            ),
                            else_: Some((
                                Expr::If(Box::new(ExprIf {
                                    cond: (Expr::Literal(ExprLiteral::Bool(true)), 22..26),
                                    then: (
                                        Expr::Block(Box::new(ExprBlock {
                                            stmts: vec![],
                                            return_expr: None
                                        })),
                                        28..30
                                    ),
                                    else_: Some((
                                        Expr::Block(Box::new(ExprBlock {
                                            stmts: vec![],
                                            return_expr: None
                                        })),
                                        36..38
                                    ))
                                })),
                                18..38
                            ))
                        })),
                        0..38
                    )
                }),
                0..39
            )]
        })
    );

    parser_test!(
        parse_if_elif_elif_else,
        "if (true) {} else if (true) {} else if (true) {} else {};",
        Ok(Module {
            stmts: vec![(
                Stmt::Expr(StmtExpr {
                    expr: (
                        Expr::If(Box::new(ExprIf {
                            cond: (Expr::Literal(ExprLiteral::Bool(true)), 4..8),
                            then: (
                                Expr::Block(Box::new(ExprBlock {
                                    stmts: vec![],
                                    return_expr: None
                                })),
                                10..12
                            ),
                            else_: Some((
                                Expr::If(Box::new(ExprIf {
                                    cond: (Expr::Literal(ExprLiteral::Bool(true)), 22..26),
                                    then: (
                                        Expr::Block(Box::new(ExprBlock {
                                            stmts: vec![],
                                            return_expr: None
                                        })),
                                        28..30
                                    ),
                                    else_: Some((
                                        Expr::If(Box::new(ExprIf {
                                            cond: (Expr::Literal(ExprLiteral::Bool(true)), 40..44),
                                            then: (
                                                Expr::Block(Box::new(ExprBlock {
                                                    stmts: vec![],
                                                    return_expr: None
                                                })),
                                                46..48
                                            ),
                                            else_: Some((
                                                Expr::Block(Box::new(ExprBlock {
                                                    stmts: vec![],
                                                    return_expr: None
                                                })),
                                                54..56
                                            ))
                                        })),
                                        36..56
                                    ))
                                })),
                                18..56
                            ))
                        })),
                        0..56
                    )
                }),
                0..57
            )]
        })
    );

    parser_test!(
        parse_if_elif_else_with_exprs_as_then_else,
        "if (true) 123 else if (true) 456 else 789",
        Err(vec![
            (
                Error::SyntaxError(SyntaxError::UnrecognizedToken {
                    token: "123".to_string(),
                    expected: vec!["\"{\"".to_string()]
                }),
                10..13
            ),
            (
                Error::SyntaxError(SyntaxError::UnrecognizedToken {
                    token: "else".to_string(),
                    expected: vec![
                        "\"!=\"".to_string(),
                        "\"%\"".to_string(),
                        "\")\"".to_string(),
                        "\"*\"".to_string(),
                        "\"+\"".to_string(),
                        "\",\"".to_string(),
                        "\"-\"".to_string(),
                        "\"/\"".to_string(),
                        "\";\"".to_string(),
                        "\"<\"".to_string(),
                        "\"<=\"".to_string(),
                        "\"==\"".to_string(),
                        "\">\"".to_string(),
                        "\">=\"".to_string(),
                        "\"]\"".to_string(),
                        "\"and\"".to_string(),
                        "\"or\"".to_string(),
                        "\"}\"".to_string(),
                    ]
                }),
                14..18
            ),
            (
                Error::SyntaxError(SyntaxError::UnrecognizedToken {
                    token: "456".to_string(),
                    expected: vec!["\"{\"".to_string()]
                }),
                29..32
            ),
            (
                Error::SyntaxError(SyntaxError::UnrecognizedToken {
                    token: "else".to_string(),
                    expected: vec![
                        "\"!=\"".to_string(),
                        "\"%\"".to_string(),
                        "\")\"".to_string(),
                        "\"*\"".to_string(),
                        "\"+\"".to_string(),
                        "\",\"".to_string(),
                        "\"-\"".to_string(),
                        "\"/\"".to_string(),
                        "\";\"".to_string(),
                        "\"<\"".to_string(),
                        "\"<=\"".to_string(),
                        "\"==\"".to_string(),
                        "\">\"".to_string(),
                        "\">=\"".to_string(),
                        "\"]\"".to_string(),
                        "\"and\"".to_string(),
                        "\"or\"".to_string(),
                        "\"}\"".to_string(),
                    ]
                }),
                33..37
            ),
            (
                Error::SyntaxError(SyntaxError::UnrecognizedEOF {
                    expected: vec![
                        "\"!=\"".to_string(),
                        "\"%\"".to_string(),
                        "\")\"".to_string(),
                        "\"*\"".to_string(),
                        "\"+\"".to_string(),
                        "\",\"".to_string(),
                        "\"-\"".to_string(),
                        "\"/\"".to_string(),
                        "\";\"".to_string(),
                        "\"<\"".to_string(),
                        "\"<=\"".to_string(),
                        "\"==\"".to_string(),
                        "\">\"".to_string(),
                        "\">=\"".to_string(),
                        "\"]\"".to_string(),
                        "\"and\"".to_string(),
                        "\"or\"".to_string(),
                        "\"}\"".to_string(),
                    ]
                }),
                41..41
            )
        ])
    );
}
