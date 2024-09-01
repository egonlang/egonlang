use lalrpop_util::lalrpop_mod;

use crate::lexer::Lexer;

use lalrpop_util::ParseError;

use crate::ast::Module;
use egonlang_errors::{EgonResultMultiSpannedErr, EgonSyntaxError};

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
        .any(|e| matches!(e, ParseError::UnrecognizedEof { .. }))
}

/// Parse a string source in to an AST [`Module`]
pub fn parse(source: &str, offset: usize) -> EgonResultMultiSpannedErr<Module> {
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

    errors.extend(parser_errors.into_iter().map(|err| {
        match err {
            ParseError::ExtraToken {
                token: (start, _, end),
            } => (
                EgonSyntaxError::ExtraToken {
                    token: source[start..end].to_string(),
                }
                .into(),
                start..end,
            ),
            ParseError::InvalidToken { location } => {
                (EgonSyntaxError::InvalidToken.into(), location..location)
            }
            ParseError::UnrecognizedEof { location, expected } => (
                EgonSyntaxError::UnrecognizedEOF { expected }.into(),
                location..location,
            ),
            ParseError::UnrecognizedToken {
                token: (start, _, end),
                expected,
            } => (
                EgonSyntaxError::UnrecognizedToken {
                    token: source[start - offset..end - offset].to_string(),
                    expected,
                }
                .into(),
                start..end,
            ),
            ParseError::User { error } => error,
        }
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

    use egonlang_types::Type;
    use pretty_assertions::assert_eq;

    use crate::ast::{
        self, Expr, ExprAssign, ExprBlock, ExprCall, ExprFn, ExprIdentifier, ExprIf, ExprInfix,
        ExprList, ExprLiteral, ExprRange, ExprTuple, ExprType, Identifier, Module, OpInfix, Stmt,
        StmtAssertType, StmtAssign, StmtExpr, StmtTypeAlias,
    };

    use egonlang_errors::{EgonError, EgonSyntaxError};

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
            EgonError::SyntaxError(EgonSyntaxError::UnrecognizedEOF {
                expected: vec![
                    "\"(\"".to_string(),
                    "\";\"".to_string(),
                    "\"..\"".to_string(),
                    "\"-\"".to_string(),
                    "\"+\"".to_string(),
                    "\"/\"".to_string(),
                    "\"%\"".to_string(),
                    "\"*\"".to_string(),
                    "\"!=\"".to_string(),
                    "\"==\"".to_string(),
                    "\">\"".to_string(),
                    "\">=\"".to_string(),
                    "\"<\"".to_string(),
                    "\"<=\"".to_string(),
                    "\"and\"".to_string(),
                    "\"or\"".to_string(),
                ]
            }),
            3..3
        )])
    );

    parser_test!(
        parse_error_with_bare_string_expression,
        r#""foo""#,
        Err(vec![(
            EgonError::SyntaxError(EgonSyntaxError::UnrecognizedEOF {
                expected: vec![
                    "\"(\"".to_string(),
                    "\";\"".to_string(),
                    "\"-\"".to_string(),
                    "\"+\"".to_string(),
                    "\"/\"".to_string(),
                    "\"%\"".to_string(),
                    "\"*\"".to_string(),
                    "\"!=\"".to_string(),
                    "\"==\"".to_string(),
                    "\">\"".to_string(),
                    "\">=\"".to_string(),
                    "\"<\"".to_string(),
                    "\"<=\"".to_string(),
                    "\"and\"".to_string(),
                    "\"or\"".to_string(),
                ]
            }),
            5..5
        )])
    );

    parser_test!(
        parse_error_with_bare_identifier_expression,
        r#"foo"#,
        Err(vec![(
            EgonError::SyntaxError(EgonSyntaxError::UnrecognizedEOF {
                expected: vec![
                    "\"(\"".to_string(),
                    "\";\"".to_string(),
                    "\"-\"".to_string(),
                    "\"+\"".to_string(),
                    "\"/\"".to_string(),
                    "\"%\"".to_string(),
                    "\"*\"".to_string(),
                    "\"!=\"".to_string(),
                    "\"=\"".to_string(),
                    "\"==\"".to_string(),
                    "\">\"".to_string(),
                    "\">=\"".to_string(),
                    "\"<\"".to_string(),
                    "\"<=\"".to_string(),
                    "\"and\"".to_string(),
                    "\"or\"".to_string()
                ]
            }),
            3..3
        )])
    );

    parser_test!(
        parse_error_with_bare_true_expression,
        r#"true"#,
        Err(vec![(
            EgonError::SyntaxError(EgonSyntaxError::UnrecognizedEOF {
                expected: vec![
                    "\"(\"".to_string(),
                    "\";\"".to_string(),
                    "\"-\"".to_string(),
                    "\"+\"".to_string(),
                    "\"/\"".to_string(),
                    "\"%\"".to_string(),
                    "\"*\"".to_string(),
                    "\"!=\"".to_string(),
                    "\"==\"".to_string(),
                    "\">\"".to_string(),
                    "\">=\"".to_string(),
                    "\"<\"".to_string(),
                    "\"<=\"".to_string(),
                    "\"and\"".to_string(),
                    "\"or\"".to_string(),
                ]
            }),
            4..4
        )])
    );

    parser_test!(
        parse_error_with_bare_false_expression,
        r#"false"#,
        Err(vec![(
            EgonError::SyntaxError(EgonSyntaxError::UnrecognizedEOF {
                expected: vec![
                    "\"(\"".to_string(),
                    "\";\"".to_string(),
                    "\"-\"".to_string(),
                    "\"+\"".to_string(),
                    "\"/\"".to_string(),
                    "\"%\"".to_string(),
                    "\"*\"".to_string(),
                    "\"!=\"".to_string(),
                    "\"==\"".to_string(),
                    "\">\"".to_string(),
                    "\">=\"".to_string(),
                    "\"<\"".to_string(),
                    "\"<=\"".to_string(),
                    "\"and\"".to_string(),
                    "\"or\"".to_string(),
                ]
            }),
            5..5
        )])
    );

    parser_test!(
        parse_error_with_bare_block_expression,
        r#"{}"#,
        Err(vec![(
            EgonError::SyntaxError(EgonSyntaxError::UnrecognizedEOF {
                expected: vec!["\";\"".to_string(),]
            }),
            2..2
        )])
    );

    parser_test!(
        parse_error_with_bare_list_expression,
        r#"[]"#,
        Err(vec![(
            EgonError::SyntaxError(EgonSyntaxError::UnrecognizedEOF {
                expected: vec![
                    "\"(\"".to_string(),
                    "\";\"".to_string(),
                    "\"-\"".to_string(),
                    "\"+\"".to_string(),
                    "\"/\"".to_string(),
                    "\"%\"".to_string(),
                    "\"*\"".to_string(),
                    "\"!=\"".to_string(),
                    "\"==\"".to_string(),
                    "\">\"".to_string(),
                    "\">=\"".to_string(),
                    "\"<\"".to_string(),
                    "\"<=\"".to_string(),
                    "\"and\"".to_string(),
                    "\"or\"".to_string(),
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
                            return_expr: None,
                            typeref: None
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
                            return_expr: Some((Expr::Literal(ExprLiteral::Number(123f64)), 1..4)),
                            typeref: None
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
                            )),
                            typeref: None
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
                            )),
                            typeref: None
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
                            return_expr: None,
                            typeref: None
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
                                            return_expr: None,
                                            typeref: None
                                        })),
                                        1..3
                                    )
                                }),
                                1..4
                            )],
                            return_expr: None,
                            typeref: None
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
                                    return_expr: None,
                                    typeref: None
                                })),
                                1..3
                            )),
                            typeref: None
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
                                            return_expr: None,
                                            typeref: None
                                        })),
                                        1..3
                                    )
                                }),
                                1..4
                            )],
                            return_expr: Some((
                                Expr::Block(Box::new(ExprBlock {
                                    stmts: vec![],
                                    return_expr: None,
                                    typeref: None
                                })),
                                4..6
                            )),
                            typeref: None
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
        r#"{foo;123;();"bar"};"#,
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
                                        expr: (Expr::Unit, 9..11)
                                    }),
                                    9..12
                                )
                            ],
                            return_expr: Some((
                                Expr::Literal(ExprLiteral::String("bar".to_string())),
                                12..17
                            )),
                            typeref: None
                        })),
                        0..18
                    )
                }),
                0..19
            )]
        })
    );

    parser_test!(
        parse_list_empty,
        "[];",
        Ok(Module {
            stmts: vec![(
                Stmt::Expr(StmtExpr {
                    expr: (Expr::List(ExprList { items: vec![] }.into()), 0..2)
                }),
                0..3
            )]
        })
    );

    parser_test!(
        parse_list_empty_with_comma,
        "[,];",
        Err(vec![(
            EgonError::SyntaxError(EgonSyntaxError::UnrecognizedToken {
                token: ",".to_string(),
                expected: vec![
                    "\"{\"".to_string(),
                    "\"[\"".to_string(),
                    "\"]\"".to_string(),
                    "\"(\"".to_string(),
                    "\"..\"".to_string(),
                    "\"-\"".to_string(),
                    "\"!\"".to_string(),
                    "\"true\"".to_string(),
                    "\"false\"".to_string(),
                    "\"if\"".to_string(),
                    "identifier".to_string(),
                    "string".to_string(),
                    "number".to_string(),
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
                        Expr::List(
                            ExprList {
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
                            }
                            .into()
                        ),
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
            EgonError::SyntaxError(EgonSyntaxError::UnrecognizedToken {
                token: "]".to_string(),
                expected: vec![
                    "\"{\"".to_string(),
                    "\"[\"".to_string(),
                    "\"(\"".to_string(),
                    "\"..\"".to_string(),
                    "\"-\"".to_string(),
                    "\"!\"".to_string(),
                    "\"true\"".to_string(),
                    "\"false\"".to_string(),
                    "\"if\"".to_string(),
                    "identifier".to_string(),
                    "string".to_string(),
                    "number".to_string(),
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
                        Expr::List(
                            ExprList {
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
                                        Expr::List(
                                            ExprList {
                                                items: vec![(
                                                    Expr::Identifier(ExprIdentifier {
                                                        identifier: Identifier {
                                                            name: "b".to_string()
                                                        }
                                                    }),
                                                    5..6
                                                )]
                                            }
                                            .into()
                                        ),
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
                            }
                            .into()
                        ),
                        0..11
                    )
                }),
                0..12
            )]
        })
    );

    parser_test!(
        parse_unit,
        "();",
        Ok(Module {
            stmts: vec![(
                Stmt::Expr(StmtExpr {
                    expr: (Expr::Unit, 0..2)
                }),
                0..3
            )]
        })
    );

    parser_test!(
        parse_tuple_empty,
        "(,);",
        Err(vec![(
            EgonError::SyntaxError(EgonSyntaxError::UnrecognizedToken {
                token: ",".to_string(),
                expected: vec![
                    "\"{\"".to_string(),
                    "\"[\"".to_string(),
                    "\"(\"".to_string(),
                    "\")\"".to_string(),
                    "\"..\"".to_string(),
                    "\"-\"".to_string(),
                    "\"!\"".to_string(),
                    "\"true\"".to_string(),
                    "\"false\"".to_string(),
                    "\"if\"".to_string(),
                    "identifier".to_string(),
                    "string".to_string(),
                    "number".to_string(),
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
                        Expr::Tuple(
                            ExprTuple {
                                items: vec![(Expr::Literal(ExprLiteral::Number(1f64)), 1..2)]
                            }
                            .into()
                        ),
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
                        Expr::Tuple(
                            ExprTuple {
                                items: vec![
                                    (Expr::Literal(ExprLiteral::Number(1f64)), 1..2),
                                    (Expr::Literal(ExprLiteral::Number(2f64)), 4..5)
                                ]
                            }
                            .into()
                        ),
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
                                        Expr::Identifier(
                                            ExprIdentifier {
                                                identifier: Identifier {
                                                    name: "a".to_string()
                                                }
                                            }
                                            .into()
                                        ),
                                        0..1
                                    ),
                                    op: $expected,
                                    rt: (
                                        Expr::Identifier(
                                            ExprIdentifier {
                                                identifier: Identifier {
                                                    name: "b".to_string()
                                                }
                                            }
                                            .into()
                                        ),
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
                            identifier: (
                                Identifier {
                                    name: "a".to_string()
                                },
                                0..1
                            ),
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
        Ok(Module {
            stmts: vec![(
                Stmt::Expr(StmtExpr {
                    expr: (
                        Expr::If(Box::new(ExprIf {
                            cond: (Expr::Literal(ExprLiteral::Bool(true)), 3..7),
                            then: (
                                Expr::Block(Box::new(ExprBlock {
                                    stmts: vec![],
                                    return_expr: None,
                                    typeref: None
                                })),
                                8..10
                            ),
                            else_: None
                        })),
                        0..10
                    )
                }),
                0..11
            )]
        })
    );

    parser_test!(
        parse_if_cond_params,
        "if (true) {};",
        Ok(Module {
            stmts: vec![(
                Stmt::Expr(StmtExpr {
                    expr: (
                        Expr::If(Box::new(ExprIf {
                            cond: (Expr::Literal(ExprLiteral::Bool(true)), 3..9),
                            then: (
                                Expr::Block(Box::new(ExprBlock {
                                    stmts: vec![],
                                    return_expr: None,
                                    typeref: None
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
                            cond: (Expr::Literal(ExprLiteral::Bool(true)), 3..9),
                            then: (
                                Expr::Block(Box::new(ExprBlock {
                                    stmts: vec![],
                                    return_expr: None,
                                    typeref: None
                                })),
                                10..12
                            ),
                            else_: Some((
                                Expr::Block(Box::new(ExprBlock {
                                    stmts: vec![],
                                    return_expr: None,
                                    typeref: None
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
                            cond: (Expr::Literal(ExprLiteral::Bool(true)), 3..9),
                            then: (
                                Expr::Block(Box::new(ExprBlock {
                                    stmts: vec![],
                                    return_expr: None,
                                    typeref: None
                                })),
                                10..12
                            ),
                            else_: Some((
                                Expr::If(Box::new(ExprIf {
                                    cond: (Expr::Literal(ExprLiteral::Bool(true)), 21..27),
                                    then: (
                                        Expr::Block(Box::new(ExprBlock {
                                            stmts: vec![],
                                            return_expr: None,
                                            typeref: None
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
                            cond: (Expr::Literal(ExprLiteral::Bool(true)), 3..9),
                            then: (
                                Expr::Block(Box::new(ExprBlock {
                                    stmts: vec![],
                                    return_expr: None,
                                    typeref: None
                                })),
                                10..12
                            ),
                            else_: Some((
                                Expr::If(Box::new(ExprIf {
                                    cond: (Expr::Literal(ExprLiteral::Bool(true)), 21..27),
                                    then: (
                                        Expr::Block(Box::new(ExprBlock {
                                            stmts: vec![],
                                            return_expr: None,
                                            typeref: None
                                        })),
                                        28..30
                                    ),
                                    else_: Some((
                                        Expr::Block(Box::new(ExprBlock {
                                            stmts: vec![],
                                            return_expr: None,
                                            typeref: None
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
                            cond: (Expr::Literal(ExprLiteral::Bool(true)), 3..9),
                            then: (
                                Expr::Block(Box::new(ExprBlock {
                                    stmts: vec![],
                                    return_expr: None,
                                    typeref: None
                                })),
                                10..12
                            ),
                            else_: Some((
                                Expr::If(Box::new(ExprIf {
                                    cond: (Expr::Literal(ExprLiteral::Bool(true)), 21..27),
                                    then: (
                                        Expr::Block(Box::new(ExprBlock {
                                            stmts: vec![],
                                            return_expr: None,
                                            typeref: None
                                        })),
                                        28..30
                                    ),
                                    else_: Some((
                                        Expr::If(Box::new(ExprIf {
                                            cond: (Expr::Literal(ExprLiteral::Bool(true)), 39..45),
                                            then: (
                                                Expr::Block(Box::new(ExprBlock {
                                                    stmts: vec![],
                                                    return_expr: None,
                                                    typeref: None
                                                })),
                                                46..48
                                            ),
                                            else_: Some((
                                                Expr::Block(Box::new(ExprBlock {
                                                    stmts: vec![],
                                                    return_expr: None,
                                                    typeref: None
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
                EgonError::SyntaxError(EgonSyntaxError::UnrecognizedToken {
                    token: "123".to_string(),
                    expected: vec![
                        "\"{\"".to_string(),
                        "\"(\"".to_string(),
                        "\"-\"".to_string(),
                        "\"+\"".to_string(),
                        "\"/\"".to_string(),
                        "\"%\"".to_string(),
                        "\"*\"".to_string(),
                        "\"!=\"".to_string(),
                        "\"==\"".to_string(),
                        "\">\"".to_string(),
                        "\">=\"".to_string(),
                        "\"<\"".to_string(),
                        "\"<=\"".to_string(),
                        "\"and\"".to_string(),
                        "\"or\"".to_string(),
                    ]
                }),
                10..13
            ),
            (
                EgonError::SyntaxError(EgonSyntaxError::UnrecognizedToken {
                    token: "else".to_string(),
                    expected: vec![
                        "\"(\"".to_string(),
                        "\";\"".to_string(),
                        "\"..\"".to_string(),
                        "\"-\"".to_string(),
                        "\"+\"".to_string(),
                        "\"/\"".to_string(),
                        "\"%\"".to_string(),
                        "\"*\"".to_string(),
                        "\"!=\"".to_string(),
                        "\"==\"".to_string(),
                        "\">\"".to_string(),
                        "\">=\"".to_string(),
                        "\"<\"".to_string(),
                        "\"<=\"".to_string(),
                        "\"and\"".to_string(),
                        "\"or\"".to_string(),
                    ]
                }),
                14..18
            ),
            (
                EgonError::SyntaxError(EgonSyntaxError::UnrecognizedToken {
                    token: "456".to_string(),
                    expected: vec![
                        "\"{\"".to_string(),
                        "\"(\"".to_string(),
                        "\"-\"".to_string(),
                        "\"+\"".to_string(),
                        "\"/\"".to_string(),
                        "\"%\"".to_string(),
                        "\"*\"".to_string(),
                        "\"!=\"".to_string(),
                        "\"==\"".to_string(),
                        "\">\"".to_string(),
                        "\">=\"".to_string(),
                        "\"<\"".to_string(),
                        "\"<=\"".to_string(),
                        "\"and\"".to_string(),
                        "\"or\"".to_string(),
                    ]
                }),
                29..32
            ),
            (
                EgonError::SyntaxError(EgonSyntaxError::UnrecognizedToken {
                    token: "else".to_string(),
                    expected: vec![
                        "\"(\"".to_string(),
                        "\";\"".to_string(),
                        "\"..\"".to_string(),
                        "\"-\"".to_string(),
                        "\"+\"".to_string(),
                        "\"/\"".to_string(),
                        "\"%\"".to_string(),
                        "\"*\"".to_string(),
                        "\"!=\"".to_string(),
                        "\"==\"".to_string(),
                        "\">\"".to_string(),
                        "\">=\"".to_string(),
                        "\"<\"".to_string(),
                        "\"<=\"".to_string(),
                        "\"and\"".to_string(),
                        "\"or\"".to_string(),
                    ]
                }),
                33..37
            ),
            (
                EgonError::SyntaxError(EgonSyntaxError::UnrecognizedEOF {
                    expected: vec![
                        "\"(\"".to_string(),
                        "\";\"".to_string(),
                        "\"..\"".to_string(),
                        "\"-\"".to_string(),
                        "\"+\"".to_string(),
                        "\"/\"".to_string(),
                        "\"%\"".to_string(),
                        "\"*\"".to_string(),
                        "\"!=\"".to_string(),
                        "\"==\"".to_string(),
                        "\">\"".to_string(),
                        "\">=\"".to_string(),
                        "\"<\"".to_string(),
                        "\"<=\"".to_string(),
                        "\"and\"".to_string(),
                        "\"or\"".to_string(),
                    ]
                }),
                41..41
            )
        ])
    );

    parser_test!(
        parse_range_start,
        "0..;",
        Ok(Module {
            stmts: vec![(
                Stmt::Expr(StmtExpr {
                    expr: (
                        Expr::Range(ExprRange {
                            start: Some((ExprLiteral::Number(0f64), 0..1)),
                            end: None,
                            inclusive_end: false
                        }),
                        0..3
                    )
                }),
                0..4
            )]
        })
    );

    parser_test!(
        parse_range_start_and_end,
        "0..10;",
        Ok(Module {
            stmts: vec![(
                Stmt::Expr(StmtExpr {
                    expr: (
                        Expr::Range(ExprRange {
                            start: Some((ExprLiteral::Number(0f64), 0..1)),
                            end: Some((ExprLiteral::Number(10f64), 3..5)),
                            inclusive_end: false
                        }),
                        0..5
                    )
                }),
                0..6
            )]
        })
    );

    parser_test!(
        parse_range_end,
        "..10;",
        Ok(Module {
            stmts: vec![(
                Stmt::Expr(StmtExpr {
                    expr: (
                        Expr::Range(ExprRange {
                            start: None,
                            end: Some((ExprLiteral::Number(10f64), 2..4)),
                            inclusive_end: false
                        }),
                        0..4
                    )
                }),
                0..5
            )]
        })
    );

    parser_test!(
        parse_range_inclusive_end,
        "..=10;",
        Ok(Module {
            stmts: vec![(
                Stmt::Expr(StmtExpr {
                    expr: (
                        Expr::Range(ExprRange {
                            start: None,
                            end: Some((ExprLiteral::Number(10f64), 3..5)),
                            inclusive_end: true
                        }),
                        0..5
                    )
                }),
                0..6
            )]
        })
    );

    parser_test!(
        parse_range_start_and_inclusive_end,
        "0..=10;",
        Ok(Module {
            stmts: vec![(
                Stmt::Expr(StmtExpr {
                    expr: (
                        Expr::Range(ExprRange {
                            start: Some((ExprLiteral::Number(0f64), 0..1)),
                            end: Some((ExprLiteral::Number(10f64), 4..6)),
                            inclusive_end: true
                        }),
                        0..6
                    )
                }),
                0..7
            )]
        })
    );

    parser_test!(
        parse_range_empty,
        "..;",
        Err(vec![(
            EgonError::SyntaxError(EgonSyntaxError::UnrecognizedToken {
                token: ";".to_string(),
                expected: vec!["\"=\"".to_string(), "number".to_string()]
            }),
            2..3
        )])
    );

    parser_test!(
        parse_range_empty_inclusive_no_end,
        "..=;",
        Err(vec![(
            EgonError::SyntaxError(EgonSyntaxError::UnrecognizedToken {
                token: ";".to_string(),
                expected: vec!["number".to_string()]
            }),
            3..4
        )])
    );

    parser_test!(
        parse_range_start_inclusive_no_end,
        "1..=;",
        Err(vec![(
            EgonError::SyntaxError(EgonSyntaxError::UnrecognizedToken {
                token: ";".to_string(),
                expected: vec!["number".to_string()]
            }),
            4..5
        )])
    );

    parser_test!(
        parse_let_decl_with_assign,
        "let a = 123;",
        Ok(Module {
            stmts: vec![(
                Stmt::Assign(StmtAssign {
                    identifier: (
                        Identifier {
                            name: "a".to_string()
                        },
                        4..5
                    ),
                    type_expr: None,
                    is_const: false,
                    value: Some((ast::Expr::Literal(ExprLiteral::Number(123f64)), 8..11))
                }),
                0..12
            )]
        })
    );

    parser_test!(
        parse_let_decl_with_assign_chain,
        "let a = b = 123;",
        Ok(Module {
            stmts: vec![(
                Stmt::Assign(StmtAssign {
                    identifier: (
                        Identifier {
                            name: "a".to_string()
                        },
                        4..5
                    ),
                    type_expr: None,
                    is_const: false,
                    value: Some((
                        ast::Expr::Assign(Box::new(ExprAssign {
                            identifier: (
                                Identifier {
                                    name: "b".to_string()
                                },
                                8..9
                            ),
                            value: (Expr::Literal(ExprLiteral::Number(123f64)), 12..15)
                        })),
                        8..15
                    ))
                }),
                0..16
            )]
        })
    );

    parser_test!(
        parse_let_decl_typed_with_assign_chain,
        "let a: number = b = 123;",
        Ok(Module {
            stmts: vec![(
                Stmt::Assign(StmtAssign {
                    identifier: (
                        Identifier {
                            name: "a".to_string()
                        },
                        4..5
                    ),
                    type_expr: Some((Expr::Type(ExprType(Type::number())), 7..13)),
                    is_const: false,
                    value: Some((
                        ast::Expr::Assign(Box::new(ExprAssign {
                            identifier: (
                                Identifier {
                                    name: "b".to_string()
                                },
                                16..17
                            ),
                            value: (Expr::Literal(ExprLiteral::Number(123f64)), 20..23)
                        })),
                        16..23
                    ))
                }),
                0..24
            )]
        })
    );

    parser_test!(
        parse_let_decl_typed_with_assign,
        "let a: number = 123;",
        Ok(Module {
            stmts: vec![(
                Stmt::Assign(StmtAssign {
                    identifier: (
                        Identifier {
                            name: "a".to_string()
                        },
                        4..5
                    ),
                    type_expr: Some((Expr::Type(ExprType(Type::number())), 7..13)),
                    is_const: false,
                    value: Some((ast::Expr::Literal(ExprLiteral::Number(123f64)), 16..19))
                }),
                0..20
            )]
        })
    );

    parser_test!(
        parse_let_decl_without_assign,
        "let a;",
        Ok(Module {
            stmts: vec![(
                Stmt::Assign(StmtAssign {
                    identifier: (
                        Identifier {
                            name: "a".to_string()
                        },
                        4..5
                    ),
                    type_expr: None,
                    is_const: false,
                    value: None
                }),
                0..6
            )]
        })
    );

    parser_test!(
        parse_type_alias,
        "type NumberList = list<number>;",
        Ok(Module {
            stmts: vec![(
                Stmt::TypeAlias(StmtTypeAlias {
                    alias: (
                        Identifier {
                            name: "NumberList".to_string()
                        },
                        5..15
                    ),
                    value: (Type::list(Type::number()), 18..30)
                }),
                0..31
            )]
        })
    );

    parser_test!(
        parse_type_alias_2,
        "
        type NumberList = list<number>;
        let a: NumberList = [1, 2];
        ",
        Ok(Module {
            stmts: vec![
                (
                    Stmt::TypeAlias(StmtTypeAlias {
                        alias: (
                            Identifier {
                                name: "NumberList".to_string()
                            },
                            14..24
                        ),
                        value: (Type::list(Type::number()), 27..39)
                    }),
                    9..40
                ),
                (
                    Stmt::Assign(StmtAssign {
                        identifier: (
                            Identifier {
                                name: "a".to_string()
                            },
                            53..54
                        ),
                        type_expr: Some((
                            Expr::Type(ExprType(Type("NumberList".to_string(), vec![]))),
                            56..66
                        )),
                        is_const: false,
                        value: Some((
                            Expr::List(
                                ExprList {
                                    items: vec![
                                        (Expr::Literal(ExprLiteral::Number(1f64)), 70..71),
                                        (Expr::Literal(ExprLiteral::Number(2f64)), 73..74)
                                    ]
                                }
                                .into()
                            ),
                            69..75
                        ))
                    }),
                    49..76
                )
            ]
        })
    );

    parser_test!(
        parse_type_alias_3,
        "
        (): () => {
            type Int = number;
            let a: number = 5;
            let b: Int = a + 10;
            b
        };
        ",
        Ok(Module {
            stmts: vec![(
                Stmt::Expr(StmtExpr {
                    expr: (
                        Expr::Fn(Box::new(ExprFn {
                            name: None,
                            params: vec![],
                            return_type: (Type::unit(), 13..15),
                            body: (
                                Expr::Block(Box::new(ExprBlock {
                                    stmts: vec![
                                        (
                                            Stmt::TypeAlias(StmtTypeAlias {
                                                alias: (
                                                    Identifier {
                                                        name: "Int".to_string()
                                                    },
                                                    38..41
                                                ),
                                                value: (Type::number(), 44..50)
                                            }),
                                            33..51
                                        ),
                                        (
                                            Stmt::Assign(StmtAssign {
                                                identifier: (
                                                    Identifier {
                                                        name: "a".to_string()
                                                    },
                                                    68..69
                                                ),
                                                type_expr: Some((
                                                    Expr::Type(ExprType(Type::number())),
                                                    71..77
                                                )),
                                                is_const: false,
                                                value: Some((
                                                    Expr::Literal(ExprLiteral::Number(5f64)),
                                                    80..81
                                                ))
                                            }),
                                            64..82
                                        ),
                                        (
                                            Stmt::Assign(StmtAssign {
                                                identifier: (
                                                    Identifier {
                                                        name: "b".to_string()
                                                    },
                                                    99..100
                                                ),
                                                type_expr: Some((
                                                    Expr::Type(ExprType(Type(
                                                        "Int".to_string(),
                                                        vec![]
                                                    ))),
                                                    102..105
                                                )),
                                                is_const: false,
                                                value: Some((
                                                    Expr::Infix(Box::new(ExprInfix {
                                                        lt: (
                                                            Expr::Identifier(ExprIdentifier {
                                                                identifier: Identifier {
                                                                    name: "a".to_string()
                                                                }
                                                            }),
                                                            108..109
                                                        ),
                                                        op: OpInfix::Add,
                                                        rt: (
                                                            Expr::Literal(ExprLiteral::Number(
                                                                10f64
                                                            )),
                                                            112..114
                                                        )
                                                    })),
                                                    108..114
                                                ))
                                            }),
                                            95..115
                                        )
                                    ],
                                    return_expr: Some((
                                        Expr::Identifier(ExprIdentifier {
                                            identifier: Identifier {
                                                name: "b".to_string()
                                            }
                                        }),
                                        128..129
                                    )),
                                    typeref: None
                                })),
                                19..139
                            )
                        })),
                        9..139
                    )
                }),
                9..140
            )]
        })
    );

    parser_test!(
        parse_let_decl_typed_without_assign,
        "let a: number;",
        Ok(Module {
            stmts: vec![(
                Stmt::Assign(StmtAssign {
                    identifier: (
                        Identifier {
                            name: "a".to_string()
                        },
                        4..5
                    ),
                    type_expr: Some((Expr::Type(ExprType(Type::number())), 7..13)),
                    is_const: false,
                    value: None
                }),
                0..14
            )]
        })
    );

    parser_test!(
        parse_let_decl_with_assign_from_if_else_expr,
        "let a = if (true) { 123 } else { 456 };",
        Ok(Module {
            stmts: vec![(
                Stmt::Assign(StmtAssign {
                    identifier: (
                        Identifier {
                            name: "a".to_string()
                        },
                        4..5
                    ),
                    type_expr: None,
                    is_const: false,
                    value: Some((
                        Expr::If(Box::new(ExprIf {
                            cond: (Expr::Literal(ExprLiteral::Bool(true)), 11..17),
                            then: (
                                Expr::Block(Box::new(ExprBlock {
                                    stmts: vec![],
                                    return_expr: Some((
                                        Expr::Literal(ExprLiteral::Number(123f64)),
                                        20..23
                                    )),
                                    typeref: None
                                })),
                                18..25
                            ),
                            else_: Some((
                                Expr::Block(Box::new(ExprBlock {
                                    stmts: vec![],
                                    return_expr: Some((
                                        Expr::Literal(ExprLiteral::Number(456f64)),
                                        33..36
                                    )),
                                    typeref: None
                                })),
                                31..38
                            ))
                        })),
                        8..38
                    ))
                }),
                0..39
            )]
        })
    );

    parser_test!(
        parse_assert_type,
        "assert_type 123, number;",
        Ok(Module {
            stmts: vec![(
                StmtAssertType {
                    value: (123f64.into(), 12..15),
                    expected_type: (ast::ExprType(Type::number()).into(), 17..23)
                }
                .into(),
                0..24
            )]
        })
    );

    parser_test!(
        parse_call_fn_expr,
        "(a: number): number => { a }(100);",
        Ok(Module {
            stmts: vec![(
                StmtExpr {
                    expr: (
                        Expr::Call(Box::new(ExprCall {
                            callee: (
                                Expr::Fn(Box::new(ExprFn {
                                    name: None,
                                    params: vec![(
                                        (
                                            Identifier {
                                                name: "a".to_string()
                                            },
                                            Type::number()
                                        ),
                                        1..10
                                    )],
                                    return_type: (Type::number(), 13..19),
                                    body: (
                                        Expr::Block(Box::new(ExprBlock {
                                            stmts: vec![],
                                            return_expr: Some((
                                                Expr::Identifier(ExprIdentifier {
                                                    identifier: Identifier {
                                                        name: "a".to_string()
                                                    }
                                                }),
                                                25..26
                                            )),
                                            typeref: None
                                        })),
                                        23..28
                                    )
                                })),
                                0..28
                            ),
                            args: vec![(Expr::Literal(ExprLiteral::Number(100f64)), 29..32)]
                        })),
                        0..33
                    )
                }
                .into(),
                0..34
            )]
        })
    );
}
