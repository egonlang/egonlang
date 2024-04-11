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
        Expr, ExprBlock, ExprIdentifier, ExprLiteral, Identifier, Module, Stmt, StmtExpr,
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
                expected: vec!["\";\"".to_string(), "\"}\"".to_string()]
            }),
            3..3
        )])
    );

    parser_test!(
        parse_error_with_bare_string_expression,
        r#""foo""#,
        Err(vec![(
            Error::SyntaxError(SyntaxError::UnrecognizedEOF {
                expected: vec!["\";\"".to_string(), "\"}\"".to_string()]
            }),
            5..5
        )])
    );

    parser_test!(
        parse_error_with_bare_identifier_expression,
        r#"foo"#,
        Err(vec![(
            Error::SyntaxError(SyntaxError::UnrecognizedEOF {
                expected: vec!["\";\"".to_string(), "\"}\"".to_string()]
            }),
            3..3
        )])
    );

    parser_test!(
        parse_error_with_bare_block_expression,
        r#"{}"#,
        Err(vec![(
            Error::SyntaxError(SyntaxError::UnrecognizedEOF {
                expected: vec!["\";\"".to_string(), "\"}\"".to_string()]
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
                    value: (Expr::Literal(ExprLiteral::Number(123f64)), 0..3),
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
                    value: (Expr::Literal(ExprLiteral::String("foo".to_string())), 0..5),
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
                    value: (
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
        parse_multiple_statements,
        r#"foo;123;"bar";"#,
        Ok(Module {
            stmts: vec![
                (
                    Stmt::Expr(StmtExpr {
                        value: (
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
                        value: (Expr::Literal(ExprLiteral::Number(123f64)), 4..7),
                    }),
                    4..8
                ),
                (
                    Stmt::Expr(StmtExpr {
                        value: (Expr::Literal(ExprLiteral::String("bar".to_string())), 8..13),
                    }),
                    8..14
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
                    value: (
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
                    value: (
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
                    value: (
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
                    value: (
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
                    value: (
                        Expr::Block(Box::new(ExprBlock {
                            stmts: vec![(
                                Stmt::Expr(StmtExpr {
                                    value: (
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
                    value: (
                        Expr::Block(Box::new(ExprBlock {
                            stmts: vec![(
                                Stmt::Expr(StmtExpr {
                                    value: (
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
                    value: (
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
                    value: (
                        Expr::Block(Box::new(ExprBlock {
                            stmts: vec![(
                                Stmt::Expr(StmtExpr {
                                    value: (
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
        r#"{foo;123;"bar"};"#,
        Ok(Module {
            stmts: vec![(
                Stmt::Expr(StmtExpr {
                    value: (
                        Expr::Block(Box::new(ExprBlock {
                            stmts: vec![
                                (
                                    Stmt::Expr(StmtExpr {
                                        value: (
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
                                        value: (Expr::Literal(ExprLiteral::Number(123f64)), 5..8)
                                    }),
                                    5..9
                                )
                            ],
                            return_expr: Some((
                                Expr::Literal(ExprLiteral::String("bar".to_string())),
                                9..14
                            ))
                        })),
                        0..15
                    )
                }),
                0..16
            )]
        })
    );
}
