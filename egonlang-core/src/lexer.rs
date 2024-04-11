use std::num::ParseFloatError;

use logos::Logos;

use crate::errors::{Error, ErrorS, SyntaxError};

/// Converts a [`String`] source in to a vector of [`Token`]
#[derive(Debug)]
pub struct Lexer<'a> {
    inner: logos::Lexer<'a, Token>,
    pending: Option<(usize, Token, usize)>,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            inner: Token::lexer(source),
            pending: None,
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<(usize, Token, usize), ErrorS>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(token) = self.pending.take() {
            return Some(Ok(token));
        }

        match self.inner.next()? {
            Token::Error => {
                let mut span = self.inner.span();

                // Check for unterminated string.
                if self.inner.slice().starts_with('"') {
                    return Some(Err((
                        Error::SyntaxError(SyntaxError::UnterminatedString),
                        span,
                    )));
                }

                // Recover error.
                while let Some(token) = self.inner.next() {
                    let span_new = self.inner.span();
                    if span.end == span_new.start {
                        span.end = span_new.end;
                    } else {
                        self.pending = Some((span_new.start, token, span_new.end));
                        break;
                    }
                }

                Some(Err((
                    Error::SyntaxError(SyntaxError::UnexpectedInput {
                        token: self.inner.source()[span.start..span.end].to_string(),
                    }),
                    span,
                )))
            }
            token => {
                let span = self.inner.span();
                Some(Ok((span.start, token, span.end)))
            }
        }
    }
}

#[derive(Clone, Debug, Logos, PartialEq)]
pub enum Token {
    // Single-character tokens.
    #[token(";")]
    Semicolon,
    #[token("{")]
    BracketOpen,
    #[token("}")]
    BracketClose,

    // Literals.
    #[regex("[a-zA-Z_][a-zA-Z0-9_]*", lex_identifier)]
    Identifier(String),
    #[regex(r#""[^"]*""#, lex_string)]
    String(String),
    #[regex(r#"[0-9]+(\.[0-9]+)?"#, lex_number)]
    Number(f64),

    #[regex(r"//.*", logos::skip)]
    #[regex(r"[ \r\n\t\f]+", logos::skip)]
    #[error]
    Error,
}

fn lex_number(lexer: &mut logos::Lexer<Token>) -> Result<f64, ParseFloatError> {
    let slice = lexer.slice();
    slice.parse::<f64>()
}

fn lex_string(lexer: &mut logos::Lexer<Token>) -> String {
    let slice = lexer.slice();
    slice[1..slice.len() - 1].to_string()
}

fn lex_identifier(lexer: &mut logos::Lexer<Token>) -> String {
    let slice = lexer.slice();
    slice.to_string()
}

#[cfg(test)]
mod lexer_tests {
    use pretty_assertions::assert_eq;

    use super::*;

    macro_rules! lexer_test {
        ($test_name:ident, $input:expr, $expected:expr) => {
            #[test]
            fn $test_name() {
                let actual = Lexer::new($input).collect::<Vec<_>>();

                assert_eq!($expected, actual);
            }
        };
    }

    lexer_test!(lex_number, "1234", vec![Ok((0, Token::Number(1234f64), 4))]);

    lexer_test!(
        lex_string,
        "\"Hello World\"",
        vec![Ok((0, Token::String("Hello World".to_string()), 13))]
    );

    lexer_test!(
        lex_multiline_string,
        "\"\nfoo\n\"",
        vec![Ok((0, Token::String("\nfoo\n".to_string()), 7))]
    );

    lexer_test!(
        lex_string_containing_semicolons,
        "\"Hello;World;\"",
        vec![Ok((0, Token::String("Hello;World;".to_string()), 14))]
    );

    lexer_test!(
        lex_identifier,
        "foo",
        vec![Ok((0, Token::Identifier("foo".to_string()), 3))]
    );

    lexer_test!(
        lex_multiple_expressions_numbers,
        "12;34",
        vec![
            Ok((0, Token::Number(12f64), 2)),
            Ok((2, Token::Semicolon, 3)),
            Ok((3, Token::Number(34f64), 5))
        ]
    );

    lexer_test!(
        lex_multiple_expressions_strings,
        "\"Hello\";\"World\"",
        vec![
            Ok((0, Token::String("Hello".to_string()), 7)),
            Ok((7, Token::Semicolon, 8)),
            Ok((8, Token::String("World".to_string()), 15))
        ]
    );

    lexer_test!(
        lex_multiple_expressions_strings_containing_semicolons,
        "\"Hel;lo\";\"Wor;ld\"",
        vec![
            Ok((0, Token::String("Hel;lo".to_string()), 8)),
            Ok((8, Token::Semicolon, 9)),
            Ok((9, Token::String("Wor;ld".to_string()), 17))
        ]
    );

    lexer_test!(
        lex_multiple_expressions_identifiers,
        "hello;world",
        vec![
            Ok((0, Token::Identifier("hello".to_string()), 5)),
            Ok((5, Token::Semicolon, 6)),
            Ok((6, Token::Identifier("world".to_string()), 11))
        ]
    );

    lexer_test!(
        lex_multiple_expressions_mixed,
        "hello;12.34;\"foo\"",
        vec![
            Ok((0, Token::Identifier("hello".to_string()), 5)),
            Ok((5, Token::Semicolon, 6)),
            Ok((6, Token::Number(12.34f64), 11)),
            Ok((11, Token::Semicolon, 12)),
            Ok((12, Token::String("foo".to_string()), 17))
        ]
    );

    lexer_test!(
        lex_error_unexpected_input,
        "@foo bar",
        vec![
            Err((
                Error::SyntaxError(SyntaxError::UnexpectedInput {
                    token: "@foo".to_string()
                }),
                0..4,
            )),
            Ok((5, Token::Identifier("bar".to_string()), 8)),
        ]
    );

    lexer_test!(
        lex_error_unterminated_string,
        "\"\nfoo",
        vec![Err((
            Error::SyntaxError(SyntaxError::UnterminatedString),
            0..5
        ))]
    );

    lexer_test!(
        lex_brackets_only_open,
        "{",
        vec![Ok((0, Token::BracketOpen, 1))]
    );

    lexer_test!(
        lex_brackets_only_close,
        "}",
        vec![Ok((0, Token::BracketClose, 1))]
    );

    lexer_test!(
        lex_brackets_empty_pair,
        "{}",
        vec![
            Ok((0, Token::BracketOpen, 1)),
            Ok((1, Token::BracketClose, 2))
        ]
    );

    lexer_test!(
        lex_brackets_inverted_pair,
        "}{",
        vec![
            Ok((0, Token::BracketClose, 1)),
            Ok((1, Token::BracketOpen, 2))
        ]
    );

    lexer_test!(
        lex_brackets_nested_empty_pair,
        "{{}}",
        vec![
            Ok((0, Token::BracketOpen, 1)),
            Ok((1, Token::BracketOpen, 2)),
            Ok((2, Token::BracketClose, 3)),
            Ok((3, Token::BracketClose, 4))
        ]
    );

    lexer_test!(
        lex_brackets_empty_pair_with_newlines,
        "\n{\n}\n",
        vec![
            Ok((1, Token::BracketOpen, 2)),
            Ok((3, Token::BracketClose, 4))
        ]
    );

    lexer_test!(
        lex_brackets_empty_pair_with_semicolons,
        ";{;};",
        vec![
            Ok((0, Token::Semicolon, 1)),
            Ok((1, Token::BracketOpen, 2)),
            Ok((2, Token::Semicolon, 3)),
            Ok((3, Token::BracketClose, 4)),
            Ok((4, Token::Semicolon, 5))
        ]
    );

    lexer_test!(
        lex_brackets_pair_containing_number_expression,
        "{ 123 }",
        vec![
            Ok((0, Token::BracketOpen, 1)),
            Ok((2, Token::Number(123f64), 5)),
            Ok((6, Token::BracketClose, 7))
        ]
    );

    lexer_test!(
        lex_brackets_pair_containing_string_expression,
        r#"{ "foo" }"#,
        vec![
            Ok((0, Token::BracketOpen, 1)),
            Ok((2, Token::String("foo".to_string()), 7)),
            Ok((8, Token::BracketClose, 9))
        ]
    );

    lexer_test!(
        lex_brackets_pair_containing_string_expression_with_brackets,
        r#"{ "f{o}o" }"#,
        vec![
            Ok((0, Token::BracketOpen, 1)),
            Ok((2, Token::String("f{o}o".to_string()), 9)),
            Ok((10, Token::BracketClose, 11))
        ]
    );

    lexer_test!(
        lex_brackets_pair_containing_identifier_expression,
        r#"{ foo }"#,
        vec![
            Ok((0, Token::BracketOpen, 1)),
            Ok((2, Token::Identifier("foo".to_string()), 5)),
            Ok((6, Token::BracketClose, 7))
        ]
    );

    lexer_test!(
        lex_brackets_pair_containing_multiple_expressions,
        r#"{ foo; 123; "bar" }"#,
        vec![
            Ok((0, Token::BracketOpen, 1)),
            Ok((2, Token::Identifier("foo".to_string()), 5)),
            Ok((5, Token::Semicolon, 6)),
            Ok((7, Token::Number(123f64), 10)),
            Ok((10, Token::Semicolon, 11)),
            Ok((12, Token::String("bar".to_string()), 17)),
            Ok((18, Token::BracketClose, 19)),
        ]
    );

    lexer_test!(
        lex_brackets_pair_containing_backet_pairs,
        r#"{{};{}};"#,
        vec![
            Ok((0, Token::BracketOpen, 1)),
            Ok((1, Token::BracketOpen, 2)),
            Ok((2, Token::BracketClose, 3)),
            Ok((3, Token::Semicolon, 4)),
            Ok((4, Token::BracketOpen, 5)),
            Ok((5, Token::BracketClose, 6)),
            Ok((6, Token::BracketClose, 7)),
            Ok((7, Token::Semicolon, 8)),
        ]
    );
}
