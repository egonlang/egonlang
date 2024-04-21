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
    #[token(";")]
    Semicolon,
    #[token(",")]
    Comma,
    #[token("{")]
    BraceOpen,
    #[token("}")]
    BraceClose,
    #[token("[")]
    BracketOpen,
    #[token("]")]
    BracketClose,
    #[token("(")]
    ParanOpen,
    #[token(")")]
    ParanClose,
    #[token("=>")]
    FatArrow,
    #[token("..")]
    DotDot,
    #[token(":")]
    Colon,

    // Binary Ops
    #[token("-")]
    Minus,
    #[token("+")]
    Plus,
    #[token("/")]
    Slash,
    #[token("*")]
    Asterisk,
    #[token("%")]
    Modulus,
    #[token("!")]
    Bang,
    #[token("!=")]
    BangEqual,
    #[token("=")]
    Equal,
    #[token("==")]
    EqualEqual,
    #[token(">")]
    Greater,
    #[token(">=")]
    GreaterEqual,
    #[token("<")]
    Less,
    #[token("<=")]
    LessEqual,

    // Keywords
    #[token("true")]
    True,
    #[token("false")]
    False,
    #[token("and")]
    And,
    #[token("or")]
    Or,
    #[token("if")]
    If,
    #[token("else")]
    Else,
    #[token("fn")]
    Fn,
    #[token("let")]
    Let,
    #[token("const")]
    Const,
    #[token("type")]
    Type,

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
    use std::vec;

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

    lexer_test!(lex_boolean_true, "true", vec![Ok((0, Token::True, 4))]);

    lexer_test!(lex_boolean_false, "false", vec![Ok((0, Token::False, 5))]);

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
        lex_braces_only_open,
        "{",
        vec![Ok((0, Token::BraceOpen, 1))]
    );

    lexer_test!(
        lex_braces_only_close,
        "}",
        vec![Ok((0, Token::BraceClose, 1))]
    );

    lexer_test!(
        lex_braces_empty_pair,
        "{}",
        vec![Ok((0, Token::BraceOpen, 1)), Ok((1, Token::BraceClose, 2))]
    );

    lexer_test!(
        lex_braces_inverted_pair,
        "}{",
        vec![Ok((0, Token::BraceClose, 1)), Ok((1, Token::BraceOpen, 2))]
    );

    lexer_test!(
        lex_braces_nested_empty_pair,
        "{{}}",
        vec![
            Ok((0, Token::BraceOpen, 1)),
            Ok((1, Token::BraceOpen, 2)),
            Ok((2, Token::BraceClose, 3)),
            Ok((3, Token::BraceClose, 4))
        ]
    );

    lexer_test!(
        lex_braces_empty_pair_with_newlines,
        "\n{\n}\n",
        vec![Ok((1, Token::BraceOpen, 2)), Ok((3, Token::BraceClose, 4))]
    );

    lexer_test!(
        lex_braces_empty_pair_with_semicolons,
        ";{;};",
        vec![
            Ok((0, Token::Semicolon, 1)),
            Ok((1, Token::BraceOpen, 2)),
            Ok((2, Token::Semicolon, 3)),
            Ok((3, Token::BraceClose, 4)),
            Ok((4, Token::Semicolon, 5))
        ]
    );

    lexer_test!(
        lex_braces_pair_containing_number_expression,
        "{ 123 }",
        vec![
            Ok((0, Token::BraceOpen, 1)),
            Ok((2, Token::Number(123f64), 5)),
            Ok((6, Token::BraceClose, 7))
        ]
    );

    lexer_test!(
        lex_braces_pair_containing_string_expression,
        r#"{ "foo" }"#,
        vec![
            Ok((0, Token::BraceOpen, 1)),
            Ok((2, Token::String("foo".to_string()), 7)),
            Ok((8, Token::BraceClose, 9))
        ]
    );

    lexer_test!(
        lex_braces_pair_containing_string_expression_with_braces,
        r#"{ "f{o}o" }"#,
        vec![
            Ok((0, Token::BraceOpen, 1)),
            Ok((2, Token::String("f{o}o".to_string()), 9)),
            Ok((10, Token::BraceClose, 11))
        ]
    );

    lexer_test!(
        lex_braces_pair_containing_identifier_expression,
        r#"{ foo }"#,
        vec![
            Ok((0, Token::BraceOpen, 1)),
            Ok((2, Token::Identifier("foo".to_string()), 5)),
            Ok((6, Token::BraceClose, 7))
        ]
    );

    lexer_test!(
        lex_braces_pair_containing_multiple_expressions,
        r#"{ foo; 123; "bar"; true; false; }"#,
        vec![
            Ok((0, Token::BraceOpen, 1)),
            Ok((2, Token::Identifier("foo".to_string()), 5)),
            Ok((5, Token::Semicolon, 6)),
            Ok((7, Token::Number(123f64), 10)),
            Ok((10, Token::Semicolon, 11)),
            Ok((12, Token::String("bar".to_string()), 17)),
            Ok((17, Token::Semicolon, 18)),
            Ok((19, Token::True, 23)),
            Ok((23, Token::Semicolon, 24)),
            Ok((25, Token::False, 30)),
            Ok((30, Token::Semicolon, 31)),
            Ok((32, Token::BraceClose, 33)),
        ]
    );

    lexer_test!(
        lex_braces_pair_containing_backet_pairs,
        r#"{{};{}};"#,
        vec![
            Ok((0, Token::BraceOpen, 1)),
            Ok((1, Token::BraceOpen, 2)),
            Ok((2, Token::BraceClose, 3)),
            Ok((3, Token::Semicolon, 4)),
            Ok((4, Token::BraceOpen, 5)),
            Ok((5, Token::BraceClose, 6)),
            Ok((6, Token::BraceClose, 7)),
            Ok((7, Token::Semicolon, 8)),
        ]
    );

    lexer_test!(
        lex_brackets_empty_pair,
        "[]",
        vec![
            Ok((0, Token::BracketOpen, 1)),
            Ok((1, Token::BracketClose, 2)),
        ]
    );

    lexer_test!(
        lex_brackets_with_items,
        "[a, b, c]",
        vec![
            Ok((0, Token::BracketOpen, 1)),
            Ok((1, Token::Identifier("a".to_string()), 2)),
            Ok((2, Token::Comma, 3)),
            Ok((4, Token::Identifier("b".to_string()), 5)),
            Ok((5, Token::Comma, 6)),
            Ok((7, Token::Identifier("c".to_string()), 8)),
            Ok((8, Token::BracketClose, 9)),
        ]
    );

    lexer_test!(
        lex_brackets_with_items_trailing_comma,
        "[a, b, c,]",
        vec![
            Ok((0, Token::BracketOpen, 1)),
            Ok((1, Token::Identifier("a".to_string()), 2)),
            Ok((2, Token::Comma, 3)),
            Ok((4, Token::Identifier("b".to_string()), 5)),
            Ok((5, Token::Comma, 6)),
            Ok((7, Token::Identifier("c".to_string()), 8)),
            Ok((8, Token::Comma, 9)),
            Ok((9, Token::BracketClose, 10)),
        ]
    );

    lexer_test!(
        lex_brackets_with_items_nested_brackets,
        "[a, [b], c,]",
        vec![
            Ok((0, Token::BracketOpen, 1)),
            Ok((1, Token::Identifier("a".to_string()), 2)),
            Ok((2, Token::Comma, 3)),
            Ok((4, Token::BracketOpen, 5)),
            Ok((5, Token::Identifier("b".to_string()), 6)),
            Ok((6, Token::BracketClose, 7)),
            Ok((7, Token::Comma, 8)),
            Ok((9, Token::Identifier("c".to_string()), 10)),
            Ok((10, Token::Comma, 11)),
            Ok((11, Token::BracketClose, 12)),
        ]
    );

    lexer_test!(
        lex_prefix_bang,
        "!a",
        vec![
            Ok((0, Token::Bang, 1)),
            Ok((1, Token::Identifier("a".to_string()), 2))
        ]
    );

    lexer_test!(
        lex_prefix_minus,
        "-a",
        vec![
            Ok((0, Token::Minus, 1)),
            Ok((1, Token::Identifier("a".to_string()), 2))
        ]
    );

    lexer_test!(
        lex_infix_plus,
        "a+b",
        vec![
            Ok((0, Token::Identifier("a".to_string()), 1)),
            Ok((1, Token::Plus, 2)),
            Ok((2, Token::Identifier("b".to_string()), 3))
        ]
    );

    lexer_test!(
        lex_infix_minus,
        "a-b",
        vec![
            Ok((0, Token::Identifier("a".to_string()), 1)),
            Ok((1, Token::Minus, 2)),
            Ok((2, Token::Identifier("b".to_string()), 3))
        ]
    );

    lexer_test!(
        lex_infix_multiply,
        "a*b",
        vec![
            Ok((0, Token::Identifier("a".to_string()), 1)),
            Ok((1, Token::Asterisk, 2)),
            Ok((2, Token::Identifier("b".to_string()), 3))
        ]
    );

    lexer_test!(
        lex_infix_divide,
        "a/b",
        vec![
            Ok((0, Token::Identifier("a".to_string()), 1)),
            Ok((1, Token::Slash, 2)),
            Ok((2, Token::Identifier("b".to_string()), 3))
        ]
    );

    lexer_test!(
        lex_infix_modulus,
        "a%b",
        vec![
            Ok((0, Token::Identifier("a".to_string()), 1)),
            Ok((1, Token::Modulus, 2)),
            Ok((2, Token::Identifier("b".to_string()), 3))
        ]
    );

    lexer_test!(
        lex_infix_equal,
        "a=b",
        vec![
            Ok((0, Token::Identifier("a".to_string()), 1)),
            Ok((1, Token::Equal, 2)),
            Ok((2, Token::Identifier("b".to_string()), 3))
        ]
    );

    lexer_test!(
        lex_infix_bang_equal,
        "a!=b",
        vec![
            Ok((0, Token::Identifier("a".to_string()), 1)),
            Ok((1, Token::BangEqual, 3)),
            Ok((3, Token::Identifier("b".to_string()), 4))
        ]
    );

    lexer_test!(
        lex_infix_equal_equal,
        "a==b",
        vec![
            Ok((0, Token::Identifier("a".to_string()), 1)),
            Ok((1, Token::EqualEqual, 3)),
            Ok((3, Token::Identifier("b".to_string()), 4))
        ]
    );

    lexer_test!(
        lex_infix_gt,
        "a>b",
        vec![
            Ok((0, Token::Identifier("a".to_string()), 1)),
            Ok((1, Token::Greater, 2)),
            Ok((2, Token::Identifier("b".to_string()), 3))
        ]
    );

    lexer_test!(
        lex_infix_lt,
        "a<b",
        vec![
            Ok((0, Token::Identifier("a".to_string()), 1)),
            Ok((1, Token::Less, 2)),
            Ok((2, Token::Identifier("b".to_string()), 3))
        ]
    );

    lexer_test!(
        lex_infix_gte,
        "a>=b",
        vec![
            Ok((0, Token::Identifier("a".to_string()), 1)),
            Ok((1, Token::GreaterEqual, 3)),
            Ok((3, Token::Identifier("b".to_string()), 4))
        ]
    );

    lexer_test!(
        lex_infix_lte,
        "a<=b",
        vec![
            Ok((0, Token::Identifier("a".to_string()), 1)),
            Ok((1, Token::LessEqual, 3)),
            Ok((3, Token::Identifier("b".to_string()), 4))
        ]
    );

    lexer_test!(
        lex_infix_with_prefix,
        "a - -b",
        vec![
            Ok((0, Token::Identifier("a".to_string()), 1)),
            Ok((2, Token::Minus, 3)),
            Ok((4, Token::Minus, 5)),
            Ok((5, Token::Identifier("b".to_string()), 6))
        ]
    );

    lexer_test!(
        lex_infix_with_infix,
        "a - b + c",
        vec![
            Ok((0, Token::Identifier("a".to_string()), 1)),
            Ok((2, Token::Minus, 3)),
            Ok((4, Token::Identifier("b".to_string()), 5)),
            Ok((6, Token::Plus, 7)),
            Ok((8, Token::Identifier("c".to_string()), 9))
        ]
    );

    lexer_test!(
        lex_infix_and,
        "a and b",
        vec![
            Ok((0, Token::Identifier("a".to_string()), 1)),
            Ok((2, Token::And, 5)),
            Ok((6, Token::Identifier("b".to_string()), 7))
        ]
    );

    lexer_test!(
        lex_infix_or,
        "a or b",
        vec![
            Ok((0, Token::Identifier("a".to_string()), 1)),
            Ok((2, Token::Or, 4)),
            Ok((5, Token::Identifier("b".to_string()), 6))
        ]
    );

    lexer_test!(
        lex_if_cond_no_parans,
        "if true {}",
        vec![
            Ok((0, Token::If, 2)),
            Ok((3, Token::True, 7)),
            Ok((8, Token::BraceOpen, 9)),
            Ok((9, Token::BraceClose, 10))
        ]
    );

    lexer_test!(
        lex_if_cond_parans,
        "if (true) {}",
        vec![
            Ok((0, Token::If, 2)),
            Ok((3, Token::ParanOpen, 4)),
            Ok((4, Token::True, 8)),
            Ok((8, Token::ParanClose, 9)),
            Ok((10, Token::BraceOpen, 11)),
            Ok((11, Token::BraceClose, 12))
        ]
    );

    lexer_test!(
        lex_if_else,
        "if true {} else {}",
        vec![
            Ok((0, Token::If, 2)),
            Ok((3, Token::True, 7)),
            Ok((8, Token::BraceOpen, 9)),
            Ok((9, Token::BraceClose, 10)),
            Ok((11, Token::Else, 15)),
            Ok((16, Token::BraceOpen, 17)),
            Ok((17, Token::BraceClose, 18)),
        ]
    );

    lexer_test!(
        lex_if_else_expr_then_else,
        "if true { 123 } else { 456 }",
        vec![
            Ok((0, Token::If, 2)),
            Ok((3, Token::True, 7)),
            Ok((8, Token::BraceOpen, 9)),
            Ok((10, Token::Number(123f64), 13)),
            Ok((14, Token::BraceClose, 15)),
            Ok((16, Token::Else, 20)),
            Ok((21, Token::BraceOpen, 22)),
            Ok((23, Token::Number(456f64), 26)),
            Ok((27, Token::BraceClose, 28)),
        ]
    );

    lexer_test!(
        lex_if_elif,
        "if true { 123 } else if true { 456 }",
        vec![
            Ok((0, Token::If, 2)),
            Ok((3, Token::True, 7)),
            Ok((8, Token::BraceOpen, 9)),
            Ok((10, Token::Number(123f64), 13)),
            Ok((14, Token::BraceClose, 15)),
            Ok((16, Token::Else, 20)),
            Ok((21, Token::If, 23)),
            Ok((24, Token::True, 28)),
            Ok((29, Token::BraceOpen, 30)),
            Ok((31, Token::Number(456f64), 34)),
            Ok((35, Token::BraceClose, 36)),
        ]
    );

    lexer_test!(
        lex_range_start_and_end,
        "0..10",
        vec![
            Ok((0, Token::Number(0f64), 1)),
            Ok((1, Token::DotDot, 3)),
            Ok((3, Token::Number(10f64), 5))
        ]
    );

    lexer_test!(
        lex_range_start_and_inclusive_end,
        "0..=10",
        vec![
            Ok((0, Token::Number(0f64), 1)),
            Ok((1, Token::DotDot, 3)),
            Ok((3, Token::Equal, 4)),
            Ok((4, Token::Number(10f64), 6))
        ]
    );

    lexer_test!(
        lex_let_decl_with_assign,
        "let a = 123;",
        vec![
            Ok((0, Token::Let, 3)),
            Ok((4, Token::Identifier("a".to_string()), 5)),
            Ok((6, Token::Equal, 7)),
            Ok((8, Token::Number(123f64), 11)),
            Ok((11, Token::Semicolon, 12)),
        ]
    );

    lexer_test!(
        lex_let_decl_typed_with_assign,
        "let a: number = 123;",
        vec![
            Ok((0, Token::Let, 3)),
            Ok((4, Token::Identifier("a".to_string()), 5)),
            Ok((5, Token::Colon, 6)),
            Ok((7, Token::Identifier("number".to_string()), 13)),
            Ok((14, Token::Equal, 15)),
            Ok((16, Token::Number(123f64), 19)),
            Ok((19, Token::Semicolon, 20)),
        ]
    );

    lexer_test!(
        lex_let_decl_without_assign,
        "let a;",
        vec![
            Ok((0, Token::Let, 3)),
            Ok((4, Token::Identifier("a".to_string()), 5)),
            Ok((5, Token::Semicolon, 6)),
        ]
    );

    lexer_test!(
        lex_const_decl_typed_without_assign,
        "const a: number;",
        vec![
            Ok((0, Token::Const, 5)),
            Ok((6, Token::Identifier("a".to_string()), 7)),
            Ok((7, Token::Colon, 8)),
            Ok((9, Token::Identifier("number".to_string()), 15)),
            Ok((15, Token::Semicolon, 16)),
        ]
    );

    lexer_test!(
        lex_const_decl_with_assign,
        "const a = 123;",
        vec![
            Ok((0, Token::Const, 5)),
            Ok((6, Token::Identifier("a".to_string()), 7)),
            Ok((8, Token::Equal, 9)),
            Ok((10, Token::Number(123f64), 13)),
            Ok((13, Token::Semicolon, 14)),
        ]
    );

    lexer_test!(
        lex_const_decl_typed_with_assign,
        "const a: number = 123;",
        vec![
            Ok((0, Token::Const, 5)),
            Ok((6, Token::Identifier("a".to_string()), 7)),
            Ok((7, Token::Colon, 8)),
            Ok((9, Token::Identifier("number".to_string()), 15)),
            Ok((16, Token::Equal, 17)),
            Ok((18, Token::Number(123f64), 21)),
            Ok((21, Token::Semicolon, 22)),
        ]
    );

    lexer_test!(
        lex_const_decl_without_assign,
        "const a;",
        vec![
            Ok((0, Token::Const, 5)),
            Ok((6, Token::Identifier("a".to_string()), 7)),
            Ok((7, Token::Semicolon, 8)),
        ]
    );

    lexer_test!(
        lex_fn_decl,
        "fn sum (a: number, b: number): number => { a + b }",
        vec![
            Ok((0, Token::Fn, 2)),
            Ok((3, Token::Identifier("sum".to_string()), 6)),
            Ok((7, Token::ParanOpen, 8)),
            Ok((8, Token::Identifier("a".to_string()), 9)),
            Ok((9, Token::Colon, 10)),
            Ok((11, Token::Identifier("number".to_string()), 17)),
            Ok((17, Token::Comma, 18)),
            Ok((19, Token::Identifier("b".to_string()), 20)),
            Ok((20, Token::Colon, 21)),
            Ok((22, Token::Identifier("number".to_string()), 28)),
            Ok((28, Token::ParanClose, 29)),
            Ok((29, Token::Colon, 30)),
            Ok((31, Token::Identifier("number".to_string()), 37)),
            Ok((38, Token::FatArrow, 40)),
            Ok((41, Token::BraceOpen, 42)),
            Ok((43, Token::Identifier("a".to_string()), 44)),
            Ok((45, Token::Plus, 46)),
            Ok((47, Token::Identifier("b".to_string()), 48)),
            Ok((49, Token::BraceClose, 50)),
        ]
    );

    lexer_test!(
        lex_type_alias,
        "type NumberList = list<number>;",
        vec![
            Ok((0, Token::Type, 4)),
            Ok((5, Token::Identifier("NumberList".to_string()), 15)),
            Ok((16, Token::Equal, 17)),
            Ok((18, Token::Identifier("list".to_string()), 22)),
            Ok((22, Token::Less, 23)),
            Ok((23, Token::Identifier("number".to_string()), 29)),
            Ok((29, Token::Greater, 30)),
            Ok((30, Token::Semicolon, 31)),
        ]
    );
}
