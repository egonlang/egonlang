use std::{
    fmt::{self, Display, Formatter},
    sync::Arc,
};

use egonlang_errors::EgonTypeError;
use egonlang_types::Type;
use serde::{Deserialize, Serialize};

use crate::parser::parse;

use span::Spanned;

use super::{Stmt, StmtS};

/// A tuple containing an expression and it's span e.g. (expr, span)
pub type ExprS = Spanned<Arc<Expr>>;

/// Expressions
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum Expr {
    /// Expression returning no value
    ///
    /// e.g. `void`, `()` in other languages
    ///
    /// ```egon
    /// ();
    /// ```
    Unit,
    /// Expression representing literal values e.g. `string`, `number`, `bool`
    ///
    /// ```egon
    /// 123;
    /// "example";
    /// false;
    /// ```
    Literal(ExprLiteral),
    /// Expression representing an identifier
    ///
    /// ```egon
    /// let a: number = 123; // `a` & `number` are identifier expressions
    /// a + 5; // `a` is an identifier expression
    /// ```
    Identifier(ExprIdentifier),
    /// Expression creating a lexical scope that optionally returns a value
    ///
    /// ```egon
    /// let a: () = { 123; };
    /// let b: number = { 456 };
    /// ```
    Block(Box<ExprBlock>),
    /// Expression creating a list of same typed values
    ///
    /// ```egon
    /// [];
    /// [1, 2, 3];
    /// ```
    List(Box<ExprList>),
    /// Expression creating a fixed sized collection of mixed typed values
    ///
    /// ```egon
    /// (1,);
    /// (1, 2, 3,);
    /// ```
    Tuple(Box<ExprTuple>),
    /// Expression preforming an infix operation
    ///
    /// ```egon
    /// 1 + 2;
    /// true != false;
    /// ```
    Infix(Box<ExprInfix>),
    /// Expression preforming a prefix operation
    ///
    /// ```egon
    /// -10;
    /// !false;
    /// ```
    Prefix(Box<ExprPrefix>),
    /// Expression for assigning a value expression to an identifier
    ///
    /// ```egon
    /// let a: number;
    /// a = 123;
    /// ```
    ///
    /// This is not allowed for constants
    ///
    /// ```egon
    /// const a: number = 123;
    /// a = 1; // This will generate an error
    /// ```
    Assign(Box<ExprAssign>),
    /// Expression returning a value based on a condition
    ///
    /// ```egon
    /// let a: number = if (true) { 123 } else { 456 };
    /// ```
    If(Box<ExprIf>),
    /// Expression creating an anonymous function
    ///
    /// ```egon
    /// (a: number): number => { a + 10 };
    /// ```
    Fn(Box<ExprFn>),
    /// Expression creating a range of values
    ///
    /// ```egon
    /// 0..10;
    /// 10..200;
    /// 100..0;
    /// ```
    Range(ExprRange),
    /// Expression representing a type
    ///
    /// ```egon
    /// type Int = number; // `number` is a type expression
    /// ```
    Type(ExprType),
    /// Expression to call the lvalue
    ///
    /// ```egon
    /// a(1, 2, 3);
    /// ```
    Call(Box<ExprCall>),
}

impl Eq for Expr {}

impl std::hash::Hash for Expr {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        core::mem::discriminant(self).hash(state);
    }
}

impl TryFrom<&str> for Expr {
    type Error = egonlang_errors::EgonError;

    fn try_from(value: &str) -> Result<Expr, egonlang_errors::EgonError> {
        let module = parse(value, 0).unwrap();

        let (stmt, _) = module.stmts.first().unwrap();

        if let Stmt::Expr(stmt_expr) = stmt {
            let (expr, _) = &stmt_expr.expr;

            // Return the `Expr` directly, not wrapped in `Rc`
            return Ok((**expr).clone()); // Dereference `Rc<Expr>` and clone `Expr`
        };

        Err(egonlang_errors::EgonSyntaxError::InvalidToken.into())
    }
}

impl From<ExprLiteral> for Expr {
    fn from(value: ExprLiteral) -> Self {
        Expr::Literal(value)
    }
}

impl From<bool> for Expr {
    fn from(value: bool) -> Self {
        Expr::Literal(value.into())
    }
}

impl From<String> for Expr {
    fn from(value: String) -> Self {
        Expr::Literal(value.into())
    }
}

impl From<f64> for Expr {
    fn from(value: f64) -> Self {
        Expr::Literal(value.into())
    }
}

impl From<ExprIdentifier> for Expr {
    fn from(value: ExprIdentifier) -> Self {
        Expr::Identifier(value)
    }
}

impl From<Box<ExprBlock>> for Expr {
    fn from(value: Box<ExprBlock>) -> Self {
        Expr::Block(value)
    }
}

impl From<ExprBlock> for Expr {
    fn from(value: ExprBlock) -> Self {
        Expr::Block(Box::from(value))
    }
}

impl From<ExprList> for Expr {
    fn from(value: ExprList) -> Self {
        Expr::List(value.into())
    }
}

impl From<ExprTuple> for Expr {
    fn from(value: ExprTuple) -> Self {
        Expr::Tuple(value.into())
    }
}

impl From<ExprType> for Expr {
    fn from(value: ExprType) -> Self {
        Expr::Type(value)
    }
}

impl From<ExprPrefix> for Expr {
    fn from(value: ExprPrefix) -> Self {
        Expr::Prefix(Box::from(value))
    }
}

impl From<ExprInfix> for Expr {
    fn from(value: ExprInfix) -> Self {
        Expr::Infix(Box::from(value))
    }
}

impl From<ExprAssign> for Expr {
    fn from(value: ExprAssign) -> Self {
        Expr::Assign(Box::from(value))
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Unit => f.write_fmt(format_args!("()")),
            Expr::Literal(expr) => f.write_fmt(format_args!("{}", expr)),
            Expr::Identifier(expr) => f.write_fmt(format_args!("{}", expr)),
            Expr::Block(expr) => f.write_fmt(format_args!("{}", expr)),
            Expr::List(expr) => f.write_fmt(format_args!("{}", expr)),
            Expr::Tuple(expr) => f.write_fmt(format_args!("{}", expr)),
            Expr::Infix(expr) => f.write_fmt(format_args!("{}", expr)),
            Expr::Prefix(expr) => f.write_fmt(format_args!("{}", expr)),
            Expr::Assign(expr) => f.write_fmt(format_args!("{}", expr)),
            Expr::If(expr) => f.write_fmt(format_args!("{}", expr)),
            Expr::Fn(expr) => f.write_fmt(format_args!("{}", expr)),
            Expr::Range(expr) => f.write_fmt(format_args!("{}", expr)),
            Expr::Type(expr) => f.write_fmt(format_args!("{}", expr)),
            Expr::Call(expr) => f.write_fmt(format_args!("{}", expr)),
        }
    }
}

/// Expression representing literal values e.g. `string`, `number`, `bool`
///
/// ```egon
/// 123;
/// "example";
/// false;
/// ```
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum ExprLiteral {
    Bool(bool),
    Number(f64),
    String(String),
}

impl From<bool> for ExprLiteral {
    fn from(value: bool) -> Self {
        ExprLiteral::Bool(value)
    }
}

impl From<String> for ExprLiteral {
    fn from(value: String) -> Self {
        ExprLiteral::String(value)
    }
}

impl From<f64> for ExprLiteral {
    fn from(value: f64) -> Self {
        ExprLiteral::Number(value)
    }
}

impl TryFrom<Expr> for f64 {
    type Error = Vec<egonlang_errors::EgonError>;

    fn try_from(value: Expr) -> Result<Self, Self::Error> {
        match value {
            Expr::Literal(literal) => match literal {
                ExprLiteral::Number(number) => Ok(number),
                ExprLiteral::Bool(_) => Err(vec![egonlang_errors::EgonTypeError::MismatchType {
                    expected: Type::number().to_string(),
                    actual: Type::bool().to_string(),
                }
                .into()]),
                ExprLiteral::String(_) => Err(vec![egonlang_errors::EgonTypeError::MismatchType {
                    expected: Type::number().to_string(),
                    actual: Type::string().to_string(),
                }
                .into()]),
            },
            Expr::Prefix(prefix) => match prefix.op {
                OpPrefix::Negate => prefix.rt.0.as_ref().clone().try_into(),
                OpPrefix::Not => Err(vec![egonlang_errors::EgonTypeError::MismatchType {
                    expected: Type::number().to_string(),
                    actual: Type::bool().to_string(),
                }
                .into()]),
            },
            _ => Err(vec![egonlang_errors::EgonTypeError::MismatchType {
                expected: Type::number().to_string(),
                actual: value.to_string(),
            }
            .into()]),
        }
    }
}

impl TryFrom<Expr> for String {
    type Error = Vec<egonlang_errors::EgonError>;

    fn try_from(value: Expr) -> Result<Self, Self::Error> {
        match value {
            Expr::Literal(literal) => match literal {
                ExprLiteral::Number(_) => Err(vec![egonlang_errors::EgonTypeError::MismatchType {
                    expected: Type::string().to_string(),
                    actual: Type::number().to_string(),
                }
                .into()]),
                ExprLiteral::Bool(_) => Err(vec![egonlang_errors::EgonTypeError::MismatchType {
                    expected: Type::string().to_string(),
                    actual: Type::bool().to_string(),
                }
                .into()]),
                ExprLiteral::String(string) => Ok(string.to_string()),
            },
            _ => Err(vec![egonlang_errors::EgonTypeError::MismatchType {
                expected: Type::string().to_string(),
                actual: value.to_string(),
            }
            .into()]),
        }
    }
}

impl TryFrom<Expr> for bool {
    type Error = Vec<egonlang_errors::EgonError>;

    fn try_from(value: Expr) -> Result<Self, Self::Error> {
        match value {
            Expr::Literal(literal) => match literal {
                ExprLiteral::Number(_) => Err(vec![EgonTypeError::MismatchType {
                    expected: Type::bool().to_string(),
                    actual: Type::number().to_string(),
                }
                .into()]),
                ExprLiteral::Bool(bool) => Ok(bool),
                ExprLiteral::String(_) => Err(vec![EgonTypeError::MismatchType {
                    expected: Type::bool().to_string(),
                    actual: Type::string().to_string(),
                }
                .into()]),
            },
            _ => Err(vec![EgonTypeError::MismatchType {
                expected: Type::bool().to_string(),
                actual: value.to_string(),
            }
            .into()]),
        }
    }
}

impl Display for ExprLiteral {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            ExprLiteral::Bool(expr) => f.write_fmt(format_args!("{}", expr)),
            ExprLiteral::Number(expr) => f.write_fmt(format_args!("{}", expr)),
            ExprLiteral::String(expr) => f.write_fmt(format_args!(r#""{}""#, expr)),
        }
    }
}

/// Expression representing literal values e.g. `string`, `number`, `bool`
///
/// ```egon
/// 123;
/// "example";
/// false;
/// ```
#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct ExprIdentifier {
    pub identifier: Identifier,
}

impl Display for ExprIdentifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("{}", self.identifier.name))
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct Identifier {
    pub name: String,
}

impl From<Identifier> for ExprIdentifier {
    fn from(value: Identifier) -> Self {
        ExprIdentifier { identifier: value }
    }
}

impl From<Identifier> for Expr {
    fn from(value: Identifier) -> Self {
        Expr::Identifier(value.into())
    }
}

/// Expression creating a lexical scope that optionally returns a value
///
/// ```egon
/// let a: () = { 123; };
/// let b: number = { 456 };
/// ```
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct ExprBlock {
    pub stmts: Vec<StmtS>,
    pub return_expr: Option<ExprS>,
    /// The block's resolved type
    ///
    /// ```egon
    /// let a: number = {
    ///   let b = 123;
    ///
    ///   b // The block's resolved type is `number`
    /// };
    /// ```
    pub typeref: Option<Type>,
}

impl Display for ExprBlock {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let return_expr = &self.return_expr;

        let stmts: String = self
            .stmts
            .clone()
            .into_iter()
            .map(|(stmt, _)| format!("{stmt}"))
            .collect::<Vec<String>>()
            .join(" ");

        let stmts = if stmts.is_empty() {
            stmts
        } else {
            format!("{stmts} ")
        };

        if return_expr.is_some() {
            f.write_fmt(format_args!(
                r"{{ {}{} }}",
                stmts,
                return_expr.as_ref().unwrap().0
            ))
        } else if stmts.is_empty() {
            f.write_fmt(format_args!(r"{{}}"))
        } else {
            f.write_fmt(format_args!(r"{{ {} }}", stmts))
        }
    }
}

/// Expression creating a list of same typed values
///
/// ```egon
/// [];
/// [1, 2, 3];
/// ```
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct ExprList {
    pub items: Vec<ExprS>,
}

impl Display for ExprList {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!(
            "[{}]",
            self.items
                .clone()
                .into_iter()
                .map(|(item, _)| format!("{item}"))
                .collect::<Vec<String>>()
                .join(", ")
        ))
    }
}

/// Expression creating a fixed sized collection of mixed typed values
///
/// ```egon
/// (1,);
/// (1, 2, 3,);
/// ```
#[derive(Clone, Debug, PartialEq, Default, Serialize, Deserialize)]
pub struct ExprTuple {
    pub items: Vec<ExprS>,
}

impl Display for ExprTuple {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!(
            "({},)",
            self.items
                .clone()
                .into_iter()
                .map(|(item, _)| format!("{item}"))
                .collect::<Vec<String>>()
                .join(", ")
        ))
    }
}

/// Expression preforming an infix operation
///
/// ```egon
/// 1 + 2;
/// true != false;
/// ```
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct ExprInfix {
    pub lt: ExprS,
    pub op: OpInfix,
    pub rt: ExprS,
}

impl Display for ExprInfix {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let left = &self.lt.0;
        let op = &self.op;
        let right = &self.rt.0;
        f.write_fmt(format_args!("{} {} {}", left, op, right))
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Serialize, Deserialize)]
pub enum OpInfix {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulus,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Equal,
    NotEqual,
    LogicAnd,
    LogicOr,
}

impl Display for OpInfix {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            OpInfix::Add => f.write_fmt(format_args!("+")),
            OpInfix::Subtract => f.write_fmt(format_args!("-")),
            OpInfix::Multiply => f.write_fmt(format_args!("*")),
            OpInfix::Divide => f.write_fmt(format_args!("/")),
            OpInfix::Modulus => f.write_fmt(format_args!("%")),
            OpInfix::Less => f.write_fmt(format_args!("<")),
            OpInfix::LessEqual => f.write_fmt(format_args!("<=")),
            OpInfix::Greater => f.write_fmt(format_args!(">")),
            OpInfix::GreaterEqual => f.write_fmt(format_args!(">=")),
            OpInfix::Equal => f.write_fmt(format_args!("==")),
            OpInfix::NotEqual => f.write_fmt(format_args!("!=")),
            OpInfix::LogicAnd => f.write_fmt(format_args!("and")),
            OpInfix::LogicOr => f.write_fmt(format_args!("or")),
        }
    }
}

/// Expression preforming a prefix operation
///
/// ```egon
/// -10;
/// !false;
/// ```
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct ExprPrefix {
    pub op: OpPrefix,
    pub rt: ExprS,
}

impl Display for ExprPrefix {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let prefix = &self.op;
        let value = &self.rt.0;
        f.write_fmt(format_args!("{}{}", prefix, value))
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Serialize, Deserialize)]
pub enum OpPrefix {
    Negate,
    Not,
}

impl Display for OpPrefix {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            OpPrefix::Negate => f.write_fmt(format_args!("-")),
            OpPrefix::Not => f.write_fmt(format_args!("!")),
        }
    }
}

/// Expression for assigning a value expression to an identifier
///
/// ```egon
/// let a: number;
/// a = 123;
/// ```
///
/// This is not allowed for constants
///
/// ```egon
/// const a: number = 123;
/// a = 1; // This will generate an error
/// ```
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct ExprAssign {
    pub identifier: Spanned<Identifier>,
    pub value: ExprS,
}

impl Display for ExprAssign {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!(
            "{} = {}",
            self.identifier.0.name, self.value.0
        ))
    }
}

/// Expression returning a value based on a condition
///
/// ```egon
/// let a: number = if (true) { 123 } else { 456 };
/// ```
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct ExprIf {
    pub cond: ExprS,
    pub then: ExprS,
    pub else_: Option<ExprS>,
}

impl Display for ExprIf {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if self.else_.is_none() {
            f.write_fmt(format_args!("if ({}) {}", self.cond.0, self.then.0))
        } else {
            f.write_fmt(format_args!(
                "if ({}) {} else {}",
                self.cond.0,
                self.then.0,
                self.else_.as_ref().unwrap().0
            ))
        }
    }
}

/// Expression creating an anonymous function
///
/// ```egon
/// (a: number): number => { a + 10 };
/// ```
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct ExprFn {
    pub name: Option<Identifier>,
    pub params: Vec<Spanned<(Identifier, Type)>>,
    pub return_type: Spanned<Type>,
    pub body: ExprS,
}

impl Display for ExprFn {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let params: String = self
            .params
            .clone()
            .into_iter()
            .map(|(param, _)| format!("{}: {}", param.0.name, param.1))
            .collect::<Vec<String>>()
            .join(", ");

        let typeref = &self.return_type.0;
        let body = &self.body.0;

        f.write_fmt(format_args!("({}): {} => {}", params, typeref, body))
    }
}

/// Expression creating a range of values
///
/// ```egon
/// 0..10;
/// 10..200;
/// 100..0;
/// ```
#[derive(Clone, Debug, PartialEq, Default, Serialize, Deserialize)]
pub struct ExprRange {
    pub start: Option<Spanned<ExprLiteral>>,
    pub end: Option<Spanned<ExprLiteral>>,
    pub inclusive_end: bool,
}

impl Display for ExprRange {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!(
            "{}..{}",
            self.start.as_ref().unwrap().0,
            self.end.as_ref().unwrap().0
        ))
    }
}

/// Expression representing a type
///
/// ```egon
/// let a: string = "example"; // `string` is a type expression
/// type Int = number; // `number` is a type expression
/// ```
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct ExprType(pub Type);

impl Display for ExprType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("{}", self.0))
    }
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct ExprCall {
    pub callee: ExprS,
    pub args: Vec<ExprS>,
}

impl Display for ExprCall {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!(
            "{}({})",
            self.callee.0,
            self.args
                .iter()
                .map(|x| x.0.to_string())
                .collect::<Vec<String>>()
                .join(", ")
        ))
    }
}

#[cfg(test)]
mod into_tests {
    macro_rules! expr_from_test {
        ($test_name:ident, $expr:expr, $expected:expr) => {
            #[test]
            fn $test_name() {
                let expr: $crate::ast::Expr = $expr.into();
                let expected: $crate::ast::Expr = $expected;

                ::pretty_assertions::assert_eq!(expected, expr);
            }
        };
    }

    expr_from_test!(
        expr_from_bool_true,
        true,
        crate::ast::ExprLiteral::Bool(true).into()
    );

    expr_from_test!(
        expr_from_bool_false,
        false,
        crate::ast::ExprLiteral::Bool(false).into()
    );

    expr_from_test!(
        expr_from_empty_string,
        "".to_string(),
        crate::ast::ExprLiteral::String("".to_string()).into()
    );

    expr_from_test!(
        expr_from_string,
        "example".to_string(),
        crate::ast::ExprLiteral::String("example".to_string()).into()
    );

    expr_from_test!(
        expr_number_literal_from_zero,
        0f64,
        crate::ast::ExprLiteral::Number(0f64).into()
    );

    expr_from_test!(
        expr_number_literal_from_number,
        123f64,
        crate::ast::ExprLiteral::Number(123f64).into()
    );
}

#[cfg(test)]
mod display_tests {
    use std::vec;

    use egonlang_types::Type;
    use pretty_assertions::assert_eq;

    use crate::ast::*;

    macro_rules! expr_display_test {
        ($test_name:ident, $expr:expr, $expected:expr) => {
            #[test]
            fn $test_name() {
                let expr: $crate::ast::Expr = $expr;
                assert_eq!($expected, expr.to_string());
            }
        };
    }

    expr_display_test!(test_expr_display_unit, Expr::Unit, "()");

    expr_display_test!(
        test_expr_literal_bool_true,
        ExprLiteral::Bool(true).into(),
        "true"
    );

    expr_display_test!(
        test_expr_literal_bool_false,
        ExprLiteral::Bool(false).into(),
        "false"
    );

    expr_display_test!(
        test_expr_literal_number_zero,
        ExprLiteral::Number(0f64).into(),
        "0"
    );

    expr_display_test!(
        test_expr_literal_number_positive,
        ExprLiteral::Number(3842f64).into(),
        "3842"
    );

    expr_display_test!(
        test_expr_literal_number_negative,
        ExprLiteral::Number(-1546f64).into(),
        "-1546"
    );

    expr_display_test!(
        test_expr_literal_number_float,
        ExprLiteral::Number(987.123f64).into(),
        "987.123"
    );

    expr_display_test!(
        test_expr_identifier,
        Identifier {
            name: "foo".to_string()
        }
        .into(),
        "foo"
    );

    expr_display_test!(
        test_expr_range,
        Expr::Range(ExprRange {
            start: Some((ExprLiteral::Number(0f64), 0..0)),
            end: Some((ExprLiteral::Number(10f64), 0..0)),
            inclusive_end: false
        }),
        "0..10"
    );

    expr_display_test!(
        test_expr_assign,
        Expr::Assign(Box::new(ExprAssign {
            identifier: (
                Identifier {
                    name: "foo".to_string()
                },
                0..0
            ),
            value: (Expr::Literal(ExprLiteral::Number(123f64)).into(), 0..0)
        })),
        "foo = 123"
    );

    expr_display_test!(
        test_expr_prefix_negate,
        Expr::Prefix(Box::new(ExprPrefix {
            op: OpPrefix::Negate,
            rt: (Expr::Literal(ExprLiteral::Number(43557f64)).into(), 0..0)
        })),
        "-43557"
    );

    expr_display_test!(
        test_expr_prefix_not,
        Expr::Prefix(Box::new(ExprPrefix {
            op: OpPrefix::Not,
            rt: (Expr::Literal(ExprLiteral::Bool(false)).into(), 0..0)
        })),
        "!false"
    );

    expr_display_test!(
        test_expr_infix_add,
        Expr::Infix(Box::new(ExprInfix {
            lt: (Expr::Literal(ExprLiteral::Number(100f64)).into(), 0..0),
            op: OpInfix::Add,
            rt: (Expr::Literal(ExprLiteral::Number(55f64)).into(), 0..0)
        })),
        "100 + 55"
    );

    expr_display_test!(
        test_expr_infix_subtract,
        Expr::Infix(Box::new(ExprInfix {
            lt: (Expr::Literal(ExprLiteral::Number(100f64)).into(), 0..0),
            op: OpInfix::Subtract,
            rt: (Expr::Literal(ExprLiteral::Number(55f64)).into(), 0..0)
        })),
        "100 - 55"
    );

    expr_display_test!(
        test_expr_infix_multiply,
        Expr::Infix(Box::new(ExprInfix {
            lt: (Expr::Literal(ExprLiteral::Number(100f64)).into(), 0..0),
            op: OpInfix::Multiply,
            rt: (Expr::Literal(ExprLiteral::Number(55f64)).into(), 0..0)
        })),
        "100 * 55"
    );

    expr_display_test!(
        test_expr_infix_divide,
        Expr::Infix(Box::new(ExprInfix {
            lt: (Expr::Literal(ExprLiteral::Number(100f64)).into(), 0..0),
            op: OpInfix::Divide,
            rt: (Expr::Literal(ExprLiteral::Number(55f64)).into(), 0..0)
        })),
        "100 / 55"
    );

    expr_display_test!(
        test_expr_infix_modulus,
        Expr::Infix(Box::new(ExprInfix {
            lt: (Expr::Literal(ExprLiteral::Number(100f64)).into(), 0..0),
            op: OpInfix::Modulus,
            rt: (Expr::Literal(ExprLiteral::Number(55f64)).into(), 0..0)
        })),
        "100 % 55"
    );

    expr_display_test!(
        test_expr_infix_eq,
        Expr::Infix(Box::new(ExprInfix {
            lt: (Expr::Literal(ExprLiteral::Number(100f64)).into(), 0..0),
            op: OpInfix::Equal,
            rt: (Expr::Literal(ExprLiteral::Number(55f64)).into(), 0..0)
        })),
        "100 == 55"
    );

    expr_display_test!(
        test_expr_infix_not_eq,
        Expr::Infix(Box::new(ExprInfix {
            lt: (Expr::Literal(ExprLiteral::Number(100f64)).into(), 0..0),
            op: OpInfix::NotEqual,
            rt: (Expr::Literal(ExprLiteral::Number(55f64)).into(), 0..0)
        })),
        "100 != 55"
    );

    expr_display_test!(
        test_expr_infix_gt,
        Expr::Infix(Box::new(ExprInfix {
            lt: (Expr::Literal(ExprLiteral::Number(100f64)).into(), 0..0),
            op: OpInfix::Greater,
            rt: (Expr::Literal(ExprLiteral::Number(55f64)).into(), 0..0)
        })),
        "100 > 55"
    );

    expr_display_test!(
        test_expr_infix_lt,
        Expr::Infix(Box::new(ExprInfix {
            lt: (Expr::Literal(ExprLiteral::Number(100f64)).into(), 0..0),
            op: OpInfix::Less,
            rt: (Expr::Literal(ExprLiteral::Number(55f64)).into(), 0..0)
        })),
        "100 < 55"
    );

    expr_display_test!(
        test_expr_infix_gte,
        Expr::Infix(Box::new(ExprInfix {
            lt: (Expr::Literal(ExprLiteral::Number(100f64)).into(), 0..0),
            op: OpInfix::GreaterEqual,
            rt: (Expr::Literal(ExprLiteral::Number(55f64)).into(), 0..0)
        })),
        "100 >= 55"
    );

    expr_display_test!(
        test_expr_infix_lte,
        Expr::Infix(Box::new(ExprInfix {
            lt: (Expr::Literal(ExprLiteral::Number(100f64)).into(), 0..0),
            op: OpInfix::LessEqual,
            rt: (Expr::Literal(ExprLiteral::Number(55f64)).into(), 0..0)
        })),
        "100 <= 55"
    );

    expr_display_test!(
        test_expr_infix_and,
        Expr::Infix(Box::new(ExprInfix {
            lt: (Expr::Literal(ExprLiteral::Bool(true)).into(), 0..0),
            op: OpInfix::LogicAnd,
            rt: (Expr::Literal(ExprLiteral::Bool(false)).into(), 0..0)
        })),
        "true and false"
    );

    expr_display_test!(
        test_expr_infix_or,
        Expr::Infix(Box::new(ExprInfix {
            lt: (Expr::Literal(ExprLiteral::Number(100f64)).into(), 0..0),
            op: OpInfix::LogicOr,
            rt: (Expr::Literal(ExprLiteral::Number(55f64)).into(), 0..0)
        })),
        "100 or 55"
    );

    expr_display_test!(
        test_expr_if,
        Expr::If(Box::new(ExprIf {
            cond: (Expr::Literal(ExprLiteral::Bool(true)).into(), 0..0),
            then: (
                Expr::Block(
                    ExprBlock {
                        stmts: vec![],
                        return_expr: Some((
                            Expr::Literal(ExprLiteral::Number(123f64)).into(),
                            0..0
                        )),
                        typeref: None
                    }
                    .into()
                )
                .into(),
                0..0
            ),
            else_: None
        })),
        "if (true) { 123 }"
    );

    expr_display_test!(
        test_expr_if_with_stmts,
        Expr::If(Box::new(ExprIf {
            cond: (Expr::Literal(ExprLiteral::Bool(true)).into(), 0..0),
            then: (
                Expr::Block(
                    ExprBlock {
                        stmts: vec![(
                            StmtExpr {
                                expr: (Expr::Literal(ExprLiteral::Number(123f64)).into(), 0..0)
                            }
                            .into(),
                            0..0
                        )],
                        return_expr: Some((
                            Expr::Literal(ExprLiteral::Number(456f64)).into(),
                            0..0
                        )),
                        typeref: None
                    }
                    .into()
                )
                .into(),
                0..0
            ),
            else_: None
        })),
        "if (true) { 123; 456 }"
    );

    expr_display_test!(
        test_expr_if_else,
        Expr::If(Box::new(ExprIf {
            cond: (Expr::Literal(ExprLiteral::Bool(true)).into(), 0..0),
            then: (
                Expr::Block(
                    ExprBlock {
                        stmts: vec![],
                        return_expr: Some((
                            Expr::Literal(ExprLiteral::Number(123f64)).into(),
                            0..0
                        )),
                        typeref: None
                    }
                    .into()
                )
                .into(),
                0..0
            ),
            else_: Some((
                Expr::Block(
                    ExprBlock {
                        stmts: vec![],
                        return_expr: Some((
                            Expr::Literal(ExprLiteral::Number(456f64)).into(),
                            0..0
                        )),
                        typeref: None
                    }
                    .into()
                )
                .into(),
                0..0
            ))
        })),
        "if (true) { 123 } else { 456 }"
    );

    expr_display_test!(
        test_expr_if_else_with_stmts,
        Expr::If(Box::new(ExprIf {
            cond: (Expr::Literal(ExprLiteral::Bool(true)).into(), 0..0),
            then: (
                Expr::Block(
                    ExprBlock {
                        stmts: vec![(
                            StmtExpr {
                                expr: (Expr::Literal(ExprLiteral::Number(123f64)).into(), 0..0)
                            }
                            .into(),
                            0..0
                        )],
                        return_expr: Some((
                            Expr::Literal(ExprLiteral::Number(456f64)).into(),
                            0..0
                        )),
                        typeref: None
                    }
                    .into()
                )
                .into(),
                0..0
            ),
            else_: Some((
                Expr::Block(
                    ExprBlock {
                        stmts: vec![(
                            StmtExpr {
                                expr: (Expr::Literal(ExprLiteral::Number(789f64)).into(), 0..0)
                            }
                            .into(),
                            0..0
                        )],
                        return_expr: Some((Expr::Literal(ExprLiteral::Number(0f64)).into(), 0..0)),
                        typeref: None
                    }
                    .into()
                )
                .into(),
                0..0
            ))
        })),
        "if (true) { 123; 456 } else { 789; 0 }"
    );

    expr_display_test!(
        test_expr_empty_list,
        Expr::List(ExprList { items: vec![] }.into()),
        "[]"
    );

    expr_display_test!(
        test_expr_list,
        Expr::List(
            ExprList {
                items: vec![
                    (Expr::Literal(ExprLiteral::Number(1f64)).into(), 0..0),
                    (Expr::Literal(ExprLiteral::Number(2f64)).into(), 0..0),
                    (Expr::Literal(ExprLiteral::Number(3f64)).into(), 0..0)
                ]
            }
            .into()
        ),
        "[1, 2, 3]"
    );

    expr_display_test!(
        test_expr_tuple,
        Expr::Tuple(
            ExprTuple {
                items: vec![
                    (Expr::Literal(ExprLiteral::Number(1f64)).into(), 0..0),
                    (Expr::Literal(ExprLiteral::Number(2f64)).into(), 0..0),
                    (Expr::Literal(ExprLiteral::Number(3f64)).into(), 0..0)
                ]
            }
            .into()
        ),
        "(1, 2, 3,)"
    );

    expr_display_test!(
        test_expr_fn,
        Expr::Fn(Box::new(ExprFn {
            name: None,
            params: vec![],
            return_type: (Type::unit(), 0..0),
            body: (
                Expr::Block(
                    ExprBlock {
                        stmts: vec![],
                        return_expr: None,
                        typeref: None
                    }
                    .into()
                )
                .into(),
                0..0
            )
        })),
        "(): () => {}"
    );

    expr_display_test!(
        test_expr_fn_with_param,
        Expr::Fn(Box::new(ExprFn {
            name: None,
            params: vec![(
                (
                    Identifier {
                        name: "a".to_string()
                    },
                    Type::number()
                ),
                0..0
            )],
            return_type: (Type::unit(), 0..0),
            body: (
                Expr::Block(
                    ExprBlock {
                        stmts: vec![],
                        return_expr: None,
                        typeref: None
                    }
                    .into()
                )
                .into(),
                0..0
            )
        })),
        "(a: number): () => {}"
    );

    expr_display_test!(
        test_expr_fn_with_params,
        Expr::Fn(Box::new(ExprFn {
            name: None,
            params: vec![
                (
                    (
                        Identifier {
                            name: "a".to_string()
                        },
                        Type::number()
                    ),
                    0..0
                ),
                (
                    (
                        Identifier {
                            name: "b".to_string()
                        },
                        Type::number()
                    ),
                    0..0
                )
            ],
            return_type: (Type::unit(), 0..0),
            body: (
                Expr::Block(
                    ExprBlock {
                        stmts: vec![],
                        return_expr: None,
                        typeref: None
                    }
                    .into()
                )
                .into(),
                0..0
            )
        })),
        "(a: number, b: number): () => {}"
    );

    expr_display_test!(
        test_expr_fn_with_params_and_return_type,
        Expr::Fn(Box::new(ExprFn {
            name: None,
            params: vec![
                (
                    (
                        Identifier {
                            name: "a".to_string()
                        },
                        Type::number()
                    ),
                    0..0
                ),
                (
                    (
                        Identifier {
                            name: "b".to_string()
                        },
                        Type::number()
                    ),
                    0..0
                )
            ],
            return_type: (Type::number(), 0..0),
            body: (
                Expr::Block(
                    ExprBlock {
                        stmts: vec![],
                        return_expr: None,
                        typeref: None
                    }
                    .into()
                )
                .into(),
                0..0
            )
        })),
        "(a: number, b: number): number => {}"
    );

    expr_display_test!(
        test_expr_fn_with_params_and_return_type_and_body_return_expr,
        Expr::Fn(Box::new(ExprFn {
            name: None,
            params: vec![
                (
                    (
                        Identifier {
                            name: "a".to_string()
                        },
                        Type::number()
                    ),
                    0..0
                ),
                (
                    (
                        Identifier {
                            name: "b".to_string()
                        },
                        Type::number()
                    ),
                    0..0
                )
            ],
            return_type: (Type::number(), 0..0),
            body: (
                Expr::Block(
                    ExprBlock {
                        stmts: vec![],
                        return_expr: Some((
                            Expr::Infix(Box::new(ExprInfix {
                                lt: (
                                    Expr::Identifier(ExprIdentifier {
                                        identifier: Identifier {
                                            name: "a".to_string()
                                        }
                                    })
                                    .into(),
                                    0..0
                                ),
                                op: OpInfix::Add,
                                rt: (
                                    Expr::Identifier(ExprIdentifier {
                                        identifier: Identifier {
                                            name: "b".to_string()
                                        }
                                    })
                                    .into(),
                                    0..0
                                )
                            }))
                            .into(),
                            0..0
                        )),
                        typeref: None
                    }
                    .into()
                )
                .into(),
                0..0
            )
        })),
        "(a: number, b: number): number => { a + b }"
    );

    expr_display_test!(
        test_call,
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
        ),
        "foo(1, 2, 3)"
    );
}
