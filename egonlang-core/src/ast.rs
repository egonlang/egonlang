use crate::span::Spanned;
use std::fmt::{self, Debug, Formatter};

/// Modules are units of code (e.g. variables, functions)
#[derive(Debug, Default, PartialEq)]
pub struct Module {
    pub stmts: Vec<StmtS>,
}

pub type StmtS = Spanned<Stmt>;
pub type ExprS = Spanned<Expr>;

/// Statements
#[derive(Clone, PartialEq)]
pub enum Stmt {
    Expr(StmtExpr),
    Error,
}

impl Debug for Stmt {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Expr(arg0) => f.write_fmt(format_args!("{:#?}", arg0)),
            Self::Error => write!(f, "Error"),
        }
    }
}

/// An expression statement evaluates an expression and discards the result.
#[derive(Clone, Debug, PartialEq)]
pub struct StmtExpr {
    pub expr: ExprS,
}

/// Expressions
#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Unit,
    Literal(ExprLiteral),
    Identifier(ExprIdentifier),
    Block(Box<ExprBlock>),
    List(ExprList),
    Tuple(ExprTuple),
    Infix(Box<ExprInfix>),
    Prefix(Box<ExprPrefix>),
    Assign(Box<ExprAssign>),
    If(Box<ExprIf>),
    Fn(Box<ExprFn>),
    Range(ExprRange),
}

#[derive(Clone, Debug, PartialEq)]
pub enum ExprLiteral {
    Bool(bool),
    Number(f64),
    String(String),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ExprIdentifier {
    pub identifier: Identifier,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Identifier {
    pub name: String,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ExprBlock {
    pub stmts: Vec<StmtS>,
    pub return_expr: Option<ExprS>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ExprList {
    pub items: Vec<ExprS>,
}

#[derive(Clone, Debug, PartialEq, Default)]
pub struct ExprTuple {
    pub items: Vec<ExprS>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ExprInfix {
    pub lt: ExprS,
    pub op: OpInfix,
    pub rt: ExprS,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
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

#[derive(Clone, Debug, PartialEq)]
pub struct ExprPrefix {
    pub op: OpPrefix,
    pub rt: ExprS,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum OpPrefix {
    Negate,
    Not,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ExprAssign {
    pub identifier: Identifier,
    pub value: ExprS,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ExprIf {
    pub cond: ExprS,
    pub then: ExprS,
    pub else_: Option<ExprS>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ExprFn {
    pub params: Vec<ExprS>,
    pub body: ExprS,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ExprRange {
    pub start: Option<Spanned<ExprLiteral>>,
    pub end: Option<Spanned<ExprLiteral>>,
    pub inclusive_end: bool,
}

impl Default for ExprRange {
    fn default() -> Self {
        Self {
            start: None,
            end: None,
            inclusive_end: false,
        }
    }
}
