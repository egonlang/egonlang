use crate::span::Spanned;
use std::fmt::{self, Debug, Display, Formatter};

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
    Assign(StmtAssign),
    Error,
}

impl Debug for Stmt {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Expr(arg0) => f.write_fmt(format_args!("{:#?}", arg0)),
            Self::Assign(arg0) => f.write_fmt(format_args!("{:#?}", arg0)),
            Self::Error => write!(f, "Error"),
        }
    }
}

/// An expression statement evaluates an expression and discards the result.
#[derive(Clone, Debug, PartialEq)]
pub struct StmtExpr {
    pub expr: ExprS,
}

/// Statement that sets `var.name` to `value`
#[derive(Clone, Debug, PartialEq)]
pub struct StmtAssign {
    pub identifier: Identifier,
    pub type_expr: Option<ExprS>,
    pub is_const: bool,
    pub value: Option<ExprS>,
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
    Type(ExprType),
}

impl Expr {
    pub fn get_type_expr(self) -> TypeRef {
        match self {
            Expr::Unit => TypeRef::simple("Void".to_string()),
            Expr::Literal(literal) => match literal {
                ExprLiteral::Bool(_) => TypeRef::simple("Bool".to_string()),
                ExprLiteral::Number(_) => TypeRef::simple("Number".to_string()),
                ExprLiteral::String(_) => TypeRef::simple("String".to_string()),
            },
            Expr::Identifier(_) => TypeRef::simple("Identifier".to_string()),
            Expr::Block(block) => block
                .return_expr
                .map_or(TypeRef::simple("Void".to_string()), |(expr, _)| {
                    expr.get_type_expr()
                }),
            Expr::List(_) => TypeRef::simple("List".to_string()),
            Expr::Tuple(_) => TypeRef::simple("Tuple".to_string()),
            Expr::Infix(infix) => match infix.op {
                OpInfix::Add => TypeRef::simple("Number".to_string()),
                OpInfix::Subtract => TypeRef::simple("Number".to_string()),
                OpInfix::Multiply => TypeRef::simple("Number".to_string()),
                OpInfix::Divide => TypeRef::simple("Number".to_string()),
                OpInfix::Modulus => TypeRef::simple("Number".to_string()),
                OpInfix::Less => TypeRef::simple("Bool".to_string()),
                OpInfix::LessEqual => TypeRef::simple("Bool".to_string()),
                OpInfix::Greater => TypeRef::simple("Bool".to_string()),
                OpInfix::GreaterEqual => TypeRef::simple("Bool".to_string()),
                OpInfix::Equal => TypeRef::simple("Bool".to_string()),
                OpInfix::NotEqual => TypeRef::simple("Bool".to_string()),
                OpInfix::LogicAnd => TypeRef::simple("Bool".to_string()),
                OpInfix::LogicOr => TypeRef::simple("Bool".to_string()),
            },
            Expr::Prefix(prefix) => prefix.rt.0.get_type_expr(),
            Expr::Assign(assign) => {
                let (expr, _) = assign.value;

                expr.get_type_expr()
            }
            Expr::If(if_) => if_.then.0.get_type_expr(),
            Expr::Fn(_) => todo!(),
            Expr::Range(_) => TypeRef::simple("Range".to_string()),
            Expr::Type(ty) => ty.0,
        }
    }
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

#[derive(Clone, Debug, PartialEq, Default)]
pub struct ExprRange {
    pub start: Option<Spanned<ExprLiteral>>,
    pub end: Option<Spanned<ExprLiteral>>,
    pub inclusive_end: bool,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ExprType(pub TypeRef);

#[derive(Clone, Debug, PartialEq)]
pub struct TypeRef(pub String, pub Vec<ExprS>);

impl TypeRef {
    pub fn simple(type_ident: String) -> TypeRef {
        TypeRef(type_ident, vec![])
    }
}

impl Display for TypeRef {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("{}", self.0))
    }
}
