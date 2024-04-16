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
            Expr::Unit => TypeRef::void(),
            Expr::Literal(literal) => match literal {
                ExprLiteral::Bool(_) => TypeRef::bool(),
                ExprLiteral::Number(_) => TypeRef::number(),
                ExprLiteral::String(_) => TypeRef::string(),
            },
            Expr::Identifier(_) => TypeRef::identifier(),
            Expr::Block(block) => block
                .return_expr
                .map_or(TypeRef::void(), |(expr, _)| expr.get_type_expr()),
            Expr::List(_) => TypeRef::list(),
            Expr::Tuple(_) => TypeRef::tuple(),
            Expr::Infix(infix) => match infix.op {
                OpInfix::Add => TypeRef::number(),
                OpInfix::Subtract => TypeRef::number(),
                OpInfix::Multiply => TypeRef::number(),
                OpInfix::Divide => TypeRef::number(),
                OpInfix::Modulus => TypeRef::number(),
                OpInfix::Less => TypeRef::bool(),
                OpInfix::LessEqual => TypeRef::bool(),
                OpInfix::Greater => TypeRef::bool(),
                OpInfix::GreaterEqual => TypeRef::bool(),
                OpInfix::Equal => TypeRef::bool(),
                OpInfix::NotEqual => TypeRef::bool(),
                OpInfix::LogicAnd => TypeRef::bool(),
                OpInfix::LogicOr => TypeRef::bool(),
            },
            Expr::Prefix(prefix) => prefix.rt.0.get_type_expr(),
            Expr::Assign(assign) => {
                let (expr, _) = assign.value;

                expr.get_type_expr()
            }
            Expr::If(if_) => if_.then.0.get_type_expr(),
            Expr::Fn(_) => todo!(),
            Expr::Range(_) => TypeRef::range(),
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
    pub fn string() -> TypeRef {
        TypeRef("String".to_string(), vec![])
    }

    pub fn number() -> TypeRef {
        TypeRef("Number".to_string(), vec![])
    }

    pub fn bool() -> TypeRef {
        TypeRef("Bool".to_string(), vec![])
    }

    pub fn void() -> TypeRef {
        TypeRef("Void".to_string(), vec![])
    }

    pub fn range() -> TypeRef {
        TypeRef("Range".to_string(), vec![])
    }

    pub fn list() -> TypeRef {
        TypeRef("List".to_string(), vec![])
    }

    pub fn tuple() -> TypeRef {
        TypeRef("Tuple".to_string(), vec![])
    }

    pub fn identifier() -> TypeRef {
        TypeRef("Identifier".to_string(), vec![])
    }
}

impl Display for TypeRef {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("{}", self.0))
    }
}
