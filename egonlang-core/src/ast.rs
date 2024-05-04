use serde::{Deserialize, Serialize};

use crate::{errors::Error, span::Spanned};
use std::fmt::{self, Debug, Display, Formatter};

/// Modules are units of code (e.g. variables, functions)
#[derive(Debug, Default, PartialEq, Serialize, Deserialize)]
pub struct Module {
    pub stmts: Vec<StmtS>,
}

pub type StmtS = Spanned<Stmt>;
pub type ExprS = Spanned<Expr>;

/// Statements
#[derive(Clone, PartialEq, Serialize, Deserialize)]
pub enum Stmt {
    Expr(StmtExpr),
    Assign(StmtAssign),
    Fn(Box<StmtFn>),
    Error,
}

impl Display for Stmt {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Stmt::Expr(stmt) => f.write_fmt(format_args!("{}", stmt)),
            Stmt::Assign(stmt) => f.write_fmt(format_args!("{}", stmt)),
            Stmt::Fn(stmt) => f.write_fmt(format_args!("{}", stmt)),
            Stmt::Error => todo!(),
        }
    }
}

impl Debug for Stmt {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Expr(arg0) => f.write_fmt(format_args!("{:#?}", arg0)),
            Self::Assign(arg0) => f.write_fmt(format_args!("{:#?}", arg0)),
            Self::Fn(arg0) => f.write_fmt(format_args!("{:#?}", arg0)),
            Self::Error => write!(f, "Error"),
        }
    }
}

/// Statement that creates a type alias
/// e.g. type NumberList = number<list>;
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct StmtType {
    pub identifier: Identifier,
    pub type_expr: Option<ExprS>,
}

/// An expression statement evaluates an expression and discards the result.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct StmtExpr {
    pub expr: ExprS,
}

impl Display for StmtExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("{};", self.expr.0))
    }
}

/// Statement that sets `var.name` to `value`
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct StmtAssign {
    pub identifier: Identifier,
    pub type_expr: Option<ExprS>,
    pub is_const: bool,
    pub value: Option<ExprS>,
}

impl Display for StmtAssign {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let decl = if self.is_const { "const" } else { "let" };
        let name = &self.identifier.name;
        let typing = self
            .type_expr
            .clone()
            .map(|f| f.0)
            .map(|f| format!(": {f}"))
            .unwrap_or_default();
        let value = self
            .value
            .clone()
            .map(|f| f.0)
            .map(|v| format!(" = {v}"))
            .unwrap_or_default();

        f.write_fmt(format_args!("{} {}{}{};", decl, name, typing, value))
    }
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct StmtFn {
    pub name: Identifier,
    pub fn_expr: ExprS,
}

impl Display for StmtFn {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("fn {} {}", self.name.name, self.fn_expr.0))
    }
}

/// Expressions
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
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
        }
    }
}

impl Expr {
    pub fn get_type_expr(self) -> TypeRef {
        match self {
            Expr::Unit => TypeRef::unit(),
            Expr::Literal(literal) => match literal {
                ExprLiteral::Bool(_) => TypeRef::bool(),
                ExprLiteral::Number(_) => TypeRef::number(),
                ExprLiteral::String(_) => TypeRef::string(),
            },
            Expr::Identifier(_) => TypeRef::identifier(),
            Expr::Block(block) => block
                .return_expr
                .map_or(TypeRef::unit(), |(expr, _)| expr.get_type_expr()),
            Expr::List(list) => {
                if list.items.is_empty() {
                    return TypeRef::list(TypeRef::unknown());
                }

                let (first_item, _) = list.items.first().unwrap().clone();

                let first_item_type_ident = first_item.get_type_expr();

                TypeRef::list(first_item_type_ident)
            }
            Expr::Tuple(tuple) => TypeRef::tuple(
                tuple
                    .items
                    .into_iter()
                    .map(|(expr, _)| expr.get_type_expr())
                    .collect(),
            ),
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
            Expr::Prefix(prefix) => match prefix.op {
                OpPrefix::Negate => TypeRef::number(),
                OpPrefix::Not => TypeRef::bool(),
            },
            Expr::Assign(assign) => {
                let (expr, _) = assign.value;

                expr.get_type_expr()
            }
            Expr::If(if_) => if_.then.0.get_type_expr(),
            Expr::Fn(fn_) => TypeRef::function(
                fn_.params.into_iter().map(|param| param.0 .1).collect(),
                fn_.return_type.0,
            ),
            Expr::Range(_) => TypeRef::range(),
            Expr::Type(ty) => ty.0,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum ExprLiteral {
    Bool(bool),
    Number(f64),
    String(String),
}

impl TryFrom<Expr> for f64 {
    type Error = Vec<Error>;

    fn try_from(value: Expr) -> Result<Self, Self::Error> {
        match value {
            Expr::Literal(literal) => match literal {
                ExprLiteral::Number(number) => Ok(number),
                ExprLiteral::Bool(_) => Err(vec![Error::TypeError(
                    crate::errors::TypeError::MismatchType {
                        expected: TypeRef::number().to_string(),
                        actual: TypeRef::bool().to_string(),
                    },
                )]),
                ExprLiteral::String(_) => Err(vec![Error::TypeError(
                    crate::errors::TypeError::MismatchType {
                        expected: TypeRef::number().to_string(),
                        actual: TypeRef::string().to_string(),
                    },
                )]),
            },
            Expr::Prefix(prefix) => match prefix.op {
                OpPrefix::Negate => prefix.rt.0.try_into(),
                OpPrefix::Not => Err(vec![Error::TypeError(
                    crate::errors::TypeError::MismatchType {
                        expected: TypeRef::number().to_string(),
                        actual: TypeRef::bool().to_string(),
                    },
                )]),
            },
            _ => Err(vec![Error::TypeError(
                crate::errors::TypeError::MismatchType {
                    expected: TypeRef::number().to_string(),
                    actual: value.get_type_expr().to_string(),
                },
            )]),
        }
    }
}

impl TryFrom<Expr> for String {
    type Error = Vec<Error>;

    fn try_from(value: Expr) -> Result<Self, Self::Error> {
        match value {
            Expr::Literal(literal) => match literal {
                ExprLiteral::Number(_) => Err(vec![Error::TypeError(
                    crate::errors::TypeError::MismatchType {
                        expected: TypeRef::string().to_string(),
                        actual: TypeRef::number().to_string(),
                    },
                )]),
                ExprLiteral::Bool(_) => Err(vec![Error::TypeError(
                    crate::errors::TypeError::MismatchType {
                        expected: TypeRef::string().to_string(),
                        actual: TypeRef::bool().to_string(),
                    },
                )]),
                ExprLiteral::String(string) => Ok(string),
            },
            _ => Err(vec![Error::TypeError(
                crate::errors::TypeError::MismatchType {
                    expected: TypeRef::string().to_string(),
                    actual: value.get_type_expr().to_string(),
                },
            )]),
        }
    }
}

impl TryFrom<Expr> for bool {
    type Error = Vec<Error>;

    fn try_from(value: Expr) -> Result<Self, Self::Error> {
        match value {
            Expr::Literal(literal) => match literal {
                ExprLiteral::Number(_) => Err(vec![Error::TypeError(
                    crate::errors::TypeError::MismatchType {
                        expected: TypeRef::bool().to_string(),
                        actual: TypeRef::number().to_string(),
                    },
                )]),
                ExprLiteral::Bool(bool) => Ok(bool),
                ExprLiteral::String(_) => Err(vec![Error::TypeError(
                    crate::errors::TypeError::MismatchType {
                        expected: TypeRef::bool().to_string(),
                        actual: TypeRef::string().to_string(),
                    },
                )]),
            },
            _ => Err(vec![Error::TypeError(
                crate::errors::TypeError::MismatchType {
                    expected: TypeRef::bool().to_string(),
                    actual: value.get_type_expr().to_string(),
                },
            )]),
        }
    }
}

impl Display for ExprLiteral {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            ExprLiteral::Bool(expr) => f.write_fmt(format_args!("{}", expr)),
            ExprLiteral::Number(expr) => f.write_fmt(format_args!("{}", expr)),
            ExprLiteral::String(expr) => f.write_fmt(format_args!("{}", expr)),
        }
    }
}

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

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct ExprBlock {
    pub stmts: Vec<StmtS>,
    pub return_expr: Option<ExprS>,
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

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct ExprAssign {
    pub identifier: Identifier,
    pub value: ExprS,
}

impl Display for ExprAssign {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("{} = {}", self.identifier.name, self.value.0))
    }
}

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

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct ExprFn {
    pub name: Option<Identifier>,
    pub params: Vec<Spanned<(Identifier, TypeRef)>>,
    pub return_type: Spanned<TypeRef>,
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

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct ExprType(pub TypeRef);

impl Display for ExprType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("{}", self.0))
    }
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct TypeRef(pub String, pub Vec<TypeRef>);

impl TypeRef {
    pub fn string() -> TypeRef {
        TypeRef("string".to_string(), vec![])
    }

    pub fn number() -> TypeRef {
        TypeRef("number".to_string(), vec![])
    }

    pub fn bool() -> TypeRef {
        TypeRef("bool".to_string(), vec![])
    }

    pub fn unit() -> TypeRef {
        TypeRef("()".to_string(), vec![])
    }

    pub fn range() -> TypeRef {
        TypeRef("range".to_string(), vec![])
    }

    pub fn list(item_type: TypeRef) -> TypeRef {
        TypeRef("list".to_string(), vec![item_type])
    }

    pub fn tuple(item_types: Vec<TypeRef>) -> TypeRef {
        TypeRef("tuple".to_string(), item_types)
    }

    pub fn identifier() -> TypeRef {
        TypeRef("identifier".to_string(), vec![])
    }

    pub fn unknown() -> TypeRef {
        TypeRef("unknown".to_string(), vec![])
    }

    pub fn function(params_types: Vec<TypeRef>, return_type: TypeRef) -> TypeRef {
        TypeRef(
            "function".to_string(),
            vec![TypeRef::tuple(params_types), return_type],
        )
    }

    pub fn typed(value: TypeRef) -> TypeRef {
        TypeRef("type".to_string(), vec![value])
    }
}

impl Display for TypeRef {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let base = if self.1.is_empty() {
            self.0.clone()
        } else {
            let m = format!(
                "{}<{}>",
                self.0,
                self.1
                    .clone()
                    .into_iter()
                    .map(|typeref| typeref.to_string())
                    .collect::<Vec<String>>()
                    .join(", ")
            );
            m
        };
        f.write_fmt(format_args!("{}", base))
    }
}

#[cfg(test)]
mod ast_tests {
    use std::vec;

    use pretty_assertions::assert_eq;

    use crate::*;

    macro_rules! expr_display_test {
        ($test_name:ident, $expr:expr, $expected:expr) => {
            #[test]
            fn $test_name() {
                assert_eq!($expected, $expr.to_string());
            }
        };
    }

    expr_display_test!(test_expr_display_unit, crate::ast::Expr::Unit, "()");

    expr_display_test!(
        test_expr_literal_bool_true,
        crate::ast::Expr::Literal(crate::ast::ExprLiteral::Bool(true)),
        "true"
    );

    expr_display_test!(
        test_expr_literal_bool_false,
        crate::ast::Expr::Literal(crate::ast::ExprLiteral::Bool(false)),
        "false"
    );

    expr_display_test!(
        test_expr_literal_number_zero,
        crate::ast::Expr::Literal(crate::ast::ExprLiteral::Number(0f64)),
        "0"
    );

    expr_display_test!(
        test_expr_literal_number_positive,
        crate::ast::Expr::Literal(crate::ast::ExprLiteral::Number(3842f64)),
        "3842"
    );

    expr_display_test!(
        test_expr_literal_number_negative,
        crate::ast::Expr::Literal(crate::ast::ExprLiteral::Number(-1546f64)),
        "-1546"
    );

    expr_display_test!(
        test_expr_literal_number_float,
        crate::ast::Expr::Literal(crate::ast::ExprLiteral::Number(987.123f64)),
        "987.123"
    );

    expr_display_test!(
        test_expr_identifier,
        crate::ast::Expr::Identifier(crate::ast::ExprIdentifier {
            identifier: crate::ast::Identifier {
                name: "foo".to_string()
            }
        }),
        "foo"
    );

    expr_display_test!(
        test_expr_range,
        crate::ast::Expr::Range(crate::ast::ExprRange {
            start: Some((crate::ast::ExprLiteral::Number(0f64), 0..0)),
            end: Some((crate::ast::ExprLiteral::Number(10f64), 0..0)),
            inclusive_end: false
        }),
        "0..10"
    );

    expr_display_test!(
        test_expr_assign,
        crate::ast::Expr::Assign(Box::new(ast::ExprAssign {
            identifier: ast::Identifier {
                name: "foo".to_string()
            },
            value: (ast::Expr::Literal(ast::ExprLiteral::Number(123f64)), 0..0)
        })),
        "foo = 123"
    );

    expr_display_test!(
        test_expr_prefix_negate,
        crate::ast::Expr::Prefix(Box::new(ast::ExprPrefix {
            op: ast::OpPrefix::Negate,
            rt: (ast::Expr::Literal(ast::ExprLiteral::Number(43557f64)), 0..0)
        })),
        "-43557"
    );

    expr_display_test!(
        test_expr_prefix_not,
        crate::ast::Expr::Prefix(Box::new(ast::ExprPrefix {
            op: ast::OpPrefix::Not,
            rt: (ast::Expr::Literal(ast::ExprLiteral::Bool(false)), 0..0)
        })),
        "!false"
    );

    expr_display_test!(
        test_expr_infix_add,
        crate::ast::Expr::Infix(Box::new(ast::ExprInfix {
            lt: (ast::Expr::Literal(ast::ExprLiteral::Number(100f64)), 0..0),
            op: ast::OpInfix::Add,
            rt: (ast::Expr::Literal(ast::ExprLiteral::Number(55f64)), 0..0)
        })),
        "100 + 55"
    );

    expr_display_test!(
        test_expr_infix_subtract,
        crate::ast::Expr::Infix(Box::new(ast::ExprInfix {
            lt: (ast::Expr::Literal(ast::ExprLiteral::Number(100f64)), 0..0),
            op: ast::OpInfix::Subtract,
            rt: (ast::Expr::Literal(ast::ExprLiteral::Number(55f64)), 0..0)
        })),
        "100 - 55"
    );

    expr_display_test!(
        test_expr_infix_multiply,
        crate::ast::Expr::Infix(Box::new(ast::ExprInfix {
            lt: (ast::Expr::Literal(ast::ExprLiteral::Number(100f64)), 0..0),
            op: ast::OpInfix::Multiply,
            rt: (ast::Expr::Literal(ast::ExprLiteral::Number(55f64)), 0..0)
        })),
        "100 * 55"
    );

    expr_display_test!(
        test_expr_infix_divide,
        crate::ast::Expr::Infix(Box::new(ast::ExprInfix {
            lt: (ast::Expr::Literal(ast::ExprLiteral::Number(100f64)), 0..0),
            op: ast::OpInfix::Divide,
            rt: (ast::Expr::Literal(ast::ExprLiteral::Number(55f64)), 0..0)
        })),
        "100 / 55"
    );

    expr_display_test!(
        test_expr_infix_modulus,
        crate::ast::Expr::Infix(Box::new(ast::ExprInfix {
            lt: (ast::Expr::Literal(ast::ExprLiteral::Number(100f64)), 0..0),
            op: ast::OpInfix::Modulus,
            rt: (ast::Expr::Literal(ast::ExprLiteral::Number(55f64)), 0..0)
        })),
        "100 % 55"
    );

    expr_display_test!(
        test_expr_infix_eq,
        crate::ast::Expr::Infix(Box::new(ast::ExprInfix {
            lt: (ast::Expr::Literal(ast::ExprLiteral::Number(100f64)), 0..0),
            op: ast::OpInfix::Equal,
            rt: (ast::Expr::Literal(ast::ExprLiteral::Number(55f64)), 0..0)
        })),
        "100 == 55"
    );

    expr_display_test!(
        test_expr_infix_not_eq,
        crate::ast::Expr::Infix(Box::new(ast::ExprInfix {
            lt: (ast::Expr::Literal(ast::ExprLiteral::Number(100f64)), 0..0),
            op: ast::OpInfix::NotEqual,
            rt: (ast::Expr::Literal(ast::ExprLiteral::Number(55f64)), 0..0)
        })),
        "100 != 55"
    );

    expr_display_test!(
        test_expr_infix_gt,
        crate::ast::Expr::Infix(Box::new(ast::ExprInfix {
            lt: (ast::Expr::Literal(ast::ExprLiteral::Number(100f64)), 0..0),
            op: ast::OpInfix::Greater,
            rt: (ast::Expr::Literal(ast::ExprLiteral::Number(55f64)), 0..0)
        })),
        "100 > 55"
    );

    expr_display_test!(
        test_expr_infix_lt,
        crate::ast::Expr::Infix(Box::new(ast::ExprInfix {
            lt: (ast::Expr::Literal(ast::ExprLiteral::Number(100f64)), 0..0),
            op: ast::OpInfix::Less,
            rt: (ast::Expr::Literal(ast::ExprLiteral::Number(55f64)), 0..0)
        })),
        "100 < 55"
    );

    expr_display_test!(
        test_expr_infix_gte,
        crate::ast::Expr::Infix(Box::new(ast::ExprInfix {
            lt: (ast::Expr::Literal(ast::ExprLiteral::Number(100f64)), 0..0),
            op: ast::OpInfix::GreaterEqual,
            rt: (ast::Expr::Literal(ast::ExprLiteral::Number(55f64)), 0..0)
        })),
        "100 >= 55"
    );

    expr_display_test!(
        test_expr_infix_lte,
        crate::ast::Expr::Infix(Box::new(ast::ExprInfix {
            lt: (ast::Expr::Literal(ast::ExprLiteral::Number(100f64)), 0..0),
            op: ast::OpInfix::LessEqual,
            rt: (ast::Expr::Literal(ast::ExprLiteral::Number(55f64)), 0..0)
        })),
        "100 <= 55"
    );

    expr_display_test!(
        test_expr_infix_and,
        crate::ast::Expr::Infix(Box::new(ast::ExprInfix {
            lt: (ast::Expr::Literal(ast::ExprLiteral::Bool(true)), 0..0),
            op: ast::OpInfix::LogicAnd,
            rt: (ast::Expr::Literal(ast::ExprLiteral::Bool(false)), 0..0)
        })),
        "true and false"
    );

    expr_display_test!(
        test_expr_infix_or,
        crate::ast::Expr::Infix(Box::new(ast::ExprInfix {
            lt: (ast::Expr::Literal(ast::ExprLiteral::Number(100f64)), 0..0),
            op: ast::OpInfix::LogicOr,
            rt: (ast::Expr::Literal(ast::ExprLiteral::Number(55f64)), 0..0)
        })),
        "100 or 55"
    );

    expr_display_test!(
        test_expr_if,
        crate::ast::Expr::If(Box::new(crate::ast::ExprIf {
            cond: (
                crate::ast::Expr::Literal(crate::ast::ExprLiteral::Bool(true)),
                0..0
            ),
            then: (
                crate::ast::Expr::Block(Box::new(crate::ast::ExprBlock {
                    stmts: vec![],
                    return_expr: Some((ast::Expr::Literal(ast::ExprLiteral::Number(123f64)), 0..0))
                })),
                0..0
            ),
            else_: None
        })),
        "if (true) { 123 }"
    );

    expr_display_test!(
        test_expr_if_with_stmts,
        crate::ast::Expr::If(Box::new(crate::ast::ExprIf {
            cond: (
                crate::ast::Expr::Literal(crate::ast::ExprLiteral::Bool(true)),
                0..0
            ),
            then: (
                crate::ast::Expr::Block(Box::new(crate::ast::ExprBlock {
                    stmts: vec![(
                        crate::ast::Stmt::Expr(crate::ast::StmtExpr {
                            expr: (ast::Expr::Literal(ast::ExprLiteral::Number(123f64)), 0..0)
                        }),
                        0..0
                    )],
                    return_expr: Some((ast::Expr::Literal(ast::ExprLiteral::Number(456f64)), 0..0))
                })),
                0..0
            ),
            else_: None
        })),
        "if (true) { 123; 456 }"
    );

    expr_display_test!(
        test_expr_if_else,
        crate::ast::Expr::If(Box::new(crate::ast::ExprIf {
            cond: (
                crate::ast::Expr::Literal(crate::ast::ExprLiteral::Bool(true)),
                0..0
            ),
            then: (
                crate::ast::Expr::Block(Box::new(crate::ast::ExprBlock {
                    stmts: vec![],
                    return_expr: Some((ast::Expr::Literal(ast::ExprLiteral::Number(123f64)), 0..0))
                })),
                0..0
            ),
            else_: Some((
                crate::ast::Expr::Block(Box::new(crate::ast::ExprBlock {
                    stmts: vec![],
                    return_expr: Some((ast::Expr::Literal(ast::ExprLiteral::Number(456f64)), 0..0))
                })),
                0..0
            ))
        })),
        "if (true) { 123 } else { 456 }"
    );

    expr_display_test!(
        test_expr_if_else_with_stmts,
        crate::ast::Expr::If(Box::new(crate::ast::ExprIf {
            cond: (
                crate::ast::Expr::Literal(crate::ast::ExprLiteral::Bool(true)),
                0..0
            ),
            then: (
                crate::ast::Expr::Block(Box::new(crate::ast::ExprBlock {
                    stmts: vec![(
                        crate::ast::Stmt::Expr(crate::ast::StmtExpr {
                            expr: (ast::Expr::Literal(ast::ExprLiteral::Number(123f64)), 0..0)
                        }),
                        0..0
                    )],
                    return_expr: Some((ast::Expr::Literal(ast::ExprLiteral::Number(456f64)), 0..0))
                })),
                0..0
            ),
            else_: Some((
                crate::ast::Expr::Block(Box::new(crate::ast::ExprBlock {
                    stmts: vec![(
                        crate::ast::Stmt::Expr(crate::ast::StmtExpr {
                            expr: (ast::Expr::Literal(ast::ExprLiteral::Number(789f64)), 0..0)
                        }),
                        0..0
                    )],
                    return_expr: Some((ast::Expr::Literal(ast::ExprLiteral::Number(0f64)), 0..0))
                })),
                0..0
            ))
        })),
        "if (true) { 123; 456 } else { 789; 0 }"
    );

    expr_display_test!(
        test_expr_empty_list,
        crate::ast::Expr::List(crate::ast::ExprList { items: vec![] }),
        "[]"
    );

    expr_display_test!(
        test_expr_list,
        crate::ast::Expr::List(crate::ast::ExprList {
            items: vec![
                (ast::Expr::Literal(ast::ExprLiteral::Number(1f64)), 0..0),
                (ast::Expr::Literal(ast::ExprLiteral::Number(2f64)), 0..0),
                (ast::Expr::Literal(ast::ExprLiteral::Number(3f64)), 0..0)
            ]
        }),
        "[1, 2, 3]"
    );

    expr_display_test!(
        test_expr_tuple,
        crate::ast::Expr::Tuple(crate::ast::ExprTuple {
            items: vec![
                (ast::Expr::Literal(ast::ExprLiteral::Number(1f64)), 0..0),
                (ast::Expr::Literal(ast::ExprLiteral::Number(2f64)), 0..0),
                (ast::Expr::Literal(ast::ExprLiteral::Number(3f64)), 0..0)
            ]
        }),
        "(1, 2, 3,)"
    );

    expr_display_test!(
        test_expr_fn,
        crate::ast::Expr::Fn(Box::new(crate::ast::ExprFn {
            name: None,
            params: vec![],
            return_type: (crate::ast::TypeRef::unit(), 0..0),
            body: (
                crate::ast::Expr::Block(Box::new(ast::ExprBlock {
                    stmts: vec![],
                    return_expr: None
                })),
                0..0
            )
        })),
        "(): () => {}"
    );

    expr_display_test!(
        test_expr_fn_with_param,
        crate::ast::Expr::Fn(Box::new(crate::ast::ExprFn {
            name: None,
            params: vec![(
                (
                    crate::ast::Identifier {
                        name: "a".to_string()
                    },
                    crate::ast::TypeRef::number()
                ),
                0..0
            )],
            return_type: (crate::ast::TypeRef::unit(), 0..0),
            body: (
                crate::ast::Expr::Block(Box::new(ast::ExprBlock {
                    stmts: vec![],
                    return_expr: None
                })),
                0..0
            )
        })),
        "(a: number): () => {}"
    );

    expr_display_test!(
        test_expr_fn_with_params,
        crate::ast::Expr::Fn(Box::new(crate::ast::ExprFn {
            name: None,
            params: vec![
                (
                    (
                        crate::ast::Identifier {
                            name: "a".to_string()
                        },
                        crate::ast::TypeRef::number()
                    ),
                    0..0
                ),
                (
                    (
                        crate::ast::Identifier {
                            name: "b".to_string()
                        },
                        crate::ast::TypeRef::number()
                    ),
                    0..0
                )
            ],
            return_type: (crate::ast::TypeRef::unit(), 0..0),
            body: (
                crate::ast::Expr::Block(Box::new(ast::ExprBlock {
                    stmts: vec![],
                    return_expr: None
                })),
                0..0
            )
        })),
        "(a: number, b: number): () => {}"
    );

    expr_display_test!(
        test_expr_fn_with_params_and_return_type,
        crate::ast::Expr::Fn(Box::new(crate::ast::ExprFn {
            name: None,
            params: vec![
                (
                    (
                        crate::ast::Identifier {
                            name: "a".to_string()
                        },
                        crate::ast::TypeRef::number()
                    ),
                    0..0
                ),
                (
                    (
                        crate::ast::Identifier {
                            name: "b".to_string()
                        },
                        crate::ast::TypeRef::number()
                    ),
                    0..0
                )
            ],
            return_type: (crate::ast::TypeRef::number(), 0..0),
            body: (
                crate::ast::Expr::Block(Box::new(ast::ExprBlock {
                    stmts: vec![],
                    return_expr: None
                })),
                0..0
            )
        })),
        "(a: number, b: number): number => {}"
    );

    expr_display_test!(
        test_expr_fn_with_params_and_return_type_and_body_return_expr,
        crate::ast::Expr::Fn(Box::new(crate::ast::ExprFn {
            name: None,
            params: vec![
                (
                    (
                        crate::ast::Identifier {
                            name: "a".to_string()
                        },
                        crate::ast::TypeRef::number()
                    ),
                    0..0
                ),
                (
                    (
                        crate::ast::Identifier {
                            name: "b".to_string()
                        },
                        crate::ast::TypeRef::number()
                    ),
                    0..0
                )
            ],
            return_type: (crate::ast::TypeRef::number(), 0..0),
            body: (
                crate::ast::Expr::Block(Box::new(ast::ExprBlock {
                    stmts: vec![],
                    return_expr: Some((
                        crate::ast::Expr::Infix(Box::new(crate::ast::ExprInfix {
                            lt: (
                                crate::ast::Expr::Identifier(crate::ast::ExprIdentifier {
                                    identifier: ast::Identifier {
                                        name: "a".to_string()
                                    }
                                }),
                                0..0
                            ),
                            op: crate::ast::OpInfix::Add,
                            rt: (
                                crate::ast::Expr::Identifier(crate::ast::ExprIdentifier {
                                    identifier: ast::Identifier {
                                        name: "b".to_string()
                                    }
                                }),
                                0..0
                            )
                        })),
                        0..0
                    ))
                })),
                0..0
            )
        })),
        "(a: number, b: number): number => { a + b }"
    );
}
