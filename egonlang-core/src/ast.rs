use serde::{Deserialize, Serialize};

use crate::{
    errors::{self, EgonError, EgonSyntaxError, EgonTypeError},
    prelude::parse,
    span::Spanned,
};
use std::fmt::{self, Debug, Display, Formatter};

/// Modules are units of code (e.g. variables, functions)
#[derive(Debug, Default, PartialEq, Serialize, Deserialize)]
pub struct Module {
    pub stmts: Vec<StmtS>,
}

impl Module {
    pub fn new() -> Self {
        Module::default()
    }

    pub fn from(stmts: Vec<StmtS>) -> Self {
        Module { stmts }
    }
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

impl From<StmtExpr> for Stmt {
    fn from(value: StmtExpr) -> Self {
        Stmt::Expr(value)
    }
}

impl From<StmtAssign> for Stmt {
    fn from(value: StmtAssign) -> Self {
        Stmt::Assign(value)
    }
}

impl From<StmtFn> for Stmt {
    fn from(value: StmtFn) -> Self {
        Stmt::Fn(Box::from(value))
    }
}

impl TryFrom<&str> for Stmt {
    type Error = errors::EgonError;

    fn try_from(value: &str) -> Result<Stmt, errors::EgonError> {
        let module = parse(value, 0).unwrap();

        let stmt_spanned = module.stmts.first().unwrap();

        Ok(stmt_spanned.0.clone())
    }
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

        let is_type_alias = self
            .value
            .as_ref()
            .map(|(type_expr, _)| matches!(type_expr, Expr::Type(_)))
            .unwrap_or(false);

        if is_type_alias {
            let typing = self
                .type_expr
                .clone()
                .map(|f| f.0)
                .map(|f| format!("{f}"))
                .unwrap_or_default();

            f.write_fmt(format_args!("type {} = {};", name, typing))
        } else {
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

impl TryFrom<&str> for Expr {
    type Error = errors::EgonError;

    fn try_from(value: &str) -> Result<Expr, errors::EgonError> {
        let module = parse(value, 0).unwrap();

        let (stmt, _) = module.stmts.first().unwrap();

        if let Stmt::Expr(stmt_expr) = stmt {
            let expr = &stmt_expr.expr;

            return Ok(expr.0.clone());
        };

        Err(EgonSyntaxError::InvalidToken.into())
    }
}

impl From<ExprLiteral> for Expr {
    fn from(value: ExprLiteral) -> Self {
        Expr::Literal(value)
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
        Expr::List(value)
    }
}

impl From<ExprTuple> for Expr {
    fn from(value: ExprTuple) -> Self {
        Expr::Tuple(value)
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
    type Error = Vec<EgonError>;

    fn try_from(value: Expr) -> Result<Self, Self::Error> {
        match value {
            Expr::Literal(literal) => match literal {
                ExprLiteral::Number(number) => Ok(number),
                ExprLiteral::Bool(_) => Err(vec![errors::EgonTypeError::MismatchType {
                    expected: TypeRef::number().to_string(),
                    actual: TypeRef::bool().to_string(),
                }
                .into()]),
                ExprLiteral::String(_) => Err(vec![errors::EgonTypeError::MismatchType {
                    expected: TypeRef::number().to_string(),
                    actual: TypeRef::string().to_string(),
                }
                .into()]),
            },
            Expr::Prefix(prefix) => match prefix.op {
                OpPrefix::Negate => prefix.rt.0.try_into(),
                OpPrefix::Not => Err(vec![errors::EgonTypeError::MismatchType {
                    expected: TypeRef::number().to_string(),
                    actual: TypeRef::bool().to_string(),
                }
                .into()]),
            },
            _ => Err(vec![errors::EgonTypeError::MismatchType {
                expected: TypeRef::number().to_string(),
                actual: value.to_string(),
            }
            .into()]),
        }
    }
}

impl TryFrom<Expr> for String {
    type Error = Vec<EgonError>;

    fn try_from(value: Expr) -> Result<Self, Self::Error> {
        match value {
            Expr::Literal(literal) => match literal {
                ExprLiteral::Number(_) => Err(vec![errors::EgonTypeError::MismatchType {
                    expected: TypeRef::string().to_string(),
                    actual: TypeRef::number().to_string(),
                }
                .into()]),
                ExprLiteral::Bool(_) => Err(vec![errors::EgonTypeError::MismatchType {
                    expected: TypeRef::string().to_string(),
                    actual: TypeRef::bool().to_string(),
                }
                .into()]),
                ExprLiteral::String(string) => Ok(string),
            },
            _ => Err(vec![errors::EgonTypeError::MismatchType {
                expected: TypeRef::string().to_string(),
                actual: value.to_string(),
            }
            .into()]),
        }
    }
}

impl TryFrom<Expr> for bool {
    type Error = Vec<EgonError>;

    fn try_from(value: Expr) -> Result<Self, Self::Error> {
        match value {
            Expr::Literal(literal) => match literal {
                ExprLiteral::Number(_) => {
                    Err(vec![EgonError::TypeError(EgonTypeError::MismatchType {
                        expected: TypeRef::bool().to_string(),
                        actual: TypeRef::number().to_string(),
                    })])
                }
                ExprLiteral::Bool(bool) => Ok(bool),
                ExprLiteral::String(_) => {
                    Err(vec![EgonError::TypeError(EgonTypeError::MismatchType {
                        expected: TypeRef::bool().to_string(),
                        actual: TypeRef::string().to_string(),
                    })])
                }
            },
            _ => Err(vec![EgonError::TypeError(EgonTypeError::MismatchType {
                expected: TypeRef::bool().to_string(),
                actual: value.to_string(),
            })]),
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
    pub fn is_type(&self) -> bool {
        self.0 == *"type"
    }

    pub fn is_list(&self) -> bool {
        self.0 == *"list"
    }

    pub fn is_known_list(&self) -> bool {
        if self.is_list() {
            let f = self.1.first().unwrap();

            TypeRef::unknown() != *f
        } else {
            false
        }
    }

    pub fn is_unknown_list(&self) -> bool {
        !self.is_known_list()
    }

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
        } else if self.is_type() {
            self.1.first().unwrap().to_string()
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

    use super::*;

    macro_rules! stmt_display_test {
        ($test_name:ident, $stmt:expr, $expected:expr) => {
            #[test]
            fn $test_name() {
                let stmt: Stmt = $stmt;
                assert_eq!($expected, stmt.to_string());
            }
        };
    }

    macro_rules! expr_display_test {
        ($test_name:ident, $expr:expr, $expected:expr) => {
            #[test]
            fn $test_name() {
                let expr: Expr = $expr;
                assert_eq!($expected, expr.to_string());
            }
        };
    }

    macro_rules! typeref_display_test {
        ($test_name:ident, $typeref:expr, $expected:expr) => {
            #[test]
            fn $test_name() {
                let typeref: TypeRef = $typeref;
                assert_eq!($expected, typeref.to_string());
            }
        };
    }

    typeref_display_test!(test_typeref_display_unit, TypeRef::unit(), "()");

    typeref_display_test!(
        test_typeref_display_type_of_number,
        TypeRef::typed(TypeRef::number()),
        "number"
    );

    typeref_display_test!(
        test_typeref_display_unknown_list,
        TypeRef::list(TypeRef::unknown()),
        "list<unknown>"
    );

    typeref_display_test!(
        test_typeref_display_number_list,
        TypeRef::list(TypeRef::number()),
        "list<number>"
    );

    typeref_display_test!(
        test_typeref_display_nested_number_list,
        TypeRef::list(TypeRef::list(TypeRef::number())),
        "list<list<number>>"
    );

    typeref_display_test!(
        test_typeref_display_nested_unknown_list,
        TypeRef::list(TypeRef::list(TypeRef::unknown())),
        "list<list<unknown>>"
    );

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
            identifier: Identifier {
                name: "foo".to_string()
            },
            value: (ExprLiteral::Number(123f64).into(), 0..0)
        })),
        "foo = 123"
    );

    expr_display_test!(
        test_expr_prefix_negate,
        Expr::Prefix(Box::new(ExprPrefix {
            op: OpPrefix::Negate,
            rt: (ExprLiteral::Number(43557f64).into(), 0..0)
        })),
        "-43557"
    );

    expr_display_test!(
        test_expr_prefix_not,
        Expr::Prefix(Box::new(ExprPrefix {
            op: OpPrefix::Not,
            rt: (ExprLiteral::Bool(false).into(), 0..0)
        })),
        "!false"
    );

    expr_display_test!(
        test_expr_infix_add,
        Expr::Infix(Box::new(ExprInfix {
            lt: (ExprLiteral::Number(100f64).into(), 0..0),
            op: OpInfix::Add,
            rt: (ExprLiteral::Number(55f64).into(), 0..0)
        })),
        "100 + 55"
    );

    expr_display_test!(
        test_expr_infix_subtract,
        Expr::Infix(Box::new(ExprInfix {
            lt: (ExprLiteral::Number(100f64).into(), 0..0),
            op: OpInfix::Subtract,
            rt: (ExprLiteral::Number(55f64).into(), 0..0)
        })),
        "100 - 55"
    );

    expr_display_test!(
        test_expr_infix_multiply,
        Expr::Infix(Box::new(ExprInfix {
            lt: (ExprLiteral::Number(100f64).into(), 0..0),
            op: OpInfix::Multiply,
            rt: (ExprLiteral::Number(55f64).into(), 0..0)
        })),
        "100 * 55"
    );

    expr_display_test!(
        test_expr_infix_divide,
        Expr::Infix(Box::new(ExprInfix {
            lt: (ExprLiteral::Number(100f64).into(), 0..0),
            op: OpInfix::Divide,
            rt: (ExprLiteral::Number(55f64).into(), 0..0)
        })),
        "100 / 55"
    );

    expr_display_test!(
        test_expr_infix_modulus,
        Expr::Infix(Box::new(ExprInfix {
            lt: (ExprLiteral::Number(100f64).into(), 0..0),
            op: OpInfix::Modulus,
            rt: (ExprLiteral::Number(55f64).into(), 0..0)
        })),
        "100 % 55"
    );

    expr_display_test!(
        test_expr_infix_eq,
        Expr::Infix(Box::new(ExprInfix {
            lt: (ExprLiteral::Number(100f64).into(), 0..0),
            op: OpInfix::Equal,
            rt: (ExprLiteral::Number(55f64).into(), 0..0)
        })),
        "100 == 55"
    );

    expr_display_test!(
        test_expr_infix_not_eq,
        Expr::Infix(Box::new(ExprInfix {
            lt: (ExprLiteral::Number(100f64).into(), 0..0),
            op: OpInfix::NotEqual,
            rt: (ExprLiteral::Number(55f64).into(), 0..0)
        })),
        "100 != 55"
    );

    expr_display_test!(
        test_expr_infix_gt,
        Expr::Infix(Box::new(ExprInfix {
            lt: (ExprLiteral::Number(100f64).into(), 0..0),
            op: OpInfix::Greater,
            rt: (ExprLiteral::Number(55f64).into(), 0..0)
        })),
        "100 > 55"
    );

    expr_display_test!(
        test_expr_infix_lt,
        Expr::Infix(Box::new(ExprInfix {
            lt: (ExprLiteral::Number(100f64).into(), 0..0),
            op: OpInfix::Less,
            rt: (ExprLiteral::Number(55f64).into(), 0..0)
        })),
        "100 < 55"
    );

    expr_display_test!(
        test_expr_infix_gte,
        Expr::Infix(Box::new(ExprInfix {
            lt: (ExprLiteral::Number(100f64).into(), 0..0),
            op: OpInfix::GreaterEqual,
            rt: (ExprLiteral::Number(55f64).into(), 0..0)
        })),
        "100 >= 55"
    );

    expr_display_test!(
        test_expr_infix_lte,
        Expr::Infix(Box::new(ExprInfix {
            lt: (ExprLiteral::Number(100f64).into(), 0..0),
            op: OpInfix::LessEqual,
            rt: (ExprLiteral::Number(55f64).into(), 0..0)
        })),
        "100 <= 55"
    );

    expr_display_test!(
        test_expr_infix_and,
        Expr::Infix(Box::new(ExprInfix {
            lt: (ExprLiteral::Bool(true).into(), 0..0),
            op: OpInfix::LogicAnd,
            rt: (ExprLiteral::Bool(false).into(), 0..0)
        })),
        "true and false"
    );

    expr_display_test!(
        test_expr_infix_or,
        Expr::Infix(Box::new(ExprInfix {
            lt: (ExprLiteral::Number(100f64).into(), 0..0),
            op: OpInfix::LogicOr,
            rt: (ExprLiteral::Number(55f64).into(), 0..0)
        })),
        "100 or 55"
    );

    expr_display_test!(
        test_expr_if,
        Expr::If(Box::new(ExprIf {
            cond: (Expr::Literal(ExprLiteral::Bool(true)), 0..0),
            then: (
                ExprBlock {
                    stmts: vec![],
                    return_expr: Some((ExprLiteral::Number(123f64).into(), 0..0))
                }
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
            cond: (ExprLiteral::Bool(true).into(), 0..0),
            then: (
                ExprBlock {
                    stmts: vec![(
                        StmtExpr {
                            expr: (ExprLiteral::Number(123f64).into(), 0..0)
                        }
                        .into(),
                        0..0
                    )],
                    return_expr: Some((ExprLiteral::Number(456f64).into(), 0..0))
                }
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
            cond: (ExprLiteral::Bool(true).into(), 0..0),
            then: (
                ExprBlock {
                    stmts: vec![],
                    return_expr: Some((ExprLiteral::Number(123f64).into(), 0..0))
                }
                .into(),
                0..0
            ),
            else_: Some((
                ExprBlock {
                    stmts: vec![],
                    return_expr: Some((ExprLiteral::Number(456f64).into(), 0..0))
                }
                .into(),
                0..0
            ))
        })),
        "if (true) { 123 } else { 456 }"
    );

    expr_display_test!(
        test_expr_if_else_with_stmts,
        Expr::If(Box::new(ExprIf {
            cond: (ExprLiteral::Bool(true).into(), 0..0),
            then: (
                ExprBlock {
                    stmts: vec![(
                        StmtExpr {
                            expr: (ExprLiteral::Number(123f64).into(), 0..0)
                        }
                        .into(),
                        0..0
                    )],
                    return_expr: Some((ExprLiteral::Number(456f64).into(), 0..0))
                }
                .into(),
                0..0
            ),
            else_: Some((
                ExprBlock {
                    stmts: vec![(
                        StmtExpr {
                            expr: (ExprLiteral::Number(789f64).into(), 0..0)
                        }
                        .into(),
                        0..0
                    )],
                    return_expr: Some((ExprLiteral::Number(0f64).into(), 0..0))
                }
                .into(),
                0..0
            ))
        })),
        "if (true) { 123; 456 } else { 789; 0 }"
    );

    expr_display_test!(
        test_expr_empty_list,
        Expr::List(ExprList { items: vec![] }),
        "[]"
    );

    expr_display_test!(
        test_expr_list,
        Expr::List(ExprList {
            items: vec![
                (ExprLiteral::Number(1f64).into(), 0..0),
                (ExprLiteral::Number(2f64).into(), 0..0),
                (ExprLiteral::Number(3f64).into(), 0..0)
            ]
        }),
        "[1, 2, 3]"
    );

    expr_display_test!(
        test_expr_tuple,
        Expr::Tuple(ExprTuple {
            items: vec![
                (ExprLiteral::Number(1f64).into(), 0..0),
                (ExprLiteral::Number(2f64).into(), 0..0),
                (ExprLiteral::Number(3f64).into(), 0..0)
            ]
        }),
        "(1, 2, 3,)"
    );

    expr_display_test!(
        test_expr_fn,
        Expr::Fn(Box::new(ExprFn {
            name: None,
            params: vec![],
            return_type: (TypeRef::unit(), 0..0),
            body: (
                ExprBlock {
                    stmts: vec![],
                    return_expr: None
                }
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
                    TypeRef::number()
                ),
                0..0
            )],
            return_type: (TypeRef::unit(), 0..0),
            body: (
                ExprBlock {
                    stmts: vec![],
                    return_expr: None
                }
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
                        TypeRef::number()
                    ),
                    0..0
                ),
                (
                    (
                        Identifier {
                            name: "b".to_string()
                        },
                        TypeRef::number()
                    ),
                    0..0
                )
            ],
            return_type: (TypeRef::unit(), 0..0),
            body: (
                ExprBlock {
                    stmts: vec![],
                    return_expr: None
                }
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
                        TypeRef::number()
                    ),
                    0..0
                ),
                (
                    (
                        Identifier {
                            name: "b".to_string()
                        },
                        TypeRef::number()
                    ),
                    0..0
                )
            ],
            return_type: (TypeRef::number(), 0..0),
            body: (
                ExprBlock {
                    stmts: vec![],
                    return_expr: None
                }
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
                        TypeRef::number()
                    ),
                    0..0
                ),
                (
                    (
                        Identifier {
                            name: "b".to_string()
                        },
                        TypeRef::number()
                    ),
                    0..0
                )
            ],
            return_type: (TypeRef::number(), 0..0),
            body: (
                ExprBlock {
                    stmts: vec![],
                    return_expr: Some((
                        Expr::Infix(Box::new(ExprInfix {
                            lt: (
                                Identifier {
                                    name: "a".to_string()
                                }
                                .into(),
                                0..0
                            ),
                            op: OpInfix::Add,
                            rt: (
                                Identifier {
                                    name: "b".to_string()
                                }
                                .into(),
                                0..0
                            )
                        })),
                        0..0
                    ))
                }
                .into(),
                0..0
            )
        })),
        "(a: number, b: number): number => { a + b }"
    );

    stmt_display_test! {
        test_stmt_assign_with_non_type_value,
        "let a: number = 123;".try_into().unwrap(),
        "let a: number = 123;"
    }

    stmt_display_test! {
        test_stmt_assign_with_no_value,
        "let a: number;".try_into().unwrap(),
        "let a: number;"
    }

    stmt_display_test! {
        test_stmt_assign_with_type_value,
        "type Number = number;".try_into().unwrap(),
        "type Number = number;"
    }
}
