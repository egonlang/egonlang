use std::fmt::{self, Debug, Display, Formatter};

use serde::{Deserialize, Serialize};

use crate::parser::parse;

use span::Spanned;

use super::{Expr, ExprS, Identifier, TypeRef};

/// A tuple containing a statement and it's span e.g. (stmt, span)
pub type StmtS = Spanned<Stmt>;
/// A statement (e.g. that does not produce a value)
#[derive(Clone, PartialEq, Serialize, Deserialize)]
pub enum Stmt {
    /// A statement that evaluates an expression then consumes the value.
    ///
    /// ```egon
    /// [1, 2, 3];
    /// ```
    Expr(StmtExpr),
    /// A statement that declares and/or initalizes a variable or constant
    ///
    /// ```egon
    /// let a: number = 123;
    /// const b = 456;
    /// ```
    Assign(StmtAssign),
    /// A statement that declares a type alias
    ///
    /// ```egon
    /// type Int = number;
    /// let a: Int = 123;
    /// ```
    TypeAlias(StmtTypeAlias),
    /// A statement declaring a function
    ///
    /// ```egon
    /// fn sum (a: number, b: number): number => { a + b }
    /// ```
    Fn(Box<StmtFn>),
    /// A statement to assert an expression's type
    ///
    /// ```egon
    /// assert_type 123 number;
    /// ```
    AssertType(StmtAssertType),
    /// A statement to return a value from a function
    ///
    /// ```egon
    /// (): number => { return 123; }
    /// ```
    Return(StmtReturn),
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

impl From<StmtAssertType> for Stmt {
    fn from(value: StmtAssertType) -> Self {
        Stmt::AssertType(value)
    }
}

impl TryFrom<&str> for Stmt {
    type Error = egonlang_errors::EgonError;

    fn try_from(value: &str) -> Result<Stmt, egonlang_errors::EgonError> {
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
            Stmt::TypeAlias(stmt) => f.write_fmt(format_args!("{}", stmt)),
            Stmt::Fn(stmt) => f.write_fmt(format_args!("{}", stmt)),
            Stmt::AssertType(stmt) => f.write_fmt(format_args!("{}", stmt)),
            Stmt::Return(stmt) => f.write_fmt(format_args!("{}", stmt)),
            Stmt::Error => todo!(),
        }
    }
}

impl Debug for Stmt {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Expr(arg0) => f.write_fmt(format_args!("{:#?}", arg0)),
            Self::Assign(arg0) => f.write_fmt(format_args!("{:#?}", arg0)),
            Self::TypeAlias(arg0) => f.write_fmt(format_args!("{:#?}", arg0)),
            Self::Fn(arg0) => f.write_fmt(format_args!("{:#?}", arg0)),
            Self::AssertType(arg0) => f.write_fmt(format_args!("{:#?}", arg0)),
            Self::Return(arg0) => f.write_fmt(format_args!("{:#?}", arg0)),
            Self::Error => write!(f, "Error"),
        }
    }
}

/// A statement that evaluates an expression then consumes the value.
///
/// ```egon
/// [1, 2, 3];
/// ```
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct StmtExpr {
    pub expr: ExprS,
}

impl Display for StmtExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("{};", self.expr.0))
    }
}

/// A statement that declares and/or initalizes a variable or constant
///
/// ```egon
/// let a: number = 123;
/// const b = 456;
/// ```
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct StmtAssign {
    pub identifier: Spanned<Identifier>,
    pub type_expr: Option<ExprS>,
    pub is_const: bool,
    pub value: Option<ExprS>,
}

impl Display for StmtAssign {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let decl = if self.is_const { "const" } else { "let" };
        let name = &self.identifier.0.name;

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

/// A statement that declares a type alias
///
/// ```egon
/// type Int = number;
/// let a: Int = 123;
/// ```
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct StmtTypeAlias {
    pub alias: Spanned<Identifier>,
    pub value: Spanned<TypeRef>,
}

impl Display for StmtTypeAlias {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let alias = &self.alias.0.name;
        let value = &self.value.0.to_string();

        f.write_fmt(format_args!("type {} = {};", alias, value))
    }
}

/// A statement declaring a function
///
/// ```egon
/// fn sum (a: number, b: number): number => { a + b }
/// ```
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct StmtFn {
    pub name: Spanned<Identifier>,
    pub fn_expr: ExprS,
}

impl Display for StmtFn {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("fn {} {}", self.name.0.name, self.fn_expr.0))
    }
}

/// A statement to assert an expression's type
///
/// ```egon
/// assert_type 123 number;
/// ```
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct StmtAssertType {
    pub value: ExprS,
    pub expected_type: ExprS,
}

impl Display for StmtAssertType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!(
            "assert_type {}, {};",
            self.value.0, self.expected_type.0
        ))
    }
}

/// A statement to return a value from a function
///
/// ```egon
/// (): number => { return 123; }
/// ```
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct StmtReturn {
    pub value: ExprS,
    used_in_block: bool,
}

impl StmtReturn {
    pub fn new(value: ExprS) -> Self {
        Self {
            value,
            used_in_block: false,
        }
    }
    pub fn set_used_in_block(&mut self) {
        self.used_in_block = true;
    }

    pub fn get_used_in_block(&self) -> bool {
        self.used_in_block
    }
}

impl Display for StmtReturn {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("return {};", self.value.0))
    }
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    macro_rules! stmt_display_test {
        ($test_name:ident, $stmt:expr, $expected:expr) => {
            #[test]
            fn $test_name() {
                let stmt: $crate::ast::Stmt = $stmt;
                assert_eq!($expected, stmt.to_string());
            }
        };
    }

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

    stmt_display_test! {
        test_stmt_assert_type,
        "assert_type 123, number;".try_into().unwrap(),
        "assert_type 123, number;"
    }

    stmt_display_test! {
        test_stmt_return,
        "return 123;".try_into().unwrap(),
        "return 123;"
    }
}
