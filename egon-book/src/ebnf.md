# EBNF

This is a rough translation of the current grammar. It may be incorrect. Please refer to the [grammar file](https://github.com/egonlang/egonlang/blob/main/egonlang-core/src/grammar.lalrpop) (LALRPOP).

```ebnf
Module = Stmt+

Stmt =
    | StmtExpr
    | StmtAssertType
    | StmtReturn
    | StmtAssignVariable
    | StmtAssignConst
    | StmtTypeAlias
    | TypeDefStmt
    | StmtFn

ExprStmt = Expr ";"
AssertTypeStmt = "assert_type" Expr "," Expr ";"
ReturnStmt = "return" Expr ";"
StmtAssignVariable =
    | "let" Identifier ":" Type Expr ";"
    | "let" Identifier ":" Type Expr "=" Expr ";"
    | "let" Identifier "=" Expr ";"
StmtAssignConst =
    | "const" Identifier ":" Type "=" Expr ";"
    | "const" Identifier "=" Expr ";"
StmtTypeAlias = "type" Identifier "=" Type ";"
StmtFn = "fn" Identifier "(" ParamList ")" ":" Type "=>" Block

Expr =
    | ExprUnit
    | ExprLiteral
    | ExprAssign
    | ExprIf
    | ExprCall
    | ExprInfix
    | ExprPrefix
    | ExprList
    | ExprTuple
    | ExprRange
    | ExprBlock
    | "(" Expr ")"

ExprUnit = "(" ")"

ExprLiteral = Bool | Number | String;

ExprAssign = Identifier = Expr

ExprIf =
    | "if" Expr Block
    | "if" Expr Block else Block
    | "if" Expr Block else ExprIf

ExprCall = |
    Expr "(" ")"
    Expr "(" Expr ("," Expr)* ")"

ExprInfix = Expr InfixOp Expr
InfixOp = 
    |"+" | "-" | "*" | "/" | "%"
    | "and" | "or"
    | "<" | ">" | "<=" | ">=" | "==" | "!="

ExprPrefix = "-" | "!" Expr

ExprList = "[" Expr ("," Expr)* "]"
ExprTuple = "(" Expr ("," Expr)* "," ")"
ExprRange = Expr ".." Expr
ExprBlock = "{" Stmt* (Expr)? "}"

ParamList = Param ("," Param)*
Param = Identifier ":" Type

Type = Identifier (<"," Type>*)?
Identifier = <identifier>
String = <string>
Number = <number>
Bool = "true" | "false"
```