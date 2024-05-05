use egonlang_core::{
    ast::{Expr, Module, Stmt},
    errors::ErrorS,
    span::Span,
};

use crate::{
    rules::{
        const_declaration_with_no_value::DeclareConstWithoutValue,
        reassigning_const_values::ReassigningConstValueRule, rule::Rule,
        type_mismatch_infix::TypeMismatchInfixRule,
        type_mismatch_list_items::TypeMisMatchListItemsRule,
        type_mismatch_negate_prefix::TypeMisMatchNegatePrefixRule,
        type_mismatch_on_declarations::TypeMismatchOnDeclarationsRule,
        undefined_identifier::UndefinedIdentifierRule,
    },
    type_env::{TypeEnv, TypeEnvValue},
    visitor::Visitor,
};

pub type VerificationResult = Result<(), Vec<ErrorS>>;

#[derive(Default)]
pub struct Verifier<'a> {
    rules: Vec<Box<dyn Rule<'a>>>,
}

impl Verifier<'_> {
    pub fn new() -> Self {
        let mut verifier = Verifier::default();

        verifier.rules.push(Box::from(TypeMismatchInfixRule));
        verifier.rules.push(Box::from(TypeMisMatchNegatePrefixRule));
        verifier.rules.push(Box::from(TypeMisMatchListItemsRule));
        verifier
            .rules
            .push(Box::from(TypeMismatchOnDeclarationsRule));
        verifier.rules.push(Box::from(DeclareConstWithoutValue));
        verifier.rules.push(Box::from(ReassigningConstValueRule));
        verifier.rules.push(Box::from(UndefinedIdentifierRule));

        verifier
    }

    pub fn verify(&self, module: &Module) -> VerificationResult {
        let mut all_errs: Vec<ErrorS> = vec![];

        let mut types = TypeEnv::new();

        for (stmt, stmt_span) in &module.stmts {
            if let Err(stmt_errs) = self.visit_stmt(stmt, stmt_span, &mut types) {
                all_errs.extend(stmt_errs);
            }
        }

        if !all_errs.is_empty() {
            return Err(all_errs);
        }

        Ok(())
    }
}

impl<'a> Visitor<'a> for Verifier<'a> {
    fn visit_stmt(
        &self,
        stmt: &Stmt,
        span: &Span,
        types: &mut TypeEnv<'a>,
    ) -> Result<(), Vec<ErrorS>> {
        match stmt {
            Stmt::Expr(stmt_expr) => {
                let mut errs: Vec<ErrorS> = vec![];

                for rule in &self.rules {
                    let rule_errs = rule.visit_stmt(stmt, span, types).err().unwrap_or_default();

                    errs.extend(rule_errs);
                }

                let (expr, expr_span) = &stmt_expr.expr;
                let expr_errs = self
                    .visit_expr(expr, expr_span, types)
                    .err()
                    .unwrap_or_default();

                errs.extend(expr_errs);

                if !errs.is_empty() {
                    return Err(errs);
                }

                Ok(())
            }
            Stmt::Assign(stmt_assign) => {
                let mut errs: Vec<ErrorS> = vec![];

                for rule in &self.rules {
                    let rule_errs = rule.visit_stmt(stmt, span, types).err().unwrap_or_default();

                    errs.extend(rule_errs);
                }

                if let Some((type_expr, type_expr_span)) = &stmt_assign.type_expr {
                    let type_expr_errs = self
                        .visit_expr(type_expr, type_expr_span, types)
                        .err()
                        .unwrap_or_default();

                    errs.extend(type_expr_errs);
                }

                if let Some((value_expr, value_expr_span)) = &stmt_assign.value {
                    let value_expr_errs = self
                        .visit_expr(value_expr, value_expr_span, types)
                        .err()
                        .unwrap_or_default();

                    errs.extend(value_expr_errs);
                }

                if !errs.is_empty() {
                    return Err(errs);
                }

                match (&stmt_assign.type_expr, &stmt_assign.value) {
                    (None, None) => todo!(),
                    (None, Some((value_expr, value_span))) => {
                        let value_typeref = types.resolve_expr_type(value_expr, value_span)?;
                        types.set(
                            &stmt_assign.identifier.name,
                            TypeEnvValue {
                                typeref: value_typeref,
                                is_const: stmt_assign.is_const,
                            },
                        );
                    }
                    (Some((type_expr, type_span)), None) => {
                        let type_typeref = types.resolve_expr_type(type_expr, type_span)?;

                        types.set(
                            &stmt_assign.identifier.name,
                            TypeEnvValue {
                                typeref: type_typeref,
                                is_const: stmt_assign.is_const,
                            },
                        );
                    }
                    (Some((type_expr, type_span)), Some((_value_expr, _value_span))) => {
                        let type_typeref = types.resolve_expr_type(type_expr, type_span)?;

                        types.set(
                            &stmt_assign.identifier.name,
                            TypeEnvValue {
                                typeref: type_typeref,
                                is_const: stmt_assign.is_const,
                            },
                        );
                    }
                };

                Ok(())
            }
            Stmt::Fn(stmt_fn) => {
                let mut errs: Vec<ErrorS> = vec![];

                let (fn_expr, fn_expr_span) = &stmt_fn.fn_expr;

                let fn_expr_errs = self
                    .visit_expr(fn_expr, fn_expr_span, types)
                    .err()
                    .unwrap_or_default();

                errs.extend(fn_expr_errs);

                if !errs.is_empty() {
                    return Err(errs);
                }

                //
                Ok(())
            }
            Stmt::Error => Ok(()),
        }
    }

    fn visit_expr(
        &self,
        expr: &Expr,
        span: &Span,
        types: &mut TypeEnv<'a>,
    ) -> Result<(), Vec<ErrorS>> {
        let mut errs: Vec<ErrorS> = vec![];

        for rule in &self.rules {
            let rule_errs = rule.visit_expr(expr, span, types).err().unwrap_or_default();

            errs.extend(rule_errs);
        }

        match expr {
            Expr::Block(block_expr) => {
                for (stmt, stmt_span) in &block_expr.stmts {
                    let stmt_errs = self
                        .visit_stmt(stmt, stmt_span, types)
                        .err()
                        .unwrap_or_default();

                    errs.extend(stmt_errs);
                }

                if block_expr.return_expr.is_some() {
                    let (return_expr, return_span) = block_expr.return_expr.as_ref().unwrap();
                    let expr_errs = self
                        .visit_expr(return_expr, return_span, types)
                        .err()
                        .unwrap_or_default();

                    errs.extend(expr_errs);
                }
            }
            Expr::Assign(assign_expr) => {
                let (value_expr, value_span) = &assign_expr.value;

                let value_errs = self
                    .visit_expr(value_expr, value_span, types)
                    .err()
                    .unwrap_or_default();

                errs.extend(value_errs);
            }
            Expr::Infix(infix_expr) => {
                let (lt_expr, lt_span) = &infix_expr.lt;

                let lt_errs = self
                    .visit_expr(lt_expr, lt_span, types)
                    .err()
                    .unwrap_or_default();

                errs.extend(lt_errs);

                let (rt_expr, rt_span) = &infix_expr.rt;

                let rt_errs = self
                    .visit_expr(rt_expr, rt_span, types)
                    .err()
                    .unwrap_or_default();

                errs.extend(rt_errs);
            }
            _ => {}
        };

        if !errs.is_empty() {
            return Err(errs);
        }

        Ok(())
    }
}

#[cfg(test)]
mod verifier_tests {
    use egonlang_core::{
        ast::{ExprAssign, ExprList, ExprLiteral, Identifier, Module, StmtExpr, TypeRef},
        errors::TypeError,
    };
    use pretty_assertions::assert_eq;

    use super::Verifier;

    #[test]
    fn works_with_empty_module() {
        let verifier = Verifier::new();

        let module = Module::default();

        let results = verifier.verify(&module);

        assert_eq!(Ok(()), results);
    }

    #[test]
    fn errors_when_referencing_undefined_identifier() {
        let verifier = Verifier::new();

        let module = Module::from(vec![(
            StmtExpr {
                expr: (
                    Identifier {
                        name: "a".to_string(),
                    }
                    .into(),
                    1..2,
                ),
            }
            .into(),
            0..3,
        )]);

        let results = verifier.verify(&module);

        assert_eq!(
            Err(vec![(TypeError::Undefined("a".to_string()).into(), 1..2)]),
            results
        );
    }

    #[test]
    fn errors_when_referencing_multiple_undefined_identifiers() {
        let verifier = Verifier::new();

        let module = Module::from(vec![
            (
                StmtExpr {
                    expr: (
                        Identifier {
                            name: "a".to_string(),
                        }
                        .into(),
                        1..2,
                    ),
                }
                .into(),
                0..0,
            ),
            (
                StmtExpr {
                    expr: (
                        Identifier {
                            name: "b".to_string(),
                        }
                        .into(),
                        5..6,
                    ),
                }
                .into(),
                0..0,
            ),
        ]);

        let results = verifier.verify(&module);

        assert_eq!(
            Err(vec![
                (TypeError::Undefined("a".to_string()).into(), 1..2),
                (TypeError::Undefined("b".to_string()).into(), 5..6)
            ]),
            results
        );
    }

    #[test]
    fn errors_when_assigning_list_with_type_mismatched_items() {
        let verifier = Verifier::new();

        let module = Module::from(vec![(
            StmtExpr {
                expr: (
                    ExprAssign {
                        identifier: Identifier {
                            name: "a".to_string(),
                        },
                        value: (
                            ExprList {
                                items: vec![
                                    (ExprLiteral::Number(10f64).into(), 0..1),
                                    (ExprLiteral::Bool(false).into(), 2..3),
                                ],
                            }
                            .into(),
                            0..0,
                        ),
                    }
                    .into(),
                    1..2,
                ),
            }
            .into(),
            0..0,
        )]);

        let results = verifier.verify(&module);

        assert_eq!(
            Err(vec![(
                TypeError::MismatchType {
                    expected: TypeRef::number().to_string(),
                    actual: TypeRef::bool().to_string()
                }
                .into(),
                2..3
            )]),
            results
        );
    }
}
