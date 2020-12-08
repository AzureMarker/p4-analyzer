//! Convert P4 to GCL

use crate::ast::{
    Assignment, BlockStatement, ControlDecl, Declaration, Expr, IfStatement, Program, Statement,
};
use crate::gcl::{GclAssignment, GclCommand, GclPredicate};
use std::collections::HashMap;

/// A "no operation" command in GCL
const GCL_NO_OP: GclCommand = GclCommand::Assumption(GclPredicate::Bool(true));

/// Trait for converting a P4 AST node into GCL
pub trait ToGcl {
    type Output;

    fn to_gcl(&self) -> Self::Output;
}

impl ToGcl for Program {
    type Output = HashMap<String, GclCommand>;

    fn to_gcl(&self) -> Self::Output {
        self.declarations.iter().map(Declaration::to_gcl).collect()
    }
}

impl ToGcl for Declaration {
    type Output = (String, GclCommand);

    fn to_gcl(&self) -> Self::Output {
        match self {
            Declaration::Control(control) => control.to_gcl(),
        }
    }
}

impl ToGcl for ControlDecl {
    type Output = (String, GclCommand);

    fn to_gcl(&self) -> Self::Output {
        (self.name.clone(), self.body.to_gcl())
    }
}

impl ToGcl for BlockStatement {
    type Output = GclCommand;

    fn to_gcl(&self) -> Self::Output {
        let mut iterator = self.0.iter().map(Statement::to_gcl).rev();
        let last = iterator.next().unwrap_or(GCL_NO_OP);

        iterator.fold(last, |acc, next| {
            GclCommand::Sequence(Box::new(next), Box::new(acc))
        })
    }
}

impl ToGcl for Statement {
    type Output = GclCommand;

    fn to_gcl(&self) -> Self::Output {
        match self {
            Statement::Assignment(Assignment { name, value, .. }) => {
                GclCommand::Assignment(GclAssignment {
                    name: name.clone(),
                    pred: value.to_gcl(),
                })
            }
            Statement::Block(block) => block.to_gcl(),
            Statement::If(IfStatement {
                condition,
                then_case,
                else_case,
            }) => {
                let pred = condition.to_gcl();
                let negated_pred = GclPredicate::Negation(Box::new(pred.clone()));
                let then_case_gcl = then_case.to_gcl();
                let else_case_gcl = else_case
                    .as_ref()
                    .map(BlockStatement::to_gcl)
                    .unwrap_or(GCL_NO_OP);

                // A choice of two branches.
                // The "then" branch assumes the conditional, while the "else"
                // branch assumes the negated conditional.
                // TODO: is this the correct translation of an "if" into p4v's
                //       version of GCL?
                GclCommand::Choice(
                    Box::new(GclCommand::Sequence(
                        Box::new(GclCommand::Assumption(pred)),
                        Box::new(then_case_gcl),
                    )),
                    Box::new(GclCommand::Sequence(
                        Box::new(GclCommand::Assumption(negated_pred)),
                        Box::new(else_case_gcl),
                    )),
                )
            }
        }
    }
}

impl ToGcl for Expr {
    type Output = GclPredicate;

    fn to_gcl(&self) -> Self::Output {
        match self {
            Expr::Bool(b) => GclPredicate::Bool(*b),
            Expr::Var(name) => GclPredicate::Var(name.clone()),
            Expr::And(left, right) => {
                GclPredicate::Conjunction(Box::new(left.to_gcl()), Box::new(right.to_gcl()))
            }
            Expr::Or(left, right) => {
                GclPredicate::Disjunction(Box::new(left.to_gcl()), Box::new(right.to_gcl()))
            }
            Expr::Negation(inner) => GclPredicate::Negation(Box::new(inner.to_gcl())),
        }
    }
}
