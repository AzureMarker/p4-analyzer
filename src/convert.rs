//! Convert P4 to GCL

use crate::ast::{Assignment, ControlDecl, Declaration, Expr, Program, Statement};
use crate::gcl::{GclAssignment, GclCommand, GclExpr};

/// Trait for converting a P4 AST node into GCL
pub trait ToGcl {
    type Output;

    fn to_gcl(&self) -> Self::Output;
}

impl ToGcl for Program {
    type Output = Vec<GclCommand>;

    fn to_gcl(&self) -> Self::Output {
        self.declarations
            .iter()
            .filter_map(Declaration::to_gcl)
            .collect()
    }
}

impl ToGcl for Declaration {
    type Output = Option<GclCommand>;

    fn to_gcl(&self) -> Self::Output {
        match self {
            Declaration::Control(control) => control.to_gcl(),
        }
    }
}

impl ToGcl for ControlDecl {
    type Output = Option<GclCommand>;

    fn to_gcl(&self) -> Self::Output {
        let mut iterator = self.body.iter().map(Statement::to_gcl);
        let first = iterator.next()?;

        Some(iterator.fold(first, |acc, next| {
            GclCommand::Sequence(Box::new(acc), Box::new(next))
        }))
    }
}

impl ToGcl for Statement {
    type Output = GclCommand;

    fn to_gcl(&self) -> Self::Output {
        match self {
            Statement::Assignment(Assignment { name, value, .. }) => {
                GclCommand::Assignment(GclAssignment {
                    name: name.clone(),
                    expr: value.to_gcl(),
                })
            }
        }
    }
}

impl ToGcl for Expr {
    type Output = GclExpr;

    fn to_gcl(&self) -> Self::Output {
        match self {
            Expr::Bool(b) => GclExpr::Bool(*b),
        }
    }
}
