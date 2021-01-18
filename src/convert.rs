//! Convert P4 to GCL

use crate::ast::{
    BlockStatement, ConstantDecl, ControlDecl, ControlLocalDecl, Declaration, Expr, IfStatement,
    Instantiation, Program, Statement, StatementOrDecl, VariableDecl,
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
            Declaration::Constant(const_decl) => (const_decl.name.clone(), const_decl.to_gcl()),
        }
    }
}

impl ToGcl for ControlDecl {
    type Output = (String, GclCommand);

    fn to_gcl(&self) -> Self::Output {
        (
            self.name.clone(),
            GclCommand::Sequence(
                Box::new(self.local_decls.to_gcl()),
                Box::new(self.apply_body.to_gcl()),
            ),
        )
    }
}

impl ToGcl for ConstantDecl {
    type Output = GclCommand;

    fn to_gcl(&self) -> Self::Output {
        GclCommand::Sequence(
            Box::new(GclCommand::Assignment(GclAssignment {
                name: format!("_var_has_value__{}", self.name),
                pred: GclPredicate::Bool(true),
            })),
            Box::new(GclCommand::Assignment(GclAssignment {
                name: self.name.clone(),
                pred: self.initializer.to_gcl(),
            })),
        )
    }
}

impl<T: ToGcl<Output = GclCommand>> ToGcl for [T] {
    type Output = GclCommand;

    fn to_gcl(&self) -> Self::Output {
        match self.split_first() {
            Some((head, [])) => head.to_gcl(),
            Some((head, tail)) => {
                GclCommand::Sequence(Box::new(head.to_gcl()), Box::new(tail.to_gcl()))
            }
            None => GCL_NO_OP,
        }
    }
}

impl ToGcl for ControlLocalDecl {
    type Output = GclCommand;

    fn to_gcl(&self) -> Self::Output {
        match self {
            ControlLocalDecl::Variable(var_decl) => var_decl.to_gcl(),
            ControlLocalDecl::Instantiation(instantiation) => instantiation.to_gcl(),
            ControlLocalDecl::Constant(const_decl) => const_decl.to_gcl(),
        }
    }
}

impl ToGcl for BlockStatement {
    type Output = GclCommand;

    fn to_gcl(&self) -> Self::Output {
        let mut iterator = self.0.iter().map(StatementOrDecl::to_gcl).rev();
        let last = iterator.next().unwrap_or(GCL_NO_OP);

        iterator.fold(last, |acc, next| {
            GclCommand::Sequence(Box::new(next), Box::new(acc))
        })
    }
}

impl ToGcl for StatementOrDecl {
    type Output = GclCommand;

    fn to_gcl(&self) -> Self::Output {
        match self {
            StatementOrDecl::Statement(statement) => statement.to_gcl(),
            StatementOrDecl::VariableDecl(var_decl) => var_decl.to_gcl(),
            StatementOrDecl::ConstantDecl(const_decl) => const_decl.to_gcl(),
            StatementOrDecl::Instantiation(instantiation) => instantiation.to_gcl(),
        }
    }
}

impl ToGcl for Statement {
    type Output = GclCommand;

    fn to_gcl(&self) -> Self::Output {
        match self {
            Statement::Block(block) => block.to_gcl(),
            Statement::If(if_statement) => if_statement.to_gcl(),
        }
    }
}

impl ToGcl for VariableDecl {
    type Output = GclCommand;

    fn to_gcl(&self) -> Self::Output {
        let has_value_command = GclCommand::Assignment(GclAssignment {
            name: format!("_var_has_value__{}", self.name),
            pred: GclPredicate::Bool(self.value.is_some()),
        });

        match self.value.as_ref() {
            Some(value) => GclCommand::Sequence(
                Box::new(has_value_command),
                Box::new(GclCommand::Assignment(GclAssignment {
                    name: self.name.clone(),
                    pred: value.to_gcl(),
                })),
            ),
            None => has_value_command,
        }
    }
}

impl ToGcl for Instantiation {
    type Output = GclCommand;

    fn to_gcl(&self) -> Self::Output {
        GclCommand::Assignment(GclAssignment {
            name: format!("_var_has_value__{}", self.name),
            pred: GclPredicate::Bool(false),
        })
    }
}

impl ToGcl for IfStatement {
    type Output = GclCommand;

    fn to_gcl(&self) -> Self::Output {
        // Make sure that all the variables read by this conditional have a value
        let vars = self.condition.find_all_vars();
        let mut var_value_check_iter = vars
            .into_iter()
            .map(|var| GclCommand::Assert(GclPredicate::Var(format!("_var_has_value__{}", var))));
        let var_value_check_first = var_value_check_iter.next().unwrap_or(GCL_NO_OP);
        let var_value_check_gcl = var_value_check_iter.rfold(var_value_check_first, |acc, next| {
            GclCommand::Sequence(Box::new(next), Box::new(acc))
        });

        let pred = self.condition.to_gcl();
        let negated_pred = GclPredicate::Negation(Box::new(pred.clone()));
        let then_case_gcl = self.then_case.to_gcl();
        let else_case_gcl = self
            .else_case
            .as_ref()
            .map(BlockStatement::to_gcl)
            .unwrap_or(GCL_NO_OP);

        // A choice of two branches.
        // The "then" branch assumes the conditional, while the "else"
        // branch assumes the negated conditional.
        // TODO: is this the correct translation of an "if" into p4v's
        //       version of GCL?
        GclCommand::Sequence(
            Box::new(var_value_check_gcl),
            Box::new(GclCommand::Choice(
                Box::new(GclCommand::Sequence(
                    Box::new(GclCommand::Assumption(pred)),
                    Box::new(then_case_gcl),
                )),
                Box::new(GclCommand::Sequence(
                    Box::new(GclCommand::Assumption(negated_pred)),
                    Box::new(else_case_gcl),
                )),
            )),
        )
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

impl Expr {
    /// Get all of the variables this expression reads from
    fn find_all_vars(&self) -> Vec<String> {
        match self {
            Expr::Bool(_) => Vec::new(),
            Expr::Var(name) => vec![name.clone()],
            Expr::And(left, right) | Expr::Or(left, right) => {
                let mut vars = left.find_all_vars();
                vars.extend(right.find_all_vars());
                vars
            }
            Expr::Negation(inner) => inner.find_all_vars(),
        }
    }
}
