//! Guarded Command Language

use std::fmt;
use std::fmt::{Debug, Display, Formatter};

#[derive(Debug)]
pub enum GclCommand {
    Assignment(GclAssignment),
    Sequence(Box<GclCommand>, Box<GclCommand>),
    Choice(Box<GclCommand>, Box<GclCommand>),
    Assumption(GclPredicate),
    Assert(GclPredicate),
}

impl Display for GclCommand {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            GclCommand::Assignment(assignment) => Display::fmt(assignment, f),
            GclCommand::Sequence(cmd1, cmd2) => write!(f, "{}; {}", cmd1, cmd2),
            GclCommand::Choice(cmd1, cmd2) => write!(f, "{} [] {}", cmd1, cmd2),
            GclCommand::Assumption(pred) => write!(f, "assume({})", pred),
            GclCommand::Assert(pred) => write!(f, "assert({})", pred),
        }
    }
}

#[derive(Debug)]
pub enum GclPredicate {
    Equality(GclExpr, GclExpr),
    Conjunction(Box<GclPredicate>, Box<GclPredicate>),
    Disjunction(Box<GclPredicate>, Box<GclPredicate>),
    Implication(Box<GclPredicate>, Box<GclPredicate>),
    Negation(Box<GclPredicate>),
    Bool(bool),
}

impl Display for GclPredicate {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            GclPredicate::Equality(e1, e2) => write!(f, "{} = {}", e1, e2),
            GclPredicate::Conjunction(p1, p2) => write!(f, "{} && {}", p1, p2),
            GclPredicate::Disjunction(p1, p2) => write!(f, "{} || {}", p1, p2),
            GclPredicate::Implication(p1, p2) => write!(f, "{} => {}", p1, p2),
            GclPredicate::Negation(pred) => write!(f, "!({})", pred),
            GclPredicate::Bool(b) => Display::fmt(b, f),
        }
    }
}

#[derive(Debug)]
pub struct GclAssignment {
    pub name: String,
    pub expr: GclExpr,
}

impl Display for GclAssignment {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{} := {}", self.name, self.expr)
    }
}

#[derive(Debug)]
pub enum GclExpr {
    Bool(bool),
}

impl Display for GclExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            GclExpr::Bool(b) => Display::fmt(b, f),
        }
    }
}
