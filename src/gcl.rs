//! Guarded Command Language

use petgraph::graph::NodeIndex;
use petgraph::stable_graph::{StableDiGraph, StableGraph};
use std::collections::HashMap;
use std::fmt;
use std::fmt::{Debug, Display, Formatter};
use std::ops::{Deref, DerefMut};

pub struct GclGraph {
    inner: StableDiGraph<GclNode, GclPredicate>,
    next_id_counter: usize,
    functions: HashMap<String, GclNodeRange>,
}

impl GclGraph {
    pub fn new() -> Self {
        GclGraph {
            inner: StableGraph::new(),
            next_id_counter: 0,
            functions: HashMap::new(),
        }
    }

    /// Create a new unique name, given the provided prefix.
    pub fn create_name(&mut self, prefix: &str) -> String {
        let name = format!("{}_{}", prefix, self.next_id_counter);
        self.next_id_counter += 1;
        name
    }

    /// Register the node range for a function. This is used later when calling
    /// it to set up edges.
    pub fn register_function(&mut self, name: String, range: GclNodeRange) {
        self.functions.insert(name, range);
    }

    pub fn get_function(&self, name: &str) -> Option<GclNodeRange> {
        self.functions.get(name).copied()
    }
}

impl Deref for GclGraph {
    type Target = StableDiGraph<GclNode, GclPredicate>;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl DerefMut for GclGraph {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

#[derive(Debug)]
pub struct GclNode {
    pub name: String,
    pub commands: Vec<GclCommand>,
}

impl GclNode {
    pub fn is_bug(&self) -> bool {
        self.commands.contains(&GclCommand::Bug)
    }
}

impl Display for GclNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        writeln!(f, "Node '{}'", self.name)?;

        for cmd in &self.commands {
            writeln!(f, "{};", cmd)?;
        }

        Ok(())
    }
}

/// Represents a sub-graph of nodes who all have `start` as a parent and who
/// all eventually lead to `end` (or exit the program/error out).
#[derive(Copy, Clone)]
pub struct GclNodeRange {
    pub start: NodeIndex,
    pub end: NodeIndex,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum GclCommand {
    Assignment(GclAssignment),
    /// Represents a bug in the program, ex. if an assert fails
    Bug,
}

impl Display for GclCommand {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            GclCommand::Assignment(assignment) => Display::fmt(assignment, f),
            GclCommand::Bug => f.write_str("bug"),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum GclPredicate {
    Equality(Box<GclPredicate>, Box<GclPredicate>),
    Conjunction(Box<GclPredicate>, Box<GclPredicate>),
    Disjunction(Box<GclPredicate>, Box<GclPredicate>),
    Implication(Box<GclPredicate>, Box<GclPredicate>),
    Negation(Box<GclPredicate>),
    Bool(bool),
    String(String),
    Var(String),
    StringVar(String),
}

impl Display for GclPredicate {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            GclPredicate::Equality(e1, e2) => write!(f, "({}) = ({})", e1, e2),
            GclPredicate::Conjunction(p1, p2) => write!(f, "({}) && ({})", p1, p2),
            GclPredicate::Disjunction(p1, p2) => write!(f, "({}) || ({})", p1, p2),
            GclPredicate::Implication(p1, p2) => write!(f, "({}) => ({})", p1, p2),
            GclPredicate::Negation(pred) => write!(f, "!({})", pred),
            GclPredicate::Bool(e) => Display::fmt(e, f),
            GclPredicate::String(s) => Debug::fmt(s, f),
            GclPredicate::Var(name) | GclPredicate::StringVar(name) => f.write_str(&name),
        }
    }
}

impl Default for GclPredicate {
    /// An always true GCL predicate
    fn default() -> Self {
        GclPredicate::Bool(true)
    }
}

impl GclPredicate {
    /// Get all of the variables this expression reads from
    pub fn find_all_vars(&self) -> Vec<String> {
        match self {
            GclPredicate::Bool(_) | GclPredicate::String(_) => Vec::new(),
            GclPredicate::Var(name) | GclPredicate::StringVar(name) => vec![name.clone()],
            GclPredicate::Negation(inner) => inner.find_all_vars(),
            GclPredicate::Conjunction(left, right)
            | GclPredicate::Disjunction(left, right)
            | GclPredicate::Equality(left, right)
            | GclPredicate::Implication(left, right) => {
                let mut vars = left.find_all_vars();
                vars.extend(right.find_all_vars());
                vars
            }
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct GclAssignment {
    pub name: String,
    pub pred: GclPredicate,
}

impl Display for GclAssignment {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{} := {}", self.name, self.pred)
    }
}
