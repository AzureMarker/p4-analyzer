//! Guarded Command Language

use crate::ir::VariableId;
use petgraph::graph::NodeIndex;
use petgraph::stable_graph::{StableDiGraph, StableGraph};
use std::collections::HashMap;
use std::fmt;
use std::fmt::{Debug, Display, Formatter, Write};
use std::ops::{Deref, DerefMut};

pub struct GclGraph {
    inner: StableDiGraph<GclNode, GclPredicate>,
    next_id_counter: usize,
    functions: HashMap<VariableId, GclNodeRange>,
    var_locations: HashMap<VariableId, MemoryLocation>,
}

impl GclGraph {
    pub fn new() -> Self {
        GclGraph {
            inner: StableGraph::new(),
            next_id_counter: 0,
            functions: HashMap::new(),
            var_locations: HashMap::new(),
        }
    }

    /// Create a new unique name, given the provided prefix.
    pub fn create_name(&mut self, prefix: &str) -> String {
        let name = format!("{}_{}", prefix, self.next_id_counter);
        self.next_id_counter += 1;
        name
    }

    pub fn fresh_mem_location(&mut self) -> MemoryLocation {
        let loc = MemoryLocation::Var(self.next_id_counter);
        self.next_id_counter += 1;
        loc
    }

    /// Register the node range for a function. This is used later when calling
    /// it to set up edges.
    pub fn register_function(&mut self, id: VariableId, range: GclNodeRange) {
        self.functions.insert(id, range);
    }

    pub fn get_function(&self, id: VariableId) -> Option<GclNodeRange> {
        self.functions.get(&id).copied()
    }

    pub fn get_var_location(&mut self, var: VariableId) -> MemoryLocation {
        *self
            .var_locations
            .entry(var)
            .or_insert_with(|| self.fresh_mem_location())
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
    AddFact(GclFact),
    /// Represents a bug in the program, ex. if an assert fails
    Bug,
}

impl Display for GclCommand {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            GclCommand::Assignment(assignment) => Display::fmt(assignment, f),
            GclCommand::AddFact(fact) => write!(f, "add_fact({})", fact),
            GclCommand::Bug => f.write_str("bug"),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum GclPredicate {
    Equality(Box<GclPredicate>, Box<GclPredicate>),
    Conjunction(Box<GclPredicate>, Box<GclPredicate>),
    Disjunction(Box<GclPredicate>, Box<GclPredicate>),
    Negation(Box<GclPredicate>),
    Bool(bool),
    String(String),
    Fact(GclFact),
    Var(MemoryLocation),
    StringVar(MemoryLocation),
}

impl Display for GclPredicate {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            GclPredicate::Equality(e1, e2) => write!(f, "({}) = ({})", e1, e2),
            GclPredicate::Conjunction(p1, p2) => write!(f, "({}) && ({})", p1, p2),
            GclPredicate::Disjunction(p1, p2) => write!(f, "({}) || ({})", p1, p2),
            GclPredicate::Negation(pred) => write!(f, "!({})", pred),
            GclPredicate::Bool(e) => Display::fmt(e, f),
            GclPredicate::String(s) => Debug::fmt(s, f),
            GclPredicate::Fact(fact) => Display::fmt(fact, f),
            GclPredicate::Var(loc) | GclPredicate::StringVar(loc) => Display::fmt(loc, f),
        }
    }
}

impl Default for GclPredicate {
    /// An always true GCL predicate
    fn default() -> Self {
        GclPredicate::Bool(true)
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct GclAssignment {
    pub lvalue: GclLValue,
    pub expr: GclExpr,
}

impl Display for GclAssignment {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{} := {}", self.lvalue, self.expr)
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum GclLValue {
    Var(MemoryLocation),
    Field(Box<GclLValue>, String),
}

impl GclLValue {
    pub fn mem_location(&self) -> MemoryLocation {
        match self {
            GclLValue::Var(loc) => *loc,
            GclLValue::Field(target, _) => target.mem_location(),
        }
    }
}

impl Display for GclLValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            GclLValue::Var(loc) => Display::fmt(loc, f),
            GclLValue::Field(target, field) => {
                write!(f, "{}.{}", target, field)
            }
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum MemoryLocation {
    ReturnVal,
    Var(usize),
}

impl Display for MemoryLocation {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            MemoryLocation::ReturnVal => f.write_str("loc_ret"),
            MemoryLocation::Var(id) => {
                write!(f, "loc_{}", id)
            }
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum GclValue {
    Bool(bool),
    String(String),
    Struct { fields: Vec<(String, GclValue)> },
}

impl Display for GclValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            GclValue::Bool(b) => Display::fmt(b, f),
            GclValue::String(s) => Debug::fmt(s, f),
            GclValue::Struct { fields } => {
                f.write_str("{ ")?;

                for (i, (name, value)) in fields.iter().enumerate() {
                    write!(f, "{}: {}", name, value)?;

                    if i == fields.len() - 1 {
                        f.write_char(' ')?;
                    } else {
                        f.write_str(", ")?;
                    }
                }

                f.write_str("}")
            }
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum GclExpr {
    Value(GclValue),
    Var(MemoryLocation),
    Negate(MemoryLocation),
}

impl Display for GclExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            GclExpr::Value(value) => Display::fmt(value, f),
            GclExpr::Var(loc) => Display::fmt(loc, f),
            GclExpr::Negate(loc) => {
                write!(f, "!({})", loc)
            }
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum GclFact {
    HasValue(MemoryLocation),
}

impl Display for GclFact {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            GclFact::HasValue(loc) => {
                write!(f, "has_value({})", loc)
            }
        }
    }
}
