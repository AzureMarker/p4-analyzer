//! Guarded Command Language

use crate::ir::{IrType, VariableId};
use petgraph::graph::NodeIndex;
use petgraph::stable_graph::{StableDiGraph, StableGraph};
use std::collections::HashMap;
use std::fmt;
use std::fmt::{Debug, Display, Formatter, Write};
use std::ops::{Deref, DerefMut};

pub struct GclGraph {
    inner: StableDiGraph<GclNode, GclExpr>,
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

    pub fn fresh_mem_location(&mut self, name: Option<String>) -> MemoryLocation {
        let loc = MemoryLocation::Var(self.next_id_counter, name);
        self.next_id_counter += 1;
        loc
    }

    /// Register the node range for a function. This is used later when calling
    /// it to set up edges.
    pub fn register_function(&mut self, id: VariableId, range: GclNodeRange) {
        self.functions.insert(id, range);
    }

    pub fn get_function(&self, id: &VariableId) -> Option<GclNodeRange> {
        self.functions.get(id).copied()
    }

    pub fn get_var_location(&mut self, var: &VariableId) -> MemoryLocation {
        if let Some(loc) = self.var_locations.get(var) {
            return loc.clone();
        }

        let loc = self.fresh_mem_location(Some(var.1.clone()));
        self.var_locations.insert(var.clone(), loc.clone());
        loc
    }
}

impl Deref for GclGraph {
    type Target = StableDiGraph<GclNode, GclExpr>;

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
    RemoveFact(GclFact),
    /// Represents a bug in the program, ex. if an assert fails
    Bug,
}

impl Display for GclCommand {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            GclCommand::Assignment(assignment) => Display::fmt(assignment, f),
            GclCommand::AddFact(fact) => write!(f, "add_fact({})", fact),
            GclCommand::RemoveFact(fact) => write!(f, "remove_fact({})", fact),
            GclCommand::Bug => f.write_str("bug"),
        }
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
            GclLValue::Var(loc) => loc.clone(),
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

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum MemoryLocation {
    ReturnVal,
    Var(usize, Option<String>),
}

impl Display for MemoryLocation {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            MemoryLocation::ReturnVal => f.write_str("loc_ret"),
            MemoryLocation::Var(id, None) => {
                write!(f, "loc_{}", id)
            }
            MemoryLocation::Var(id, Some(name)) => {
                write!(f, "loc_{}_{}", id, name)
            }
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct GclExpr {
    pub ty: IrType,
    pub data: GclExprData,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum GclExprData {
    Bool(bool),
    String(String),
    Fact(GclFact),
    Var(MemoryLocation),
    BinOp(GclBinOp, Box<GclExpr>, Box<GclExpr>),
    UnOp(GclUnOp, Box<GclExpr>),
    Struct { fields: Vec<(String, GclExpr)> },
    FieldAccess(Box<GclExpr>, String),
}

impl Default for GclExpr {
    /// An always true GCL predicate
    fn default() -> Self {
        GclExpr::bool(true)
    }
}

impl GclExpr {
    pub fn bool(b: bool) -> Self {
        GclExpr {
            ty: IrType::bool(),
            data: GclExprData::Bool(b),
        }
    }

    pub fn string(s: String) -> Self {
        GclExpr {
            ty: IrType::string(),
            data: GclExprData::String(s),
        }
    }

    pub fn fact(fact: GclFact) -> Self {
        GclExpr {
            ty: IrType::bool(),
            data: GclExprData::Fact(fact),
        }
    }

    pub fn var(loc: MemoryLocation, ty: IrType) -> Self {
        GclExpr {
            ty,
            data: GclExprData::Var(loc),
        }
    }

    pub fn bin_op(op: GclBinOp, left: GclExpr, right: GclExpr) -> Self {
        GclExpr {
            ty: op.ty(),
            data: GclExprData::BinOp(op, Box::new(left), Box::new(right)),
        }
    }

    pub fn negate(&self) -> Self {
        GclExpr {
            ty: IrType::bool(),
            data: GclExprData::UnOp(GclUnOp::Negate, Box::new(self.clone())),
        }
    }

    /// Simplify this expression if possible. This is usually only useful for
    /// expressions which are used as predicates (ex. GCL edges).
    pub fn simplify(&mut self) {
        match &mut self.data {
            GclExprData::Bool(_) => {}
            GclExprData::String(_) => {}
            GclExprData::Fact(_) => {}
            GclExprData::Var(_) => {}
            GclExprData::BinOp(op, left, right) => {
                left.simplify();
                right.simplify();

                match op {
                    GclBinOp::And => match (&left.data, &right.data) {
                        (GclExprData::Bool(false), _) | (_, GclExprData::Bool(false)) => {
                            self.data = GclExprData::Bool(false);
                        }
                        (GclExprData::Bool(true), _) => {
                            *self = (**right).clone();
                        }
                        (_, GclExprData::Bool(true)) => {
                            *self = (**left).clone();
                        }
                        _ => {}
                    },
                    GclBinOp::Or => match (&left.data, &right.data) {
                        (GclExprData::Bool(true), _) | (_, GclExprData::Bool(true)) => {
                            self.data = GclExprData::Bool(true);
                        }
                        (GclExprData::Bool(false), _) => {
                            *self = (**right).clone();
                        }
                        (_, GclExprData::Bool(false)) => {
                            *self = (**left).clone();
                        }
                        _ => {}
                    },
                    GclBinOp::Equals => {
                        if left.data == right.data {
                            self.data = GclExprData::Bool(true);
                        }
                    }
                }
            }
            GclExprData::UnOp(op, inner) => {
                inner.simplify();

                match op {
                    GclUnOp::Negate => {
                        if let GclExprData::Bool(b) = &inner.data {
                            self.data = GclExprData::Bool(!*b);
                        }
                    }
                }
            }
            GclExprData::Struct { .. } => {}
            GclExprData::FieldAccess(_, _) => {}
        }
    }
}

impl Display for GclExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match &self.data {
            GclExprData::Bool(b) => Display::fmt(b, f),
            GclExprData::String(s) => Debug::fmt(s, f),
            GclExprData::Fact(fact) => Display::fmt(fact, f),
            GclExprData::Var(loc) => Display::fmt(loc, f),
            GclExprData::BinOp(op, left, right) => {
                write!(f, "({}) {} ({})", left, op, right)
            }
            GclExprData::UnOp(op, inner) => {
                write!(f, "{}({})", op, inner)
            }
            GclExprData::Struct { fields } => {
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
            GclExprData::FieldAccess(loc, field) => {
                write!(f, "{}.{}", loc, field)
            }
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum GclBinOp {
    And,
    Or,
    Equals,
}

impl Display for GclBinOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            GclBinOp::And => f.write_str("&&"),
            GclBinOp::Or => f.write_str("||"),
            GclBinOp::Equals => f.write_str("=="),
        }
    }
}

impl GclBinOp {
    pub fn ty(&self) -> IrType {
        match self {
            GclBinOp::And | GclBinOp::Or | GclBinOp::Equals => IrType::bool(),
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum GclUnOp {
    Negate,
}

impl Display for GclUnOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            GclUnOp::Negate => f.write_char('!'),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum GclFact {
    HasValue(MemoryLocation),
}

impl GclFact {
    /// Get the variable name to use for this fact in Z3
    pub fn z3_name(&self) -> String {
        match self {
            GclFact::HasValue(loc) => {
                format!("has_value__{}", loc)
            }
        }
    }
}

impl Display for GclFact {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            GclFact::HasValue(loc) => {
                write!(f, "HasValue({})", loc)
            }
        }
    }
}
