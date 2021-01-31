//! Guarded Command Language

use petgraph::graph::NodeIndex;
use petgraph::stable_graph::{StableDiGraph, StableGraph};
use petgraph::visit::EdgeRef;
use petgraph::Direction;
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

impl Display for GclGraph {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        writeln!(f, "Graph ({} nodes):", self.inner.node_count())?;

        let mut nodes: Vec<(&GclNode, Vec<String>)> = self
            .inner
            .node_indices()
            .map(|idx| {
                (
                    self.inner.node_weight(idx).unwrap(),
                    self.inner
                        .edges_directed(idx, Direction::Outgoing)
                        .map(|edge| {
                            format!(
                                "{} => {}",
                                edge.weight(),
                                self.inner.node_weight(edge.target()).unwrap().name
                            )
                        })
                        .collect(),
                )
            })
            .collect();
        nodes.sort_by_key(|(node, _)| node.name.as_str());

        for (node, edges) in nodes {
            writeln!(
                f,
                "Node '{}'\n  pre_condition = {}\n  command = {}\n  jump = {:?}",
                node.name, node.pre_condition, node.command, edges
            )?;
        }

        Ok(())
    }
}

#[derive(Debug)]
pub struct GclNode {
    pub pre_condition: GclPredicate,
    pub name: String,
    pub command: GclCommand,
}

impl Display for GclNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Node '{}'\nprecondition = {}\ncommand = {}",
            self.name, self.pre_condition, self.command
        )
    }
}

/// Represents a sub-graph of nodes who all have `start` as a parent and who
/// all eventually lead to `end` (or exit the program/error out).
#[derive(Copy, Clone)]
pub struct GclNodeRange {
    pub start: NodeIndex,
    pub end: NodeIndex,
}

#[derive(Clone, Debug)]
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
            GclCommand::Choice(cmd1, cmd2) => write!(f, "({}) [] ({})", cmd1, cmd2),
            GclCommand::Assumption(pred) => write!(f, "assume({})", pred),
            GclCommand::Assert(pred) => write!(f, "assert({})", pred),
        }
    }
}

impl Default for GclCommand {
    /// A "no operation" command in GCL
    fn default() -> Self {
        GclCommand::Assumption(GclPredicate::default())
    }
}

pub trait Flatten: IntoIterator {
    /// Convert from a stream of items into a single item representing the item
    /// stream. For example, used to convert `Vec<GclCommand>` into `GclCommand`
    /// by using `GclCommand::Sequence`.
    fn flatten(self) -> Self::Item;
}

impl Flatten for Vec<GclCommand> {
    fn flatten(self) -> Self::Item {
        let mut command_iter = self.into_iter().rev();
        let last_command = command_iter.next().unwrap_or_default();

        command_iter.fold(last_command, |acc, next| {
            GclCommand::Sequence(Box::new(next), Box::new(acc))
        })
    }
}

#[derive(Clone, Debug)]
pub enum GclPredicate {
    Equality(Box<GclPredicate>, Box<GclPredicate>),
    Conjunction(Box<GclPredicate>, Box<GclPredicate>),
    Disjunction(Box<GclPredicate>, Box<GclPredicate>),
    Implication(Box<GclPredicate>, Box<GclPredicate>),
    Negation(Box<GclPredicate>),
    Bool(bool),
    Var(String),
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
            GclPredicate::Var(name) => f.write_str(&name),
        }
    }
}

impl Default for GclPredicate {
    /// An always true GCL predicate
    fn default() -> Self {
        GclPredicate::Bool(true)
    }
}

impl Flatten for Vec<GclPredicate> {
    fn flatten(self) -> Self::Item {
        let mut pred_iter = self.into_iter().rev();
        let last_pred = pred_iter.next().unwrap_or_default();

        pred_iter.fold(last_pred, |acc, next| {
            GclPredicate::Conjunction(Box::new(next), Box::new(acc))
        })
    }
}

#[derive(Clone, Debug)]
pub struct GclAssignment {
    pub name: String,
    pub pred: GclPredicate,
}

impl Display for GclAssignment {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{} := {}", self.name, self.pred)
    }
}
