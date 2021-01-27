//! Guarded Command Language

use std::collections::HashMap;
use std::fmt;
use std::fmt::{Debug, Display, Formatter};

pub struct GclGraph {
    pub nodes: HashMap<String, GclNode>,
    next_id_counter: usize,
}

impl GclGraph {
    pub fn new() -> Self {
        GclGraph {
            nodes: HashMap::new(),
            next_id_counter: 0,
        }
    }

    /// Create a new unique name, given the provided prefix.
    pub fn create_name(&mut self, prefix: &str) -> String {
        let name = format!("{}_{}", prefix, self.next_id_counter);
        self.next_id_counter += 1;
        name
    }

    /// Add a command to the end of the node to jump directly to the given
    /// node. This only works if the node does not already have a jump command.
    pub fn set_node_jump(&mut self, node_name: &str, new_jump: String) {
        let node = self.nodes.get_mut(node_name).unwrap();
        node.command = GclCommand::Sequence(
            Box::new(node.command.clone()),
            Box::new(GclCommand::Jump(GclJump::Direct {
                next_node: new_jump,
            })),
        );
    }
}

impl Display for GclGraph {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        writeln!(f, "Graph ({} nodes):", self.nodes.len())?;

        let mut nodes: Vec<_> = self.nodes.iter().collect();
        nodes.sort_by_key(|(name, _)| name.as_str());
        for (name, node) in nodes {
            writeln!(
                f,
                "Node '{}'\n  pre_condition = {}\n  command = {}",
                name, node.pre_condition, node.command
            )?;
        }

        Ok(())
    }
}

#[derive(Debug)]
pub struct GclNode {
    pub pre_condition: GclPredicate,
    pub command: GclCommand,
}

// #[derive(Clone, Debug)]
// pub struct GclJump {
//     pub kind: GclJumpKind,
// }
//
// impl Display for GclJump {
//     fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
//         write!(f, "{}", self.kind)
//     }
// }

#[derive(Clone, Debug)]
pub enum GclJump {
    /// Push a node onto the stack and jump to the next node
    Push {
        return_node: String,
        next_node: String,
    },
    /// Pop a node off of the stack and jump to it
    Pop,
    /// Jump directly to a node
    Direct { next_node: String },
    /// Stop execution (only used in the end node)
    End,
}

impl Display for GclJump {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            GclJump::Push {
                return_node,
                next_node,
            } => {
                write!(f, "push({}, {})", return_node, next_node)
            }
            GclJump::Pop => write!(f, "pop()"),
            GclJump::Direct { next_node } => write!(f, "direct({})", next_node),
            GclJump::End => write!(f, "end()"),
        }
    }
}

#[derive(Clone, Debug)]
pub enum GclCommand {
    Assignment(GclAssignment),
    Sequence(Box<GclCommand>, Box<GclCommand>),
    Choice(Box<GclCommand>, Box<GclCommand>),
    Assumption(GclPredicate),
    Assert(GclPredicate),
    Jump(GclJump),
}

impl Display for GclCommand {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            GclCommand::Assignment(assignment) => Display::fmt(assignment, f),
            GclCommand::Sequence(cmd1, cmd2) => write!(f, "{}; {}", cmd1, cmd2),
            GclCommand::Choice(cmd1, cmd2) => write!(f, "({}) [] ({})", cmd1, cmd2),
            GclCommand::Assumption(pred) => write!(f, "assume({})", pred),
            GclCommand::Assert(pred) => write!(f, "assert({})", pred),
            GclCommand::Jump(jump) => write!(f, "jump({})", jump),
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
