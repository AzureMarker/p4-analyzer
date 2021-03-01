use crate::gcl::{GclAssignment, GclCommand, GclGraph, GclPredicate};
use petgraph::algo::toposort;
use petgraph::graph::NodeIndex;
use petgraph::visit::EdgeRef;
use petgraph::Direction;
use std::collections::{HashMap, HashSet};
use std::iter::FromIterator;
use std::ops::Deref;

pub type WlpMap = HashMap<NodeIndex, GclPredicate>;
pub type VariableMap<'a> = HashMap<NodeIndex, HashMap<&'a str, HashSet<GclPredicate>>>;

impl GclGraph {
    pub fn to_wlp(&self) -> (WlpMap, VariableMap) {
        let sorted_nodes = toposort(self.deref(), None).expect("There should be no cycles");
        let mut node_wlp: WlpMap = HashMap::new();
        let mut node_variables: VariableMap = HashMap::new();

        // Iterate through the nodes in topological order (parents before children)
        for node_idx in sorted_nodes {
            let node = self.node_weight(node_idx).unwrap();

            // Gather all of the variables known to the parents
            let parent_variables: Vec<_> = self
                .edges_directed(node_idx, Direction::Incoming)
                .map(|edge| node_variables.get(&edge.source()).unwrap())
                .collect();
            let mut current_variables: HashMap<&str, HashSet<GclPredicate>> = HashMap::new();

            // Add all of the common variables among the parents into the current
            // node's variable cache.
            for variables in parent_variables.iter() {
                for (name, values) in *variables {
                    if parent_variables.iter().all(|map| map.contains_key(name)) {
                        current_variables
                            .entry(*name)
                            .or_default()
                            .extend(values.iter().cloned());
                    }
                }
            }

            // Consider this node's variable assignments
            for cmd in &node.commands {
                let (name, pred) = match cmd {
                    GclCommand::Assignment(GclAssignment { name, pred }) => (name, pred),
                    _ => continue,
                };
                let pred = pred.clone();
                let preds = pred.fill_in(&current_variables);

                current_variables.insert(name.as_str(), preds);
            }

            // Calculate the WLP for each incoming edge
            let edge_wlps = self
                .edges_directed(node_idx, Direction::Incoming)
                .map(|edge| {
                    let parent_idx = edge.source();
                    let parent_wlp = node_wlp.get(&parent_idx).unwrap();
                    let parent_vars = node_variables.get(&parent_idx).unwrap();
                    let edge_pred = edge
                        .weight()
                        .fill_in(parent_vars)
                        .into_iter()
                        .reduce(|acc, next| {
                            GclPredicate::Disjunction(Box::new(acc), Box::new(next))
                        })
                        .unwrap();

                    // The parent's predicate and this edge's predicate have to
                    // be true in order for the node to be reachable via this edge
                    GclPredicate::Conjunction(Box::new(parent_wlp.clone()), Box::new(edge_pred))
                });

            // Calculate this node's WLP by taking an OR of the edge WLPs
            let wlp = edge_wlps
                .reduce(|acc, next| GclPredicate::Disjunction(Box::new(acc), Box::new(next)))
                .unwrap_or_default();

            // Update the accumulated variables and WLPs
            node_variables.insert(node_idx, current_variables);
            node_wlp.insert(node_idx, wlp);
        }

        (node_wlp, node_variables)
    }
}

impl GclPredicate {
    /// Try to fill in this predicate's variables with the given values. Since
    /// variables may have multiple values (due to converging branches), this
    /// returns a vector of predicates.
    pub fn fill_in(&self, values: &HashMap<&str, HashSet<GclPredicate>>) -> HashSet<GclPredicate> {
        fn cross_product(
            left: &GclPredicate,
            right: &GclPredicate,
            values: &HashMap<&str, HashSet<GclPredicate>>,
            f: impl Fn(Box<GclPredicate>, Box<GclPredicate>) -> GclPredicate,
        ) -> HashSet<GclPredicate> {
            let left = left.fill_in(values);
            let right = right.fill_in(values);
            let mut results = HashSet::new();

            for l in left {
                for r in &right {
                    results.insert(f(Box::new(l.clone()), Box::new(r.clone())));
                }
            }

            results
        }

        match self {
            GclPredicate::Equality(left, right) => {
                cross_product(left, right, values, GclPredicate::Equality)
            }
            GclPredicate::Conjunction(left, right) => {
                cross_product(left, right, values, GclPredicate::Conjunction)
            }
            GclPredicate::Disjunction(left, right) => {
                cross_product(left, right, values, GclPredicate::Disjunction)
            }
            GclPredicate::Negation(inner) => inner
                .fill_in(values)
                .into_iter()
                .map(|pred| GclPredicate::Negation(Box::new(pred)))
                .collect(),
            GclPredicate::Bool(_) | GclPredicate::String(_) => {
                HashSet::from_iter(Some(self.clone()))
            }
            GclPredicate::Var(name) | GclPredicate::StringVar(name) => {
                if let Some(values) = values.get(name.as_str()) {
                    values.clone()
                } else {
                    HashSet::from_iter(Some(self.clone()))
                }
            }
        }
    }
}

// TODO: Replace usages with std's `reduce` when Rust 1.51 is released:
//       https://github.com/rust-lang/rust/issues/68125
trait ReduceIter: Iterator {
    fn reduce<F>(mut self, f: F) -> Option<Self::Item>
    where
        Self: Sized,
        F: FnMut(Self::Item, Self::Item) -> Self::Item,
    {
        let first = self.next()?;
        Some(self.fold(first, f))
    }
}

impl<I: Iterator> ReduceIter for I {}
