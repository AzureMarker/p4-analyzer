use crate::gcl::{GclAssignment, GclCommand, GclGraph, GclPredicate};
use petgraph::algo::toposort;
use petgraph::graph::NodeIndex;
use petgraph::visit::EdgeRef;
use petgraph::Direction;
use std::collections::HashMap;
use std::ops::Deref;

impl GclGraph {
    pub fn to_wlp(&self) -> HashMap<NodeIndex, GclPredicate> {
        let sorted_nodes = toposort(self.deref(), None).expect("There should be no cycles");
        let mut node_wlp: HashMap<NodeIndex, GclPredicate> = HashMap::new();
        let mut node_variables: HashMap<NodeIndex, HashMap<&str, &GclPredicate>> = HashMap::new();

        for node_idx in sorted_nodes {
            let node = self.node_weight(node_idx).unwrap();

            let parent_assignments: Vec<_> = self
                .edges_directed(node_idx, Direction::Incoming)
                .map(|edge| node_variables.get(&edge.source()).unwrap())
                .collect();
            let mut common_assignments = HashMap::new();

            for assignments in parent_assignments.iter() {
                for (name, value) in *assignments {
                    if parent_assignments.iter().all(|map| map.contains_key(name)) {
                        common_assignments.insert(*name, *value);
                    }
                }
            }

            let assignments = node
                .command
                .get_assignments()
                .map(|GclAssignment { name, pred }| (name.as_str(), pred));
            common_assignments.extend(assignments);

            let mut parent_wlps = self
                .edges_directed(node_idx, Direction::Incoming)
                .map(|edge| {
                    let parent_idx = edge.source();
                    let parent_wlp = node_wlp.get(&parent_idx).unwrap();
                    let parent_vars = node_variables.get(&parent_idx).unwrap();
                    let mut edge_pred = edge.weight().clone();

                    for var in edge_pred.find_all_vars() {
                        if let Some(value) = parent_vars.get(var.as_str()) {
                            edge_pred.replace_var(&var, *value);
                        }
                    }

                    GclPredicate::Conjunction(Box::new(parent_wlp.clone()), Box::new(edge_pred))
                });
            let mut precondition = node.pre_condition.clone();

            for var in precondition.find_all_vars() {
                if let Some(value) = common_assignments.get(var.as_str()) {
                    precondition.replace_var(&var, *value);
                }
            }

            let wlp = if let Some(first) = parent_wlps.next() {
                let parent_wlp = parent_wlps.fold(first, |acc, next| {
                    GclPredicate::Disjunction(Box::new(acc), Box::new(next))
                });

                GclPredicate::Conjunction(Box::new(parent_wlp), Box::new(precondition))
            } else {
                precondition
            };

            node_variables.insert(node_idx, common_assignments);
            node_wlp.insert(node_idx, wlp);
        }

        node_wlp
    }
}

impl GclPredicate {
    /// Replace a variable in the predicate with another predicate in-place
    pub fn replace_var(&mut self, var: &str, value: &GclPredicate) {
        match self {
            GclPredicate::Equality(left, right)
            | GclPredicate::Conjunction(left, right)
            | GclPredicate::Disjunction(left, right)
            | GclPredicate::Implication(left, right) => {
                left.replace_var(var, value);
                right.replace_var(var, value);
            }
            GclPredicate::Negation(inner) => inner.replace_var(var, value),
            GclPredicate::Bool(_) => {}
            GclPredicate::Var(name) => {
                if name == var {
                    *self = value.clone();
                }
            }
        }
    }
}
