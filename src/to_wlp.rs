use crate::gcl::{
    GclAssignment, GclBinOp, GclCommand, GclExpr, GclExprData, GclFact, GclGraph, GclLValue,
    MemoryLocation,
};
use petgraph::graph::NodeIndex;
use petgraph::visit::{EdgeRef, Topo};
use petgraph::Direction;
use std::collections::{HashMap, HashSet};
use std::iter::FromIterator;
use std::ops::Deref;

pub type WlpMap = HashMap<NodeIndex, GclExpr>;
pub type VariableMap = HashMap<NodeIndex, HashMap<MemoryLocation, HashSet<GclExpr>>>;
pub type FactSets = HashMap<NodeIndex, HashSet<GclFact>>;

impl GclGraph {
    pub fn to_wlp(&self) -> (WlpMap, VariableMap) {
        let mut topological_iter = Topo::new(self.deref());
        let mut node_wlp: WlpMap = HashMap::new();
        let mut node_variables: VariableMap = HashMap::new();
        let mut node_facts: FactSets = HashMap::new();

        // Iterate through the nodes in topological order (parents before children)
        while let Some(node_idx) = topological_iter.next(self.deref()) {
            let node = self.node_weight(node_idx).unwrap();

            // Gather all of the variables and facts known to the parents
            let parent_variables: Vec<_> = self
                .edges_directed(node_idx, Direction::Incoming)
                .map(|edge| node_variables.get(&edge.source()).unwrap())
                .collect();
            let parent_facts: Vec<_> = self
                .edges_directed(node_idx, Direction::Incoming)
                .map(|edge| node_facts.get(&edge.source()).unwrap())
                .collect();
            let mut current_variables: HashMap<MemoryLocation, HashSet<GclExpr>> = HashMap::new();

            // Add all of the common variables among the parents into the current
            // node's variable cache.
            for variables in &parent_variables {
                for (loc, values) in *variables {
                    if parent_variables.iter().all(|map| map.contains_key(loc)) {
                        current_variables
                            .entry(*loc)
                            .or_default()
                            .extend(values.iter().cloned());
                    }
                }
            }

            // TODO: add some notion of uncertainty if a fact is not shared by all parents?
            // Add all of the common facts among the parents into the current
            // node's facts
            let mut current_facts = parent_facts
                .into_iter()
                .cloned()
                .reduce(|acc, next| acc.intersection(&next).cloned().collect())
                .unwrap_or_default();

            // Consider this node's variable assignments
            for cmd in &node.commands {
                let (lvalue, expr) = match cmd {
                    GclCommand::Assignment(GclAssignment { lvalue, expr }) => (lvalue, expr),
                    GclCommand::AddFact(fact) => {
                        current_facts.insert(fact.clone());
                        continue;
                    }
                    GclCommand::RemoveFact(fact) => {
                        current_facts.remove(fact);
                        continue;
                    }
                    GclCommand::Bug => continue,
                };

                let loc = lvalue.mem_location();
                let values = expr.fill_in(&current_variables, &current_facts);
                let new_exprs = values
                    .into_iter()
                    .map(|value| expr.update_by_lvalue(lvalue, value))
                    .collect();

                current_variables.insert(loc, new_exprs);
            }

            // Calculate the WLP for each incoming edge
            let edge_wlps = self
                .edges_directed(node_idx, Direction::Incoming)
                .map(|edge| {
                    let parent_idx = edge.source();
                    let parent_wlp = node_wlp.get(&parent_idx).unwrap();
                    let parent_vars = node_variables.get(&parent_idx).unwrap();
                    let parent_facts = node_facts.get(&parent_idx).unwrap();
                    let edge_pred = edge
                        .weight()
                        .fill_in(parent_vars, parent_facts)
                        .into_iter()
                        .reduce(|acc, next| GclExpr::bin_op(GclBinOp::Or, acc, next))
                        .unwrap();

                    // The parent's predicate and this edge's predicate have to
                    // be true in order for the node to be reachable via this edge
                    GclExpr::bin_op(GclBinOp::And, parent_wlp.clone(), edge_pred)
                });

            // Calculate this node's WLP by taking an OR of the edge WLPs
            let wlp = edge_wlps
                .reduce(|acc, next| GclExpr::bin_op(GclBinOp::Or, acc, next))
                .unwrap_or_default();

            // Update the accumulated variables and WLPs
            node_variables.insert(node_idx, current_variables);
            node_wlp.insert(node_idx, wlp);
            node_facts.insert(node_idx, current_facts);
        }

        (node_wlp, node_variables)
    }
}

impl GclExpr {
    fn update_by_lvalue(&self, lvalue: &GclLValue, value: GclExpr) -> GclExpr {
        let mut expr = self.clone();
        let mut current = &mut expr;

        for entry in lvalue.entries() {
            match entry {
                GclLValueEntry::Var(_) => {
                    *current = value;
                    break;
                }
                GclLValueEntry::Field(field) => match &mut current.data {
                    GclExprData::Struct { fields } => {
                        current = fields
                            .iter_mut()
                            .find_map(|(name, expr)| if *name == field { Some(expr) } else { None })
                            .expect("Tried to access an unknown struct field. This should have been caught during type checking");
                    }
                    _ => {
                        panic!("Tried to use a field lvalue on a non-header/-struct type. This should have been caught during type checking")
                    }
                },
            }
        }

        expr
    }
}

impl GclExpr {
    /// Try to fill in this expression's variables with the given values. Since
    /// variables may have multiple values (due to converging branches), this
    /// returns a number of expressions.
    pub fn fill_in(
        &self,
        values: &HashMap<MemoryLocation, HashSet<GclExpr>>,
        facts: &HashSet<GclFact>,
    ) -> HashSet<GclExpr> {
        match &self.data {
            GclExprData::BinOp(op, left, right) => {
                let left = left.fill_in(values, facts);
                let right = right.fill_in(values, facts);
                let mut results = HashSet::new();

                for l in left {
                    for r in &right {
                        results.insert(GclExpr::bin_op(*op, l.clone(), r.clone()));
                    }
                }

                results
            }
            GclExprData::UnOp(op, inner) => inner
                .fill_in(values, facts)
                .into_iter()
                .map(|pred| GclExpr {
                    ty: self.ty.clone(),
                    data: GclExprData::UnOp(*op, Box::new(pred)),
                })
                .collect(),
            GclExprData::Bool(_) | GclExprData::String(_) => HashSet::from_iter(Some(self.clone())),
            GclExprData::Fact(fact) => {
                // TODO: this loses info about facts, should we keep facts around
                //       to improve diagnostics in Z3 later?
                HashSet::from_iter(Some(GclExpr::bool(facts.contains(fact))))
            }
            GclExprData::Var(loc) => {
                if let Some(values) = values.get(loc) {
                    values.clone()
                } else {
                    HashSet::from_iter(Some(self.clone()))
                }
            }
            GclExprData::Struct { fields } => {
                let mut structs: Vec<Vec<(String, GclExpr)>> = Vec::new();

                let fields: Vec<_> = fields
                    .iter()
                    .map(|(name, field)| (name, field.fill_in(values, facts)))
                    .collect();

                for (name, values) in fields {
                    let mut struct_classes = structs.repeat_vec(values.len());

                    for (struct_class, value) in struct_classes.iter_mut().zip(values) {
                        for s in struct_class {
                            s.push((name.clone(), value.clone()));
                        }
                    }

                    structs = struct_classes.into_iter().flatten().collect();
                }

                structs
                    .into_iter()
                    .map(|fields| GclExpr {
                        ty: self.ty.clone(),
                        data: GclExprData::Struct { fields },
                    })
                    .collect()
            }
            GclExprData::FieldAccess(loc, field) => {
                if let Some(values) = values.get(loc) {
                    values
                        .iter()
                        .map(|value| {
                            let fields = if let GclExprData::Struct { fields } = &value.data {
                                fields
                            } else {
                                panic!("value in field access is not a struct. Type checking should have caught this.");
                            };

                            // Return the field's value
                            fields
                                .iter()
                                .find_map(
                                    |(name, value)| {
                                        if name == field {
                                            Some(value.clone())
                                        } else {
                                            None
                                        }
                                    },
                                )
                                .expect("field should exist (thanks to type checking)")
                        })
                        .collect()
                } else {
                    HashSet::from_iter(Some(self.clone()))
                }
            }
        }
    }
}

enum GclLValueEntry {
    Var(MemoryLocation),
    Field(String),
}

impl GclLValue {
    fn entries(&self) -> Vec<GclLValueEntry> {
        let mut entries = Vec::new();
        let mut current = self;

        loop {
            match current {
                GclLValue::Var(loc) => {
                    entries.push(GclLValueEntry::Var(*loc));
                    break;
                }
                GclLValue::Field(target, field) => {
                    entries.push(GclLValueEntry::Field(field.clone()));
                    current = target;
                }
            }
        }

        entries.reverse();
        entries
    }
}

trait RepeatVec: Sized {
    fn repeat_vec(self, times: usize) -> Vec<Self>;
}

impl<T: Clone> RepeatVec for Vec<T> {
    fn repeat_vec(self, times: usize) -> Vec<Self> {
        if times == 0 {
            return Vec::new();
        }

        let mut result = Vec::new();

        for _ in 1..times {
            result.push(self.clone());
        }

        result.push(self);
        result
    }
}
