use crate::gcl::{GclAssignment, GclCommand, GclPredicate};

impl GclCommand {
    /// Compute the wlp (weakest liberal preconditions) of a GCL command. This is a
    /// predicate which can be checked for satisfiability to ensure the command is
    /// valid.
    pub fn to_wlp(&self, post_condition: GclPredicate) -> GclPredicate {
        match self {
            GclCommand::Assignment(GclAssignment { name, pred }) => {
                post_condition.replace_var(name, pred)
            }
            GclCommand::Sequence(c1, c2) => c1.to_wlp(c2.to_wlp(post_condition)),
            GclCommand::Choice(c1, c2) => GclPredicate::Conjunction(
                Box::new(c1.to_wlp(post_condition.clone())),
                Box::new(c2.to_wlp(post_condition)),
            ),
            GclCommand::Assumption(pred) => {
                GclPredicate::Implication(Box::new(pred.clone()), Box::new(post_condition))
            }
            GclCommand::Assert(pred) => {
                GclPredicate::Conjunction(Box::new(pred.clone()), Box::new(post_condition))
            }
        }
    }
}

impl GclPredicate {
    /// Replace a variable in the predicate with another predicate
    pub fn replace_var(&self, var: &str, pred: &GclPredicate) -> GclPredicate {
        match self {
            GclPredicate::Equality(left, right) => GclPredicate::Equality(
                Box::new(left.replace_var(var, pred)),
                Box::new(right.replace_var(var, pred)),
            ),
            GclPredicate::Conjunction(left, right) => GclPredicate::Conjunction(
                Box::new(left.replace_var(var, pred)),
                Box::new(right.replace_var(var, pred)),
            ),
            GclPredicate::Disjunction(left, right) => GclPredicate::Disjunction(
                Box::new(left.replace_var(var, pred)),
                Box::new(right.replace_var(var, pred)),
            ),
            GclPredicate::Implication(left, right) => GclPredicate::Implication(
                Box::new(left.replace_var(var, pred)),
                Box::new(right.replace_var(var, pred)),
            ),
            GclPredicate::Negation(inner) => {
                GclPredicate::Negation(Box::new(inner.replace_var(var, pred)))
            }
            GclPredicate::Bool(_) => self.clone(),
            GclPredicate::Var(name) => {
                if name == var {
                    pred.clone()
                } else {
                    self.clone()
                }
            }
        }
    }
}
