use crate::gcl::GclPredicate;
use z3::ast::{Ast, Bool};
use z3::Context;

impl GclPredicate {
    /// Convert the GCL predicate into a Z3 Bool
    pub fn as_z3_bool<'ctx>(&self, context: &'ctx Context) -> Bool<'ctx> {
        match self {
            GclPredicate::Bool(value) => Bool::from_bool(context, *value),
            GclPredicate::Var(name) => Bool::new_const(context, name.as_str()),
            GclPredicate::Negation(inner) => inner.as_z3_bool(context).not(),
            GclPredicate::Equality(left, right) => {
                left.as_z3_bool(context)._eq(&right.as_z3_bool(context))
            }
            GclPredicate::Implication(left, right) => {
                left.as_z3_bool(context).implies(&right.as_z3_bool(context))
            }
            GclPredicate::Conjunction(left, right) => Bool::and(
                context,
                &[&left.as_z3_bool(context), &right.as_z3_bool(context)],
            ),
            GclPredicate::Disjunction(left, right) => Bool::or(
                context,
                &[&left.as_z3_bool(context), &right.as_z3_bool(context)],
            ),
        }
    }
}
