use crate::gcl::GclPredicate;
use z3::ast::{Ast, Bool, Dynamic};
use z3::Context;

impl GclPredicate {
    /// Convert the GCL predicate into a Z3 AST type
    pub fn as_z3_ast<'ctx>(&self, context: &'ctx Context) -> Dynamic<'ctx> {
        match self {
            GclPredicate::Bool(value) => Dynamic::from_ast(&Bool::from_bool(context, *value)),
            GclPredicate::String(value) => {
                Dynamic::from_ast(&z3::ast::String::from_str(context, value).unwrap())
            }
            GclPredicate::Var(name) => Dynamic::from_ast(&Bool::new_const(context, name.as_str())),
            GclPredicate::StringVar(name) => {
                Dynamic::from_ast(&z3::ast::String::new_const(context, name.as_str()))
            }
            GclPredicate::Negation(inner) => {
                Dynamic::from_ast(&inner.as_z3_ast(context).as_bool().unwrap().not())
            }
            GclPredicate::Equality(left, right) => {
                Dynamic::from_ast(&left.as_z3_ast(context)._eq(&right.as_z3_ast(context)))
            }
            GclPredicate::Conjunction(left, right) => Dynamic::from_ast(&Bool::and(
                context,
                &[
                    &left.as_z3_ast(context).as_bool().unwrap(),
                    &right.as_z3_ast(context).as_bool().unwrap(),
                ],
            )),
            GclPredicate::Disjunction(left, right) => Dynamic::from_ast(&Bool::or(
                context,
                &[
                    &left.as_z3_ast(context).as_bool().unwrap(),
                    &right.as_z3_ast(context).as_bool().unwrap(),
                ],
            )),
        }
    }
}
