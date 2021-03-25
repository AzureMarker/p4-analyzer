use crate::gcl::{GclBinOp, GclExpr, GclExprData, GclUnOp};
use crate::ir::{IrBaseType, IrType};
use z3::ast::{Ast, Bool, Dynamic, String as Z3String};
use z3::Context;

impl GclExpr {
    /// Convert the GCL predicate into a Z3 AST type
    pub fn as_z3_ast<'ctx>(&self, context: &'ctx Context) -> Dynamic<'ctx> {
        match &self.data {
            GclExprData::Bool(value) => Bool::from_bool(context, *value).into(),
            GclExprData::String(value) => z3::ast::String::from_str(context, value).unwrap().into(),
            GclExprData::Fact(fact) => Bool::new_const(context, fact.z3_name()).into(),
            GclExprData::Var(name) => match &self.ty {
                IrType::Base(IrBaseType::Bool) => Bool::new_const(context, name.to_string()).into(),
                IrType::Base(IrBaseType::String) => {
                    Z3String::new_const(context, name.to_string()).into()
                }
                IrType::Base(IrBaseType::Struct { fields }) => todo!(),
                IrType::Base(_) => unimplemented!(),
                _ => unimplemented!(),
            },
            GclExprData::UnOp(GclUnOp::Negate, inner) => {
                Dynamic::from(inner.as_z3_ast(context).as_bool().unwrap().not())
            }
            GclExprData::BinOp(op, left, right) => match op {
                GclBinOp::And => Bool::and(
                    context,
                    &[
                        &left.as_z3_ast(context).as_bool().unwrap(),
                        &right.as_z3_ast(context).as_bool().unwrap(),
                    ],
                )
                .into(),
                GclBinOp::Or => Bool::or(
                    context,
                    &[
                        &left.as_z3_ast(context).as_bool().unwrap(),
                        &right.as_z3_ast(context).as_bool().unwrap(),
                    ],
                )
                .into(),
                GclBinOp::Equals => left
                    .as_z3_ast(context)
                    ._eq(&right.as_z3_ast(context))
                    .into(),
            },
            GclExprData::Struct { fields } => {
                todo!()
            }
        }
    }
}
