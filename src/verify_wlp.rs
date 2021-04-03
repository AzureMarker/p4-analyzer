use crate::gcl::{GclBinOp, GclExpr, GclExprData, GclUnOp};
use crate::generate_z3_types::Z3TypeMap;
use crate::ir::{IrBaseType, IrType};
use z3::ast::{Ast, Bool, Datatype, Dynamic, String as Z3String};
use z3::Context;

impl GclExpr {
    /// Convert the GCL predicate into a Z3 AST type
    pub fn as_z3_ast<'ctx>(
        &self,
        context: &'ctx Context,
        type_map: &Z3TypeMap<'ctx>,
    ) -> Dynamic<'ctx> {
        match &self.data {
            GclExprData::Bool(value) => Bool::from_bool(context, *value).into(),
            GclExprData::String(value) => z3::ast::String::from_str(context, value).unwrap().into(),
            GclExprData::Fact(fact) => Bool::new_const(context, fact.z3_name()).into(),
            GclExprData::Var(name) => match &self.ty {
                IrType::Base(IrBaseType::Bool) => Bool::new_const(context, name.to_string()).into(),
                IrType::Base(IrBaseType::String) => {
                    Z3String::new_const(context, name.to_string()).into()
                }
                IrType::Base(IrBaseType::Struct { .. }) => {
                    let z3_type = type_map.get(&self.ty).unwrap();
                    Datatype::new_const(context, name.to_string(), &z3_type.sort).into()
                }
                IrType::Base(_) => unimplemented!(),
                _ => unimplemented!(),
            },
            GclExprData::UnOp(GclUnOp::Negate, inner) => {
                Dynamic::from(inner.as_z3_ast(context, type_map).as_bool().unwrap().not())
            }
            GclExprData::BinOp(op, left, right) => match op {
                GclBinOp::And => Bool::and(
                    context,
                    &[
                        &left.as_z3_ast(context, type_map).as_bool().unwrap(),
                        &right.as_z3_ast(context, type_map).as_bool().unwrap(),
                    ],
                )
                .into(),
                GclBinOp::Or => Bool::or(
                    context,
                    &[
                        &left.as_z3_ast(context, type_map).as_bool().unwrap(),
                        &right.as_z3_ast(context, type_map).as_bool().unwrap(),
                    ],
                )
                .into(),
                GclBinOp::Equals => left
                    .as_z3_ast(context, type_map)
                    ._eq(&right.as_z3_ast(context, type_map))
                    .into(),
            },
            GclExprData::Struct { fields } => {
                let z3_type = type_map.get(&self.ty).unwrap();
                let fields_z3: Vec<_> = fields
                    .iter()
                    .map(|(_, field)| field.as_z3_ast(context, type_map))
                    .collect();
                let fields_z3_ref: Vec<_> = fields_z3.iter().collect();

                z3_type.variants[0].constructor.apply(&fields_z3_ref)
            }
            GclExprData::FieldAccess(target, field) => {
                let target_z3 = target.as_z3_ast(context, type_map);
                let z3_type = type_map.get(&target.ty).unwrap();
                let field_idx = match &target.ty {
                    IrType::Base(IrBaseType::Struct { fields }) => {
                        fields.iter().position(|(_, name)| name == field).unwrap()
                    }
                    IrType::Base(IrBaseType::Header { .. }) => todo!(),
                    _ => panic!(
                        "Type error in Z3 gen: cannot access field of ty: {:?}",
                        target.ty
                    ),
                };

                z3_type.variants[0].accessors[field_idx].apply(&[&target_z3])
            }
        }
    }
}
