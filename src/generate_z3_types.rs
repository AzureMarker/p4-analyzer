use crate::ir::{IrBaseType, IrType};
use std::collections::HashMap;
use z3::{Context, DatatypeAccessor, DatatypeBuilder, DatatypeSort, Sort};

pub type Z3TypeMap<'ctx> = HashMap<IrType, DatatypeSort<'ctx>>;

/// Translate the user-defined IR types (ex. structs) into Z3 types
pub fn generate_types<'ctx>(types: &[(String, IrType)], context: &'ctx Context) -> Z3TypeMap<'ctx> {
    let mut next_id = 0;
    let mut z3_types: Z3TypeMap = HashMap::new();

    // FIXME: Maybe this dance can be avoided, context:
    //        https://github.com/prove-rs/z3.rs/pull/137
    let bool_sort = Sort::bool(context);
    let int_sort = Sort::int(context);
    let string_sort = Sort::string(context);

    for (ty_name, ty) in types {
        match ty {
            IrType::Base(IrBaseType::Struct { fields }) => {
                let name = format!("struct_{}__{}", next_id, ty_name);
                next_id += 1;
                let builder = DatatypeBuilder::new(context, name.as_str());

                let z3_fields = fields
                    .iter()
                    .map(|(field_ty, field_name)| {
                        let datatype_accessor = match field_ty {
                            IrBaseType::Bool => DatatypeAccessor::Sort(&bool_sort),
                            IrBaseType::Int => DatatypeAccessor::Sort(&int_sort),
                            IrBaseType::Bit { .. } => {
                                todo!()
                            }
                            IrBaseType::String => DatatypeAccessor::Sort(&string_sort),
                            IrBaseType::Error => {
                                unimplemented!()
                            }
                            IrBaseType::MatchKind => {
                                unimplemented!()
                            }
                            IrBaseType::Enum { .. } => {
                                todo!()
                            }
                            IrBaseType::Struct { .. } => {
                                let z3_ty = z3_types
                                    .get(&IrType::Base(field_ty.clone()))
                                    .expect("Use of type before it was declared");

                                DatatypeAccessor::Sort(&z3_ty.sort)
                            }
                            IrBaseType::Header { .. } => {
                                todo!()
                            }
                            IrBaseType::TyVar(_) => {
                                unimplemented!()
                            }
                        };

                        (field_name.as_str(), datatype_accessor)
                    })
                    .collect();

                let new_datatype = builder.variant(&name, z3_fields).finish();
                z3_types.insert(ty.clone(), new_datatype);
            }
            IrType::Base(_) => {}
            IrType::Table => {}
            IrType::Function(_) => {}
            IrType::Constructor(_) => {}
        }
    }

    z3_types
}
