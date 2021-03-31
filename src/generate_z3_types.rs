use crate::ir::{IrBaseType, IrType};
use std::collections::{HashMap, HashSet};
use z3::{Context, DatatypeAccessor, DatatypeBuilder, DatatypeSort, Sort, Symbol};

pub type Z3TypeMap<'ctx> = HashMap<IrType, DatatypeSort<'ctx>>;

/// Translate the user-defined IR types (ex. structs) into Z3 types
pub fn generate_types<'ctx>(types: HashSet<&IrType>, context: &'ctx Context) -> Z3TypeMap<'ctx> {
    let mut next_id = 0;
    let mut z3_types: Z3TypeMap = HashMap::new();

    for ty in types {
        match ty {
            IrType::Base(IrBaseType::Struct { fields }) => {
                let name = format!("struct_{}", next_id);
                next_id += 1;
                let builder = DatatypeBuilder::new(context, name.as_str());

                let z3_fields = fields
                    .iter()
                    .map(|(field_ty, field_name)| {
                        let datatype_accessor = match field_ty {
                            IrBaseType::Bool => DatatypeAccessor::Sort(Sort::bool(context)),
                            IrBaseType::Int => DatatypeAccessor::Sort(Sort::int(context)),
                            IrBaseType::Bit { .. } => {
                                todo!()
                            }
                            IrBaseType::String => DatatypeAccessor::Sort(Sort::string(context)),
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

                                DatatypeAccessor::Datatype(Symbol::String(z3_ty.sort.to_string()))
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

                z3_types.insert(ty.clone(), builder.variant(&name, z3_fields).finish());
            }
            IrType::Base(_) => {}
            IrType::Table => {}
            IrType::Function(_) => {}
            IrType::Constructor(_) => {}
        }
    }

    z3_types
}
