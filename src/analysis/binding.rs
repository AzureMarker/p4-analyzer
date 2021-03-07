//! Perform binding analysis (connect variables to declarations).

use crate::ast::{ConstantDecl, Declaration, Expr, Instantiation, Program, TypeRef};
use std::collections::HashMap;

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub struct VariableId(usize);

#[derive(Debug)]
pub enum BindingError {
    /// The declaration of this variable was not found
    UnknownVar(String),
}

pub fn perform_binding_analysis(
    program: &Program,
) -> Result<(Program, HashMap<String, VariableId>), BindingError> {
    let mut scope = VariableStack::new();
    let new_program = program.binding_analysis(&mut scope)?;

    Ok((new_program, scope.name_to_id))
}

/// Represents the variable names that are in scope. Each scope level has its
/// own map in the stack. The maps convert in-program variable names into
/// variable IDs.
#[derive(Default)]
struct VariableStack {
    stack: Vec<HashMap<String, VariableId>>,
    name_to_id: HashMap<String, VariableId>,
    id_to_name: HashMap<VariableId, String>,
    next_id: usize,
}

impl VariableStack {
    fn new() -> Self {
        Self::default()
    }

    fn get(&self, name: &str) -> Option<VariableId> {
        for map in self.stack.iter().rev() {
            if let Some(var) = map.get(name) {
                return Some(*var);
            }
        }

        None
    }

    fn get_name(&self, id: VariableId) -> Option<&str> {
        self.id_to_name.get(&id).map(String::as_str)
    }

    fn get_new_name(&self, old_name: &str) -> Option<&str> {
        let id = self.get(old_name)?;
        self.get_name(id)
    }

    fn contains(&self, name: &str) -> bool {
        self.get(name).is_some()
    }

    fn insert(&mut self, name: String) -> (String, VariableId) {
        if self.stack.is_empty() {
            self.stack.push(HashMap::new());
        }

        let id = VariableId(self.next_id);
        let new_name = format!("var_{}__{}", self.next_id, name);
        self.next_id += 1;
        self.stack.last_mut().unwrap().insert(name, id);
        self.name_to_id.insert(new_name.clone(), id);
        self.id_to_name.insert(id, new_name.clone());

        (new_name, id)
    }

    fn push_scope(&mut self) {
        self.stack.push(HashMap::new());
    }

    fn pop_scope(&mut self) {
        self.stack.pop();
    }
}

trait BindingAnalysis: Sized {
    fn binding_analysis(&self, scope: &mut VariableStack) -> Result<Self, BindingError>;
}

impl BindingAnalysis for Program {
    fn binding_analysis(&self, scope: &mut VariableStack) -> Result<Self, BindingError> {
        let mut declarations = Vec::new();

        for decl in &self.declarations {
            match decl {
                Declaration::Struct(_) => {
                    unimplemented!()
                }
                Declaration::Control(_) => {
                    unimplemented!()
                }
                Declaration::Constant(const_decl) => {
                    declarations.push(Declaration::Constant(const_decl.binding_analysis(scope)?));
                }
                Declaration::Instantiation(instantiation) => {
                    declarations.push(Declaration::Instantiation(
                        instantiation.binding_analysis(scope)?,
                    ));
                }
            }
        }

        Ok(Program { declarations })
    }
}

impl BindingAnalysis for ConstantDecl {
    fn binding_analysis(&self, scope: &mut VariableStack) -> Result<Self, BindingError> {
        let new_value = self.value.binding_analysis(scope)?;
        let (new_name, _) = scope.insert(self.name.clone());

        // todo: check type

        Ok(ConstantDecl {
            ty: self.ty.clone(),
            name: new_name,
            value: new_value,
        })
    }
}

impl BindingAnalysis for Instantiation {
    fn binding_analysis(&self, scope: &mut VariableStack) -> Result<Self, BindingError> {
        // todo: check type and args

        let (new_name, _) = scope.insert(self.name.clone());

        Ok(Instantiation {
            ty: self.ty.clone(),
            name: new_name,
            args: self.args.clone(),
        })
    }
}

impl BindingAnalysis for Expr {
    fn binding_analysis(&self, scope: &mut VariableStack) -> Result<Self, BindingError> {
        match self {
            Expr::Bool(_) => Ok(self.clone()),
            Expr::Var(name) => {
                let new_name = scope
                    .get_new_name(name)
                    .ok_or_else(|| BindingError::UnknownVar(name.clone()))?
                    .to_string();

                Ok(Expr::Var(new_name))
            }
            Expr::And(left, right) | Expr::Or(left, right) => {
                left.binding_analysis(scope)?;
                right.binding_analysis(scope)
            }
            Expr::Negation(inner) => inner.binding_analysis(scope),
            Expr::FunctionCall(_) => {
                todo!()
            }
        }
    }
}
