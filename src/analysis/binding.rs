//! Perform binding analysis (connect variables to declarations).

use crate::ast::{
    ActionDecl, Argument, Assignment, BlockStatement, ConstantDecl, ControlDecl, ControlLocalDecl,
    Declaration, Expr, FunctionCall, IfStatement, Instantiation, KeyElement, Param, Program,
    Statement, StatementOrDecl, TableDecl, TableProperty, VariableDecl,
};
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

    fn get_new_name_or_err(&self, old_name: &str) -> Result<String, BindingError> {
        self.get_new_name(old_name)
            .map(str::to_string)
            .ok_or_else(|| BindingError::UnknownVar(old_name.to_string()))
    }

    fn insert(&mut self, name: String) -> String {
        if self.stack.is_empty() {
            self.stack.push(HashMap::new());
        }

        let id = VariableId(self.next_id);
        let new_name = format!("var_{}__{}", self.next_id, name);
        self.next_id += 1;
        self.stack.last_mut().unwrap().insert(name, id);
        self.name_to_id.insert(new_name.clone(), id);
        self.id_to_name.insert(id, new_name.clone());

        new_name
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

impl<T: BindingAnalysis> BindingAnalysis for Vec<T> {
    fn binding_analysis(&self, scope: &mut VariableStack) -> Result<Self, BindingError> {
        let items = self
            .iter()
            .map(|item| item.binding_analysis(scope))
            .collect::<Result<_, _>>()?;

        Ok(items)
    }
}

impl<T: BindingAnalysis> BindingAnalysis for Option<T> {
    fn binding_analysis(&self, scope: &mut VariableStack) -> Result<Self, BindingError> {
        self.as_ref()
            .map(|inner| inner.binding_analysis(scope))
            .transpose()
    }
}

impl BindingAnalysis for Program {
    fn binding_analysis(&self, scope: &mut VariableStack) -> Result<Self, BindingError> {
        let mut declarations = Vec::new();

        for decl in &self.declarations {
            declarations.push(match decl {
                Declaration::Struct(struct_decl) => {
                    // TODO: handle types
                    Declaration::Struct(struct_decl.clone())
                }
                Declaration::Control(control_decl) => {
                    Declaration::Control(control_decl.binding_analysis(scope)?)
                }
                Declaration::Constant(const_decl) => {
                    Declaration::Constant(const_decl.binding_analysis(scope)?)
                }
                Declaration::Instantiation(instantiation) => {
                    Declaration::Instantiation(instantiation.binding_analysis(scope)?)
                }
            });
        }

        Ok(Program { declarations })
    }
}

impl BindingAnalysis for ControlDecl {
    fn binding_analysis(&self, scope: &mut VariableStack) -> Result<Self, BindingError> {
        // TODO: check name against types
        scope.push_scope();

        let params = self.params.binding_analysis(scope)?;
        let local_decls = self.local_decls.binding_analysis(scope)?;
        let apply_body = self.apply_body.binding_analysis(scope)?;

        scope.pop_scope();
        Ok(ControlDecl {
            name: self.name.clone(),
            params,
            local_decls,
            apply_body,
        })
    }
}

impl BindingAnalysis for Param {
    fn binding_analysis(&self, scope: &mut VariableStack) -> Result<Self, BindingError> {
        // TODO: check type ref
        let new_name = scope.insert(self.name.clone());

        Ok(Param {
            ty: self.ty.clone(),
            name: new_name,
            direction: self.direction,
        })
    }
}

impl BindingAnalysis for ControlLocalDecl {
    fn binding_analysis(&self, scope: &mut VariableStack) -> Result<Self, BindingError> {
        match self {
            ControlLocalDecl::Variable(var_decl) => Ok(ControlLocalDecl::Variable(
                var_decl.binding_analysis(scope)?,
            )),
            ControlLocalDecl::Instantiation(instantiation) => Ok(ControlLocalDecl::Instantiation(
                instantiation.binding_analysis(scope)?,
            )),
            ControlLocalDecl::Constant(const_decl) => Ok(ControlLocalDecl::Constant(
                const_decl.binding_analysis(scope)?,
            )),
            ControlLocalDecl::Action(action_decl) => Ok(ControlLocalDecl::Action(
                action_decl.binding_analysis(scope)?,
            )),
            ControlLocalDecl::Table(table_decl) => {
                Ok(ControlLocalDecl::Table(table_decl.binding_analysis(scope)?))
            }
        }
    }
}

impl BindingAnalysis for StatementOrDecl {
    fn binding_analysis(&self, scope: &mut VariableStack) -> Result<Self, BindingError> {
        match self {
            StatementOrDecl::Statement(stmt) => {
                Ok(StatementOrDecl::Statement(stmt.binding_analysis(scope)?))
            }
            StatementOrDecl::VariableDecl(var_decl) => Ok(StatementOrDecl::VariableDecl(
                var_decl.binding_analysis(scope)?,
            )),
            StatementOrDecl::ConstantDecl(const_decl) => Ok(StatementOrDecl::ConstantDecl(
                const_decl.binding_analysis(scope)?,
            )),
            StatementOrDecl::Instantiation(instantiation) => Ok(StatementOrDecl::Instantiation(
                instantiation.binding_analysis(scope)?,
            )),
        }
    }
}

impl BindingAnalysis for Statement {
    fn binding_analysis(&self, scope: &mut VariableStack) -> Result<Self, BindingError> {
        match self {
            Statement::Block(block) => Ok(Statement::Block(block.binding_analysis(scope)?)),
            Statement::If(if_stmt) => Ok(Statement::If(if_stmt.binding_analysis(scope)?)),
            Statement::Assignment(assignment) => {
                Ok(Statement::Assignment(assignment.binding_analysis(scope)?))
            }
            Statement::FunctionCall(func_call) => {
                Ok(Statement::FunctionCall(func_call.binding_analysis(scope)?))
            }
        }
    }
}

impl BindingAnalysis for BlockStatement {
    fn binding_analysis(&self, scope: &mut VariableStack) -> Result<Self, BindingError> {
        scope.push_scope();
        let stmts = self.0.binding_analysis(scope)?;
        scope.pop_scope();

        Ok(BlockStatement(stmts))
    }
}

impl BindingAnalysis for ActionDecl {
    fn binding_analysis(&self, scope: &mut VariableStack) -> Result<Self, BindingError> {
        scope.push_scope();
        let params = self.params.binding_analysis(scope)?;
        let body = self.body.binding_analysis(scope)?;
        scope.pop_scope();

        let new_name = scope.insert(self.name.clone());

        Ok(ActionDecl {
            name: new_name,
            params,
            body,
        })
    }
}

impl BindingAnalysis for TableDecl {
    fn binding_analysis(&self, scope: &mut VariableStack) -> Result<Self, BindingError> {
        let properties = self.properties.binding_analysis(scope)?;
        let new_name = scope.insert(self.name.clone());

        Ok(TableDecl {
            name: new_name,
            properties,
        })
    }
}

impl BindingAnalysis for TableProperty {
    fn binding_analysis(&self, scope: &mut VariableStack) -> Result<Self, BindingError> {
        match self {
            TableProperty::Key(keys) => Ok(TableProperty::Key(keys.binding_analysis(scope)?)),
            TableProperty::Actions(actions) => Ok(TableProperty::Actions(
                actions
                    .iter()
                    .map(|action| scope.get_new_name_or_err(action))
                    .collect::<Result<_, _>>()?,
            )),
        }
    }
}

impl BindingAnalysis for KeyElement {
    fn binding_analysis(&self, scope: &mut VariableStack) -> Result<Self, BindingError> {
        // Note: the "name" of the key is not to be modified. It refers to a key
        // type (ex. exact or lpm) and does not reference or declare a variable.
        Ok(KeyElement {
            name: self.name.clone(),
            expr: self.expr.binding_analysis(scope)?,
        })
    }
}

impl BindingAnalysis for ConstantDecl {
    fn binding_analysis(&self, scope: &mut VariableStack) -> Result<Self, BindingError> {
        let new_value = self.value.binding_analysis(scope)?;
        let new_name = scope.insert(self.name.clone());

        // todo: check type

        Ok(ConstantDecl {
            ty: self.ty.clone(),
            name: new_name,
            value: new_value,
        })
    }
}

impl BindingAnalysis for VariableDecl {
    fn binding_analysis(&self, scope: &mut VariableStack) -> Result<Self, BindingError> {
        let new_value = self.value.binding_analysis(scope)?;
        let new_name = scope.insert(self.name.clone());

        // todo: check type

        Ok(VariableDecl {
            ty: self.ty.clone(),
            name: new_name,
            value: new_value,
        })
    }
}

impl BindingAnalysis for Instantiation {
    fn binding_analysis(&self, scope: &mut VariableStack) -> Result<Self, BindingError> {
        // todo: check type

        let args = self.args.binding_analysis(scope)?;
        let new_name = scope.insert(self.name.clone());

        Ok(Instantiation {
            ty: self.ty.clone(),
            name: new_name,
            args,
        })
    }
}

impl BindingAnalysis for IfStatement {
    fn binding_analysis(&self, scope: &mut VariableStack) -> Result<Self, BindingError> {
        Ok(IfStatement {
            condition: self.condition.binding_analysis(scope)?,
            then_case: self.then_case.binding_analysis(scope)?,
            else_case: self.else_case.binding_analysis(scope)?,
        })
    }
}

impl BindingAnalysis for Assignment {
    fn binding_analysis(&self, scope: &mut VariableStack) -> Result<Self, BindingError> {
        let new_name = scope.get_new_name_or_err(&self.name)?;
        let value = self.value.binding_analysis(scope)?;

        Ok(Assignment {
            name: new_name,
            value,
        })
    }
}

impl BindingAnalysis for FunctionCall {
    fn binding_analysis(&self, scope: &mut VariableStack) -> Result<Self, BindingError> {
        let new_name = scope.get_new_name_or_err(&self.name)?;
        let arguments = self.arguments.binding_analysis(scope)?;

        Ok(FunctionCall {
            name: new_name,
            arguments,
        })
    }
}

impl BindingAnalysis for Argument {
    fn binding_analysis(&self, scope: &mut VariableStack) -> Result<Self, BindingError> {
        match self {
            Argument::Value(value) => Ok(Argument::Value(value.binding_analysis(scope)?)),
            Argument::Named(name, value) => Ok(Argument::Named(
                scope.get_new_name_or_err(name)?,
                value.binding_analysis(scope)?,
            )),
            Argument::DontCare => Ok(Argument::DontCare),
        }
    }
}

impl BindingAnalysis for Expr {
    fn binding_analysis(&self, scope: &mut VariableStack) -> Result<Self, BindingError> {
        match self {
            Expr::Bool(_) => Ok(self.clone()),
            Expr::Var(name) => Ok(Expr::Var(scope.get_new_name_or_err(name)?)),
            Expr::And(left, right) | Expr::Or(left, right) => {
                left.binding_analysis(scope)?;
                right.binding_analysis(scope)
            }
            Expr::Negation(inner) => inner.binding_analysis(scope),
            Expr::FunctionCall(func_call) => {
                Ok(Expr::FunctionCall(func_call.binding_analysis(scope)?))
            }
        }
    }
}
