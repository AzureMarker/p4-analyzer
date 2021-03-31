//! Perform frontend analysis:
//! * Binding analysis (connect variables to declarations and give each a unique name)
//! * Type checking (check the type usage and attach type information to AST nodes)

use std::collections::{HashMap, HashSet};

use crate::ast::{
    ActionDecl, Argument, Assignment, BaseType, BlockStatement, ConstantDecl, ControlDecl,
    ControlLocalDecl, Declaration, Expr, FunctionCall, IfStatement, Instantiation, KeyElement,
    LValue, Param, Program, Statement, StatementOrDecl, StructDecl, TableDecl, TableProperty,
    TypeRef, VariableDecl,
};
use crate::ir::{
    IrActionDecl, IrArgument, IrAssignment, IrBaseType, IrBlockStatement, IrControlDecl,
    IrControlLocalDecl, IrDeclaration, IrExpr, IrExprData, IrFunctionCall, IrFunctionType,
    IrIfStatement, IrInstantiation, IrKeyElement, IrLValue, IrLValueData, IrParam, IrProgram,
    IrStatement, IrStatementOrDecl, IrTableDecl, IrTableProperty, IrType, IrVariableDecl,
    VariableId,
};

#[derive(Debug)]
pub enum TypeCheckError {
    /// The declaration of this variable was not found
    UnknownVar(String),
    /// The declaration of this type was not found
    UnknownType(String),
    /// The given field was not found on the struct/header
    UnknownField(String),
    /// There is more than one declaration of this variable in the same scope
    DuplicateDecl(String),
    /// There is already a type declared with the given name
    DuplicateTypeDecl(String),
    /// Expected one type but got another
    MismatchedTypes { expected: IrType, found: IrType },
    /// Expected a {expected}, found other type
    MismatchedTypeKind {
        expected: &'static str,
        found: IrType,
    },
    /// Tried to assign to a const value
    ModifyingConstValue,
}

/// Run binding analysis on the program, creating a new program with unique
/// variable names given to each variable and a map from new name to ID.
pub fn run_type_checking(
    program: &Program,
) -> Result<(IrProgram, ProgramMetadata), TypeCheckError> {
    let mut env = EnvironmentStack::new();
    let new_program = program.type_check(&mut env)?;

    Ok((new_program, env.into()))
}

/// Holds some metadata about the program, such as the IR type of each declared type.
// TODO: do we need this passed through ToGcl?
pub struct ProgramMetadata {
    pub types: HashMap<String, IrType>,
}

impl From<EnvironmentStack> for ProgramMetadata {
    fn from(env: EnvironmentStack) -> Self {
        Self { types: env.types }
    }
}

/// Maps AST identifiers such as variable names to semantic information for
/// items declared in a specific scope. For example:
/// * Variable name to ID
#[derive(Default)]
struct Environment {
    variables: HashMap<String, VariableId>,
}

/// Maps AST identifiers such as variable names to semantic information via a
/// stack of environments. This stack represents the various levels of scope at
/// a specific point in the program.
#[derive(Default)]
struct EnvironmentStack {
    stack: Vec<Environment>,
    var_tys: HashMap<VariableId, IrType>,
    types: HashMap<String, IrType>,
    const_set: HashSet<VariableId>,
    next_id: usize,
}

impl EnvironmentStack {
    fn new() -> Self {
        Self::default()
    }

    /// Get the ID and type of the variable
    fn get_var(&self, name: &str) -> Option<(VariableId, &IrType)> {
        let id = self
            .stack
            .iter()
            .rev()
            .filter_map(|env| env.variables.get(name))
            .copied()
            .next()?;
        let ty = self.var_tys.get(&id)?;

        Some((id, ty))
    }

    fn get_var_or_err(&self, name: &str) -> Result<(VariableId, &IrType), TypeCheckError> {
        self.get_var(name)
            .ok_or_else(|| TypeCheckError::UnknownVar(name.to_string()))
    }

    /// Insert a variable into the environment and return a unique ID for it.
    /// If the variable has already been declared in this same scope, an
    /// error is returned.
    fn insert_var(&mut self, name: String, ty: IrType) -> Result<VariableId, TypeCheckError> {
        if self.stack.is_empty() {
            self.stack.push(Environment::default());
        }

        let env = self.stack.last_mut().unwrap();

        if env.variables.contains_key(&name) {
            return Err(TypeCheckError::DuplicateDecl(name));
        }

        let id = VariableId(self.next_id);
        self.next_id += 1;
        self.var_tys.insert(id, ty);
        env.variables.insert(name, id);

        Ok(id)
    }

    fn get_type(&self, name: &str) -> Option<&IrType> {
        self.types.get(name)
    }

    fn get_type_or_err(&self, name: &str) -> Result<&IrType, TypeCheckError> {
        self.get_type(name)
            .ok_or_else(|| TypeCheckError::UnknownType(name.to_string()))
    }

    /// Insert a user-defined type into the map
    fn insert_type(&mut self, name: String, ty: IrType) -> Result<(), TypeCheckError> {
        if self.types.insert(name.clone(), ty).is_some() {
            return Err(TypeCheckError::DuplicateTypeDecl(name));
        }

        Ok(())
    }

    /// Mark a variable as const
    fn mark_const(&mut self, id: VariableId) {
        self.const_set.insert(id);
    }

    /// Check if a variable was marked as const
    fn is_const(&self, id: VariableId) -> bool {
        self.const_set.contains(&id)
    }

    /// Push a scope (new environment) onto the stack
    fn push_scope(&mut self) {
        self.stack.push(Environment::default());
    }

    /// Pop a scope (environment) from the stack
    fn pop_scope(&mut self) {
        self.stack.pop();
    }
}

impl IrType {
    /// Expect to find a base type. Return an error if this is not a base type.
    fn unwrap_base(self) -> Result<IrBaseType, TypeCheckError> {
        match self {
            IrType::Base(ty) => Ok(ty),
            ty => Err(TypeCheckError::MismatchedTypeKind {
                expected: "base type",
                found: ty,
            }),
        }
    }

    /// Assuming this is a struct or header type, get the type of a field.
    fn get_field_ty(&self, field: &str) -> Result<&IrBaseType, TypeCheckError> {
        let fields = match self {
            IrType::Base(IrBaseType::Struct { fields })
            | IrType::Base(IrBaseType::Header { fields }) => fields,
            _ => {
                return Err(TypeCheckError::MismatchedTypeKind {
                    expected: "struct or header",
                    found: self.clone(),
                })
            }
        };

        fields
            .iter()
            .find_map(|(ty, name)| if name == field { Some(ty) } else { None })
            .ok_or_else(|| TypeCheckError::UnknownField(field.to_string()))
    }
}

/// Trait for performing type checking and binding analysis on an AST node while
/// transforming it into typed IR.
trait TypeCheck: Sized {
    type IrNode;

    fn type_check(&self, env: &mut EnvironmentStack) -> Result<Self::IrNode, TypeCheckError>;
}

impl<T: TypeCheck> TypeCheck for Vec<T> {
    type IrNode = Vec<T::IrNode>;

    fn type_check(&self, env: &mut EnvironmentStack) -> Result<Self::IrNode, TypeCheckError> {
        let items = self
            .iter()
            .map(|item| item.type_check(env))
            .collect::<Result<_, _>>()?;

        Ok(items)
    }
}

impl<T: TypeCheck> TypeCheck for Option<T> {
    type IrNode = Option<T::IrNode>;

    fn type_check(&self, env: &mut EnvironmentStack) -> Result<Self::IrNode, TypeCheckError> {
        self.as_ref().map(|inner| inner.type_check(env)).transpose()
    }
}

impl TypeCheck for Program {
    type IrNode = IrProgram;

    fn type_check(&self, env: &mut EnvironmentStack) -> Result<Self::IrNode, TypeCheckError> {
        Ok(IrProgram {
            declarations: self
                .declarations
                .type_check(env)?
                .into_iter()
                .flatten()
                .collect(),
        })
    }
}

impl TypeCheck for Declaration {
    type IrNode = Option<IrDeclaration>;

    fn type_check(&self, env: &mut EnvironmentStack) -> Result<Self::IrNode, TypeCheckError> {
        match self {
            Declaration::Struct(StructDecl { name, fields }) => {
                let struct_ty = IrType::Base(IrBaseType::Struct {
                    fields: fields
                        .iter()
                        .map(|(ty_ref, field_name)| {
                            let ty = ty_ref.type_check(env)?.unwrap_base()?;
                            Ok((ty, field_name.clone()))
                        })
                        .collect::<Result<_, _>>()?,
                });

                env.insert_type(name.clone(), struct_ty)?;
                Ok(None)
            }
            Declaration::Control(control_decl) => {
                Ok(Some(IrDeclaration::Control(control_decl.type_check(env)?)))
            }
            Declaration::Constant(const_decl) => {
                Ok(Some(IrDeclaration::Constant(const_decl.type_check(env)?)))
            }
            Declaration::Instantiation(instantiation) => Ok(Some(IrDeclaration::Instantiation(
                instantiation.type_check(env)?,
            ))),
        }
    }
}

impl TypeCheck for ControlDecl {
    type IrNode = IrControlDecl;

    fn type_check(&self, env: &mut EnvironmentStack) -> Result<Self::IrNode, TypeCheckError> {
        // TODO: check name against types

        env.push_scope();
        let params = self.params.type_check(env)?;
        let local_decls = self.local_decls.type_check(env)?;
        let apply_body = self.apply_body.type_check(env)?;
        env.pop_scope();

        Ok(IrControlDecl {
            // name: self.name.clone(),
            params,
            local_decls,
            apply_body,
        })
    }
}

impl TypeCheck for Param {
    type IrNode = IrParam;

    fn type_check(&self, env: &mut EnvironmentStack) -> Result<Self::IrNode, TypeCheckError> {
        let ty = self.ty.type_check(env)?;
        let id = env.insert_var(self.name.clone(), ty.clone())?;

        Ok(IrParam {
            ty: ty.unwrap_base()?,
            id,
            direction: self.direction,
        })
    }
}

impl TypeCheck for ControlLocalDecl {
    type IrNode = IrControlLocalDecl;

    fn type_check(&self, env: &mut EnvironmentStack) -> Result<Self::IrNode, TypeCheckError> {
        match self {
            ControlLocalDecl::Variable(var_decl) => {
                Ok(IrControlLocalDecl::Variable(var_decl.type_check(env)?))
            }
            ControlLocalDecl::Instantiation(instantiation) => Ok(
                IrControlLocalDecl::Instantiation(instantiation.type_check(env)?),
            ),
            ControlLocalDecl::Constant(const_decl) => {
                Ok(IrControlLocalDecl::Variable(const_decl.type_check(env)?))
            }
            ControlLocalDecl::Action(action_decl) => {
                Ok(IrControlLocalDecl::Action(action_decl.type_check(env)?))
            }
            ControlLocalDecl::Table(table_decl) => {
                Ok(IrControlLocalDecl::Table(table_decl.type_check(env)?))
            }
        }
    }
}

impl TypeCheck for StatementOrDecl {
    type IrNode = IrStatementOrDecl;

    fn type_check(&self, env: &mut EnvironmentStack) -> Result<Self::IrNode, TypeCheckError> {
        match self {
            StatementOrDecl::Statement(stmt) => {
                Ok(IrStatementOrDecl::Statement(stmt.type_check(env)?))
            }
            StatementOrDecl::VariableDecl(var_decl) => {
                Ok(IrStatementOrDecl::VariableDecl(var_decl.type_check(env)?))
            }
            StatementOrDecl::ConstantDecl(const_decl) => {
                Ok(IrStatementOrDecl::VariableDecl(const_decl.type_check(env)?))
            }
            StatementOrDecl::Instantiation(instantiation) => Ok(IrStatementOrDecl::Instantiation(
                instantiation.type_check(env)?,
            )),
        }
    }
}

impl TypeCheck for Statement {
    type IrNode = IrStatement;

    fn type_check(&self, env: &mut EnvironmentStack) -> Result<Self::IrNode, TypeCheckError> {
        match self {
            Statement::Block(block) => Ok(IrStatement::Block(block.type_check(env)?)),
            Statement::If(if_stmt) => Ok(IrStatement::If(if_stmt.type_check(env)?)),
            Statement::Assignment(assignment) => {
                Ok(IrStatement::Assignment(assignment.type_check(env)?))
            }
            Statement::FunctionCall(func_call) => {
                Ok(IrStatement::FunctionCall(func_call.type_check(env)?))
            }
        }
    }
}

impl TypeCheck for BlockStatement {
    type IrNode = IrBlockStatement;

    fn type_check(&self, env: &mut EnvironmentStack) -> Result<Self::IrNode, TypeCheckError> {
        env.push_scope();
        let stmts = self.0.type_check(env)?;
        env.pop_scope();

        Ok(IrBlockStatement(stmts))
    }
}

impl TypeCheck for ActionDecl {
    type IrNode = IrActionDecl;

    fn type_check(&self, env: &mut EnvironmentStack) -> Result<Self::IrNode, TypeCheckError> {
        env.push_scope();
        let params = self.params.type_check(env)?;
        let body = self.body.type_check(env)?;
        env.pop_scope();

        let ty = IrFunctionType {
            result: Box::new(IrBaseType::void()),
            inputs: params
                .iter()
                .map(|param| (param.direction, param.ty.clone()))
                .collect(),
        };
        let id = env.insert_var(self.name.clone(), IrType::Function(ty.clone()))?;

        Ok(IrActionDecl {
            ty,
            id,
            params,
            body,
        })
    }
}

impl TypeCheck for TableDecl {
    type IrNode = IrTableDecl;

    fn type_check(&self, env: &mut EnvironmentStack) -> Result<Self::IrNode, TypeCheckError> {
        let properties = self.properties.type_check(env)?;
        let id = env.insert_var(self.name.clone(), IrType::Table)?;

        Ok(IrTableDecl { id, properties })
    }
}

impl TypeCheck for TableProperty {
    type IrNode = IrTableProperty;

    fn type_check(&self, env: &mut EnvironmentStack) -> Result<Self::IrNode, TypeCheckError> {
        match self {
            TableProperty::Key(keys) => Ok(IrTableProperty::Key(keys.type_check(env)?)),
            TableProperty::Actions(actions) => Ok(IrTableProperty::Actions(
                actions
                    .iter()
                    .map(|action| {
                        let (id, ty) = env.get_var_or_err(action)?;

                        match ty {
                            IrType::Function(IrFunctionType { result, .. }) if result.is_void() => {
                                Ok(id)
                            }
                            _ => Err(TypeCheckError::MismatchedTypeKind {
                                expected: "action",
                                found: ty.clone(),
                            }),
                        }
                    })
                    .collect::<Result<_, _>>()?,
            )),
        }
    }
}

impl TypeCheck for KeyElement {
    type IrNode = IrKeyElement;

    fn type_check(&self, env: &mut EnvironmentStack) -> Result<Self::IrNode, TypeCheckError> {
        // Note: the "name" of the key is not to be modified. It refers to a key
        // type (ex. exact or lpm) and does not reference or declare a variable.
        // TODO: verify that the match kind has been declared previously
        Ok(IrKeyElement {
            match_kind: self.match_kind.clone(),
            expr: self.expr.type_check(env)?,
        })
    }
}

impl TypeCheck for TypeRef {
    type IrNode = IrType;

    fn type_check(&self, env: &mut EnvironmentStack) -> Result<Self::IrNode, TypeCheckError> {
        match self {
            TypeRef::Base(base_ty) => Ok(IrType::Base(base_ty.type_check(env)?)),
            TypeRef::Identifier(name) => env.get_type_or_err(name).map(IrType::clone),
        }
    }
}

impl TypeCheck for BaseType {
    type IrNode = IrBaseType;

    fn type_check(&self, _env: &mut EnvironmentStack) -> Result<Self::IrNode, TypeCheckError> {
        match self {
            BaseType::Bool => Ok(IrBaseType::Bool),
            BaseType::String => Ok(IrBaseType::String),
        }
    }
}

impl TypeCheck for ConstantDecl {
    type IrNode = IrVariableDecl;

    fn type_check(&self, env: &mut EnvironmentStack) -> Result<Self::IrNode, TypeCheckError> {
        let ty = self.ty.type_check(env)?;
        let value = self.value.type_check(env)?;
        let id = env.insert_var(self.name.clone(), ty.clone())?;

        assert_ty(&value.ty, &ty)?;
        env.mark_const(id);

        Ok(IrVariableDecl {
            ty,
            id,
            value: Some(value),
            is_const: true,
        })
    }
}

impl TypeCheck for VariableDecl {
    type IrNode = IrVariableDecl;

    fn type_check(&self, env: &mut EnvironmentStack) -> Result<Self::IrNode, TypeCheckError> {
        let ty = self.ty.type_check(env)?;
        let value = self.value.type_check(env)?;
        let id = env.insert_var(self.name.clone(), ty.clone())?;

        if let Some(value) = &value {
            assert_ty(&value.ty, &ty)?;
        }

        Ok(IrVariableDecl {
            ty,
            id,
            value,
            is_const: false,
        })
    }
}

impl TypeCheck for Instantiation {
    type IrNode = IrInstantiation;

    fn type_check(&self, env: &mut EnvironmentStack) -> Result<Self::IrNode, TypeCheckError> {
        let ty = self.ty.type_check(env)?;
        let args = self.args.type_check(env)?;
        let id = env.insert_var(self.name.clone(), ty.clone())?;

        // TODO: ensure type is a constructor and arg types match up

        Ok(IrInstantiation { ty, id, args })
    }
}

impl TypeCheck for IfStatement {
    type IrNode = IrIfStatement;

    fn type_check(&self, env: &mut EnvironmentStack) -> Result<Self::IrNode, TypeCheckError> {
        let condition = self.condition.type_check(env)?;
        let then_case = self.then_case.type_check(env)?;
        let else_case = self.else_case.type_check(env)?;

        assert_ty(&condition.ty, &IrType::bool())?;

        Ok(IrIfStatement {
            condition,
            then_case,
            else_case,
        })
    }
}

impl TypeCheck for Assignment {
    type IrNode = IrAssignment;

    fn type_check(&self, env: &mut EnvironmentStack) -> Result<Self::IrNode, TypeCheckError> {
        let lvalue = self.lvalue.type_check(env)?;
        let value = self.value.type_check(env)?;

        if env.is_const(lvalue.var_id()) {
            return Err(TypeCheckError::ModifyingConstValue);
        }

        assert_ty(&value.ty, &lvalue.ty)?;

        Ok(IrAssignment { lvalue, value })
    }
}

impl TypeCheck for LValue {
    type IrNode = IrLValue;

    fn type_check(&self, env: &mut EnvironmentStack) -> Result<Self::IrNode, TypeCheckError> {
        match self {
            LValue::Var(var) => {
                let (id, ty) = env.get_var_or_err(var)?;
                Ok(IrLValue {
                    ty: ty.clone(),
                    data: IrLValueData::Var(id),
                })
            }
            LValue::Field(target, field) => {
                let target_ir = target.type_check(env)?;
                let field_ty = target_ir.ty.get_field_ty(field)?;

                Ok(IrLValue {
                    ty: IrType::Base(field_ty.clone()),
                    data: IrLValueData::Field(Box::new(target_ir), field.to_string()),
                })
            }
        }
    }
}

impl TypeCheck for FunctionCall {
    type IrNode = IrFunctionCall;

    fn type_check(&self, env: &mut EnvironmentStack) -> Result<Self::IrNode, TypeCheckError> {
        let (target_id, target_ty) = env.get_var_or_err(&self.target)?;
        let target_ty = target_ty.clone();
        let arguments = self.arguments.type_check(env)?;

        let func_ty = match target_ty {
            IrType::Function(ty) => ty,
            _ => {
                return Err(TypeCheckError::MismatchedTypeKind {
                    expected: "function",
                    found: target_ty,
                })
            }
        };

        Ok(IrFunctionCall {
            result_ty: *func_ty.result,
            target: target_id,
            arguments,
        })
    }
}

impl TypeCheck for Argument {
    type IrNode = IrArgument;

    fn type_check(&self, env: &mut EnvironmentStack) -> Result<Self::IrNode, TypeCheckError> {
        match self {
            Argument::Value(value) => Ok(IrArgument::Value(value.type_check(env)?)),
            Argument::Named(name, value) => Ok(IrArgument::Named(
                env.get_var_or_err(name)?.0,
                value.type_check(env)?,
            )),
            Argument::DontCare => Ok(IrArgument::DontCare),
        }
    }
}

fn assert_ty(found: &IrType, expected: &IrType) -> Result<(), TypeCheckError> {
    if found == expected {
        Ok(())
    } else {
        Err(TypeCheckError::MismatchedTypes {
            expected: expected.clone(),
            found: found.clone(),
        })
    }
}

impl TypeCheck for Expr {
    type IrNode = IrExpr;

    fn type_check(&self, env: &mut EnvironmentStack) -> Result<Self::IrNode, TypeCheckError> {
        match self {
            Expr::Bool(value) => Ok(IrExpr {
                ty: IrType::bool(),
                data: IrExprData::Bool(*value),
            }),
            Expr::Var(name) => {
                let (id, ty) = env.get_var_or_err(name)?;

                Ok(IrExpr {
                    ty: ty.clone(),
                    data: IrExprData::Var(id),
                })
            }
            Expr::And(left, right) => {
                let left_ir = left.type_check(env)?;
                let right_ir = right.type_check(env)?;

                assert_ty(&left_ir.ty, &IrType::bool())?;
                assert_ty(&right_ir.ty, &IrType::bool())?;

                Ok(IrExpr {
                    ty: IrType::bool(),
                    data: IrExprData::And(Box::new(left_ir), Box::new(right_ir)),
                })
            }
            Expr::Or(left, right) => {
                let left_ir = left.type_check(env)?;
                let right_ir = right.type_check(env)?;

                assert_ty(&left_ir.ty, &IrType::bool())?;
                assert_ty(&right_ir.ty, &IrType::bool())?;

                Ok(IrExpr {
                    ty: IrType::bool(),
                    data: IrExprData::Or(Box::new(left_ir), Box::new(right_ir)),
                })
            }
            Expr::Negation(inner) => {
                let inner_ir = inner.type_check(env)?;

                assert_ty(&inner_ir.ty, &IrType::bool())?;

                Ok(IrExpr {
                    ty: IrType::bool(),
                    data: IrExprData::Negation(Box::new(inner_ir)),
                })
            }
            Expr::FunctionCall(func_call) => {
                let func_call_ir = func_call.type_check(env)?;

                Ok(IrExpr {
                    ty: IrType::Base(func_call_ir.result_ty.clone()),
                    data: IrExprData::FunctionCall(func_call_ir),
                })
            }
            Expr::FieldAccess(target, field) => {
                let target_ir = target.type_check(env)?;
                let field_ty = target_ir.ty.get_field_ty(field)?;

                Ok(IrExpr {
                    ty: IrType::Base(field_ty.clone()),
                    data: IrExprData::FieldAccess(Box::new(target_ir), field.clone()),
                })
            }
        }
    }
}
