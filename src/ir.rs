//! An Intermediate Representation (IR) of P4 code which includes type information

use crate::ast::Direction;
use std::fmt;
use std::fmt::{Display, Formatter};

/****************************** IDs ******************************/

/// Each variable will have a unique ID
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct VariableId(pub usize);

/// Type variables (i.e. generics) have unique IDs
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct TypeVarId(pub usize);

impl Display for VariableId {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "var_{}", self.0)
    }
}

/****************************** Types ******************************/

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum IrType {
    Base(IrBaseType),
    Table,
    Function(IrFunctionType),
    #[allow(unused)]
    Constructor(IrConstructorType),
}

impl IrType {
    pub fn bool() -> Self {
        IrType::Base(IrBaseType::Bool)
    }

    pub fn string() -> Self {
        IrType::Base(IrBaseType::String)
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
#[allow(unused)]
pub enum IrBaseType {
    // TODO: extend these types, e.g. with varbit<> and int<>
    Bool,
    Int,
    Bit { width: usize },
    String,
    Error,
    MatchKind,
    Enum { name: String, fields: Vec<String> },
    Struct { fields: Vec<(IrBaseType, String)> },
    Header { fields: Vec<(IrBaseType, String)> },
    TyVar(TypeVarId),
}

impl IrBaseType {
    pub fn void() -> Self {
        IrBaseType::Struct { fields: Vec::new() }
    }

    pub fn is_void(&self) -> bool {
        matches!(self, IrBaseType::Struct { fields } if fields.is_empty())
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct IrFunctionType {
    pub result: Box<IrBaseType>,
    pub inputs: Vec<(Direction, IrBaseType)>,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct IrConstructorType {
    pub result: Box<IrType>,
    pub inputs: Vec<(IrType, String)>,
}

/****************************** Nodes ******************************/
// Note: node types are sorted alphabetically

#[derive(Debug)]
pub struct IrActionDecl {
    pub ty: IrFunctionType,
    pub id: VariableId,
    pub params: Vec<IrParam>,
    pub body: IrBlockStatement,
}

#[derive(Clone, Debug)]
pub enum IrArgument {
    Value(IrExpr),
    Named(VariableId, IrExpr),
    DontCare,
}

#[derive(Clone, Debug)]
pub struct IrAssignment {
    pub lvalue: IrLValue,
    pub value: IrExpr,
}

#[derive(Clone, Debug)]
pub struct IrBlockStatement(pub Vec<IrStatementOrDecl>);

#[derive(Debug)]
pub struct IrControlDecl {
    // todo: store type ID
    // pub name: String,
    pub params: Vec<IrParam>,
    pub local_decls: Vec<IrControlLocalDecl>,
    pub apply_body: IrBlockStatement,
}

#[derive(Debug)]
pub enum IrControlLocalDecl {
    Variable(IrVariableDecl),
    Instantiation(IrInstantiation),
    Action(IrActionDecl),
    Table(IrTableDecl),
}

#[derive(Debug)]
pub enum IrDeclaration {
    Control(IrControlDecl),
    Constant(IrVariableDecl),
    Instantiation(IrInstantiation),
}

#[derive(Clone, Debug)]
pub struct IrExpr {
    pub ty: IrType,
    pub data: IrExprData,
}

#[derive(Clone, Debug)]
pub enum IrExprData {
    Bool(bool),
    Var(VariableId),
    And(Box<IrExpr>, Box<IrExpr>),
    Or(Box<IrExpr>, Box<IrExpr>),
    Negation(Box<IrExpr>),
    FunctionCall(IrFunctionCall),
    FieldAccess(Box<IrExpr>, String),
    Struct(Vec<(String, IrExpr)>),
}

#[derive(Clone, Debug)]
pub struct IrFunctionCall {
    pub result_ty: IrBaseType,
    pub target: VariableId,
    pub arguments: Vec<IrArgument>,
}

#[derive(Clone, Debug)]
pub struct IrIfStatement {
    pub condition: IrExpr,
    pub then_case: IrBlockStatement,
    pub else_case: Option<IrBlockStatement>,
}

#[derive(Clone, Debug)]
pub struct IrInstantiation {
    pub ty: IrType,
    pub args: Vec<IrArgument>,
    pub id: VariableId,
}

#[derive(Debug)]
pub struct IrKeyElement {
    pub expr: IrExpr,
    pub match_kind: String,
}

#[derive(Clone, Debug)]
pub struct IrLValue {
    pub ty: IrType,
    pub data: IrLValueData,
}

impl IrLValue {
    pub fn var_id(&self) -> VariableId {
        match &self.data {
            IrLValueData::Var(id) => *id,
            IrLValueData::Field(target, _) => target.var_id(),
        }
    }
}

#[derive(Clone, Debug)]
pub enum IrLValueData {
    Var(VariableId),
    Field(Box<IrLValue>, String),
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct IrParam {
    pub direction: Direction,
    pub ty: IrBaseType,
    pub id: VariableId,
}

#[derive(Debug)]
pub struct IrProgram {
    pub declarations: Vec<IrDeclaration>,
}

#[derive(Clone, Debug)]
pub enum IrStatement {
    Block(IrBlockStatement),
    If(IrIfStatement),
    Assignment(IrAssignment),
    FunctionCall(IrFunctionCall),
}

#[derive(Clone, Debug)]
pub enum IrStatementOrDecl {
    Statement(IrStatement),
    VariableDecl(IrVariableDecl),
    Instantiation(IrInstantiation),
}

#[derive(Debug)]
pub struct IrTableDecl {
    pub id: VariableId,
    pub properties: Vec<IrTableProperty>,
}

#[derive(Debug)]
pub enum IrTableProperty {
    Key(Vec<IrKeyElement>),
    Actions(Vec<VariableId>),
}

#[derive(Clone, Debug)]
pub struct IrVariableDecl {
    pub ty: IrType,
    pub id: VariableId,
    pub value: Option<IrExpr>,
    pub is_const: bool,
}
