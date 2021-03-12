//! An Intermediate Representation (IR) of P4 code which includes type information

use crate::ast::Direction;
use std::fmt;
use std::fmt::{Display, Formatter};

/****************************** IDs ******************************/

/// Each variable will have a unique ID
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct VariableId(pub usize);

/// Each variable will have a unique ID
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct TypeId(pub usize);

impl Display for VariableId {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "var_{}", self.0)
    }
}

/****************************** Types ******************************/

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum IrType {
    Base(IrBaseType),
    Function(IrFunctionType),
    Control(IrControlType),
    Struct(IrStructType),
}

impl IrType {
    pub fn bool() -> Self {
        IrType::Base(IrBaseType::Bool)
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum IrBaseType {
    Void,
    // TODO: table is not listed in the spec's types, so how does p4c handle it?
    Table,
    Error,
    String,
    MatchKind,
    Bool,
    Int,
    BitString { width: usize },
    IntString { width: usize },
    VarBitString { width: usize },
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct IrFunctionType {
    pub result: Box<IrType>,
    pub inputs: Vec<(IrType, Direction)>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct IrControlType {
    pub inputs: Vec<(IrType, Direction)>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct IrStructType {
    pub id: TypeId,
    // TODO: do we need the field name string?
    pub field_tys: Vec<(IrType, String)>,
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
    pub var: VariableId,
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
}

#[derive(Clone, Debug)]
pub struct IrFunctionCall {
    pub result_ty: IrType,
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

#[derive(Debug)]
pub struct IrParam {
    pub direction: Direction,
    pub ty: IrType,
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
