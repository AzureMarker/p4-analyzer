//! An Intermediate Representation (IR) of P4 code which includes type information

use crate::ast::Direction;

/****************************** IDs ******************************/

/// Each variable will have a unique ID
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct VariableId(pub usize);

/****************************** Types ******************************/

#[derive(Clone, Debug)]
pub enum IrType {
    Base(IrBaseType),
    Function(IrFunctionType),
    Control(IrControlType),
}

impl IrType {
    pub fn bool() -> Self {
        IrType::Base(IrBaseType::Bool)
    }

    pub fn is_bool(&self) -> bool {
        matches!(self, IrType::Base(IrBaseType::Bool))
    }
}

#[derive(Clone, Debug)]
pub enum IrBaseType {
    Void,
    Error,
    String,
    MatchKind,
    Bool,
    Int,
    BitString { width: usize },
    IntString { width: usize },
    VarBitString { width: usize },
}

#[derive(Clone, Debug)]
pub struct IrFunctionType {
    pub result: Box<IrType>,
    pub inputs: Vec<(IrType, Direction)>,
}

#[derive(Clone, Debug)]
pub struct IrControlType {
    pub inputs: Vec<(IrType, Direction)>,
}

/****************************** Nodes ******************************/
// Note: node types are sorted alphabetically

#[derive(Clone, Debug)]
pub enum IrArgument {
    Value(IrExpr),
    Named(VariableId, IrExpr),
    DontCare,
}

#[derive(Clone, Debug)]
pub struct IrBlockStatement(Vec<IrStatementOrDecl>);

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
    pub ty: IrType,
    pub condition: IrExpr,
    pub then_case: IrBlockStatement,
    pub else_case: Option<IrBlockStatement>,
}

#[derive(Clone, Debug)]
pub enum IrStatementOrDecl {}
