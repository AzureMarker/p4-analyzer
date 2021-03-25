// Note: types are sorted alphabetically

#[derive(Debug)]
pub struct ActionDecl {
    pub name: String,
    pub params: Vec<Param>,
    pub body: BlockStatement,
}

#[derive(Clone, Debug)]
pub enum Argument {
    Value(Expr),
    Named(String, Expr),
    DontCare,
}

#[derive(Clone, Debug)]
pub struct Assignment {
    pub lvalue: LValue,
    pub value: Expr,
}

#[derive(Clone, Debug)]
pub enum BaseType {
    Bool,
    // TODO: Add more base types
}

#[derive(Clone, Debug)]
pub struct BlockStatement(pub Vec<StatementOrDecl>);

#[derive(Clone, Debug)]
pub struct ConstantDecl {
    pub ty: TypeRef,
    pub name: String,
    pub value: Expr,
}

#[derive(Debug)]
pub struct ControlDecl {
    pub name: String,
    pub params: Vec<Param>,
    pub local_decls: Vec<ControlLocalDecl>,
    pub apply_body: BlockStatement,
}

#[derive(Debug)]
pub enum ControlLocalDecl {
    Variable(VariableDecl),
    Instantiation(Instantiation),
    Constant(ConstantDecl),
    Action(ActionDecl),
    Table(TableDecl),
}

#[derive(Debug)]
pub enum Declaration {
    Struct(StructDecl),
    Control(ControlDecl),
    Constant(ConstantDecl),
    Instantiation(Instantiation),
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum Direction {
    In,
    Out,
    InOut,
}

#[derive(Clone, Debug)]
pub enum Expr {
    Bool(bool),
    Var(String),
    And(Box<Expr>, Box<Expr>),
    Or(Box<Expr>, Box<Expr>),
    Negation(Box<Expr>),
    FunctionCall(FunctionCall),
    FieldAccess(Box<Expr>, String),
}

#[derive(Clone, Debug)]
pub struct FunctionCall {
    pub target: String,
    pub arguments: Vec<Argument>,
}

#[derive(Clone, Debug)]
pub struct IfStatement {
    pub condition: Expr,
    pub then_case: BlockStatement,
    pub else_case: Option<BlockStatement>,
}

#[derive(Clone, Debug)]
pub struct Instantiation {
    pub ty: TypeRef,
    pub args: Vec<Argument>,
    pub name: String,
}

#[derive(Debug)]
pub struct KeyElement {
    pub expr: Expr,
    pub match_kind: String,
}

#[derive(Clone, Debug)]
pub enum LValue {
    Var(String),
    Field(Box<LValue>, String),
}

#[derive(Debug)]
pub struct Param {
    pub direction: Direction,
    pub ty: TypeRef,
    pub name: String,
}

#[derive(Debug)]
pub struct Program {
    pub declarations: Vec<Declaration>,
}

#[derive(Clone, Debug)]
pub enum Statement {
    Block(BlockStatement),
    If(IfStatement),
    Assignment(Assignment),
    FunctionCall(FunctionCall),
}

#[derive(Clone, Debug)]
pub enum StatementOrDecl {
    Statement(Statement),
    VariableDecl(VariableDecl),
    ConstantDecl(ConstantDecl),
    Instantiation(Instantiation),
}

#[derive(Clone, Debug)]
pub struct StructDecl {
    pub name: String,
    pub fields: Vec<(TypeRef, String)>,
}

#[derive(Debug)]
pub struct TableDecl {
    pub name: String,
    pub properties: Vec<TableProperty>,
}

#[derive(Debug)]
pub enum TableProperty {
    Key(Vec<KeyElement>),
    Actions(Vec<String>),
}

#[derive(Clone, Debug)]
pub enum TypeRef {
    Base(BaseType),
    Identifier(String),
}

#[derive(Clone, Debug)]
pub struct VariableDecl {
    pub ty: TypeRef,
    pub name: String,
    pub value: Option<Expr>,
}
