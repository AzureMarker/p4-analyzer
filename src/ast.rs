pub type TypeRef = String;

#[derive(Debug)]
pub struct Program {
    pub declarations: Vec<Declaration>,
}

#[derive(Debug)]
pub enum Declaration {
    Control(ControlDecl),
    Constant(ConstantDecl),
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
}

#[derive(Debug)]
pub struct ActionDecl {
    pub name: String,
    pub params: Vec<Param>,
    pub body: BlockStatement,
}

#[derive(Clone, Debug)]
pub struct Instantiation {
    pub ty: TypeRef,
    // todo: arguments
    pub name: String,
}

#[derive(Debug)]
pub struct Param {
    pub direction: Direction,
    pub ty: TypeRef,
    pub name: String,
}

#[derive(Debug)]
pub enum Direction {
    In,
    Out,
    InOut,
}

#[derive(Clone, Debug)]
pub struct BlockStatement(pub Vec<StatementOrDecl>);

#[derive(Clone, Debug)]
pub enum Statement {
    Block(BlockStatement),
    If(IfStatement),
    Assignment(Assignment),
    // MethodCall(MethodCall),
    // actions, tables, apply block
}

#[derive(Clone, Debug)]
pub enum StatementOrDecl {
    Statement(Statement),
    VariableDecl(VariableDecl),
    ConstantDecl(ConstantDecl),
    Instantiation(Instantiation),
}

#[derive(Clone, Debug)]
pub struct VariableDecl {
    pub ty: TypeRef,
    pub name: String,
    pub value: Option<Expr>,
}

#[derive(Clone, Debug)]
pub struct ConstantDecl {
    pub ty: TypeRef,
    pub name: String,
    pub value: Expr,
}

#[derive(Clone, Debug)]
pub struct Assignment {
    pub name: String,
    pub value: Expr,
}

#[derive(Debug)]
pub struct MethodCall {
    pub name: String,
    pub arguments: Vec<Expr>,
}

#[derive(Clone, Debug)]
pub struct IfStatement {
    pub condition: Expr,
    pub then_case: BlockStatement,
    pub else_case: Option<BlockStatement>,
}

#[derive(Clone, Debug)]
pub enum Expr {
    Bool(bool),
    Var(String),
    And(Box<Expr>, Box<Expr>),
    Or(Box<Expr>, Box<Expr>),
    Negation(Box<Expr>),
}
