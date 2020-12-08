#[derive(Debug)]
pub struct Program {
    pub declarations: Vec<Declaration>,
}

#[derive(Debug)]
pub enum Declaration {
    Control(ControlDecl),
}

#[derive(Debug)]
pub struct ControlDecl {
    pub name: String,
    pub params: Vec<Param>,
    pub body: BlockStatement,
}

#[derive(Debug)]
pub struct Param {
    pub direction: ParamDirection,
    pub ty: String,
    pub name: String,
}

#[derive(Debug)]
pub enum ParamDirection {
    In,
    Out,
    InOut,
}

#[derive(Debug)]
pub struct BlockStatement(pub Vec<Statement>);

#[derive(Debug)]
pub enum Statement {
    Assignment(Assignment),
    Block(BlockStatement),
    If(IfStatement),
    // actions, tables, apply block
}

#[derive(Debug)]
pub struct Assignment {
    pub ty: String,
    pub name: String,
    pub value: Expr,
}

#[derive(Debug)]
pub struct IfStatement {
    pub condition: Expr,
    pub then_case: BlockStatement,
    pub else_case: Option<BlockStatement>,
}

#[derive(Debug)]
pub enum Expr {
    Bool(bool),
    Var(String),
    And(Box<Expr>, Box<Expr>),
    Or(Box<Expr>, Box<Expr>),
    Negation(Box<Expr>),
}
