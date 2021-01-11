pub type TypeRef = String;

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
    pub local_decls: Vec<ControlLocalDecl>,
    pub apply_body: BlockStatement,
}

#[derive(Debug)]
pub enum ControlLocalDecl {
    VariableDecl(VariableDecl),
    Instantiation(Instantiation),
}

#[derive(Debug)]
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

#[derive(Debug)]
pub struct BlockStatement(pub Vec<Statement>);

#[derive(Debug)]
pub enum Statement {
    VariableDecl(VariableDecl),
    Block(BlockStatement),
    If(IfStatement),
    // actions, tables, apply block
}

#[derive(Debug)]
pub struct VariableDecl {
    pub ty: TypeRef,
    pub name: String,
    pub value: Option<Expr>,
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
