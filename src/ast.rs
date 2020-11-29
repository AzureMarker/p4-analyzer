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
    pub body: Vec<Statement>,
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
pub enum Statement {
    Assignment(Assignment),
    // actions, tables, apply block
}

#[derive(Debug)]
pub struct Assignment {
    pub ty: String,
    pub name: String,
    pub value: Expr,
}

#[derive(Debug)]
pub enum Expr {
    Bool(bool),
}
