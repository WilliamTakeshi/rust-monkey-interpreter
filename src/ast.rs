#[derive(Debug, PartialEq)]
pub enum Expression {
    Ident(String),
    Literal(u64),
    Boolean(bool),
    PrefixExpr(Prefix, Box<Expression>),
    InfixExpr(Infix, Box<Expression>, Box<Expression>),
    IfExpr(Box<Expression>, Block, Block),
    FnLiteral(Vec<Expression>, Block),
    Call(Box<Expression>, Vec<Expression>)
}

#[derive(Debug, PartialEq)]
pub enum Prefix {
    Minus,
    Bang,
}

#[derive(Debug, PartialEq)]
pub enum Infix {
    Eq,
    Neq,
    Lt,
    Gt,
    Plus,
    Minus,
    Slash,
    Asterisk,
}

#[derive(Debug, PartialEq)]
pub enum Statement {
    Let(String),
    Return,
    ExpressionStmt(Expression),
}

pub type Block = Vec<Statement>;

pub type Program = Vec<Statement>;
