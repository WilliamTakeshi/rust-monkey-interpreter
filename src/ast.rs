
#[derive(Debug, PartialEq)]
pub enum Expression {
    Ident(String),
    Literal(u64),
    Boolean(bool),
    PrefixExpr(Prefix, Box<Expression>),
    InfixExpr(Infix, Box<Expression>, Box<Expression>),
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

pub type Program = Vec<Statement>;
