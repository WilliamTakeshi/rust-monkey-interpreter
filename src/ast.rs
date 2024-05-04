
#[derive(Debug, PartialEq)]
pub enum Expression {
    Ident(String),
    Literal(u64),
    PrefixExpr(Prefix, Box<Expression>),
    // InfixExpr(Infix, Box<Expr>, Box<Expr>),
}

#[derive(Debug, PartialEq)]
pub enum Prefix {
    Minus,
    Bang,
}

// #[derive(Debug)]
// pub enum Infix {

// }


#[derive(Debug, PartialEq)]
pub enum Statement {
    Let(String),
    Return,
    ExpressionStmt(Expression),
}

pub type Program = Vec<Statement>;
