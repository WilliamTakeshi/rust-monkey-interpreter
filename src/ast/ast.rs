#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Statement {
    Let(String, Expression),
    Return(Expression),
    ExpressionStmt(Expression),
}

pub type Block = Vec<Statement>;

pub type Program = Vec<Statement>;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Expression {
    Ident(String),
    IntLiteral(u64),
    Boolean(bool),
    StringLiteral(String),
    ArrayLiteral(Vec<Expression>),
    HashLiteral(Vec<(Expression, Expression)>),
    IndexExpression(Box<Expression>, Box<Expression>),
    PrefixExpr(Prefix, Box<Expression>),
    InfixExpr(Infix, Box<Expression>, Box<Expression>),
    IfExpr(Box<Expression>, Block, Block),
    FnLiteral(Vec<Expression>, Block),
    Call(Box<Expression>, Vec<Expression>),
    MacroLiteral(Vec<Expression>, Block),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Prefix {
    Minus,
    Bang,
}

#[derive(Clone, Debug, PartialEq, Eq)]
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

impl ToString for Infix {
    fn to_string(&self) -> String {
        match &self {
            Self::Eq => String::from("=="),
            Self::Neq => String::from("!="),
            Self::Lt => String::from("<"),
            Self::Gt => String::from(">"),
            Self::Plus => String::from("+"),
            Self::Minus => String::from("-"),
            Self::Slash => String::from("/"),
            Self::Asterisk => String::from("*"),
        }
    }
}
