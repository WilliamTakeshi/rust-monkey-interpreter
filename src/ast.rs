// type Node interface {
//     TokenLiteral() string
// }
// type Statement interface {
//     Node
//     statementNode()
// }
// type Expression interface {
//     Node
//     expressionNode()
// }

// trait Node {
//     fn token_literal() -> String {}
// }

// trait Statement {
//     todo!();
// }


// pub enum Node {
//     Node,
//     Statement,
//     Expression
// }


#[derive(Debug)]
pub enum Expression {
    Ident(String),
    Plus,
    Integer(u64),
}

#[derive(Debug)]
pub enum Statement {
    Let(String),
    Return,
    ExpressionStmt(Expression),
}

pub type Program = Vec<Statement>;
