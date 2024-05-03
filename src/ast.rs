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


pub enum Expression {
    Plus
}

#[derive(Debug)]
pub enum Statement {
    Let(String),
}

pub type Program = Vec<Statement>;


// fn token_literal(program: Program) -> String {
//     if program.len() > 0 {
//         program[0];
//         todo!()
//     } else {
//         String::default()
//     }
// }




    // func (p *Program) TokenLiteral() string {
    // if len(p.Statements) > 0 {
    // return p.Statements[0].TokenLiteral()
    // } else {
    // return ""
    // }
    // }