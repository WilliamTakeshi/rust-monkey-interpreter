use crate::ast::{Program, Statement};
use crate::lexer::Lexer;
use crate::token::Token;
use anyhow::{anyhow, Result};

struct Parser {
    lexer: Lexer,
    curr_token: Option<Token>,
    peek_token: Option<Token>,
}

impl Parser {
    fn new(lexer: Lexer) -> Self {
        let mut parser = Parser {
            lexer,
            curr_token: None,
            peek_token: None,
        };

        parser.next_token();
        parser.next_token();

        parser
    }

    fn next_token(&mut self) {
        self.curr_token = self.peek_token.clone();
        self.peek_token = Some(self.lexer.next_token());
    }

    fn parse_program(&mut self) -> Program {
        let mut program: Program = vec![];

        while self.curr_token != Some(Token::Eof) {
            match self.parse_statement() {
                Ok(statement) => program.push(statement),
                Err(error) => {
                    dbg!(&error);
                    ()
                },
            }

            self.next_token();
        }
        program
    }

    fn parse_statement(&mut self) -> Result<Statement> {
        match self.curr_token {
            Some(Token::Let) => self.parse_let_statement(),
            _ => todo!()
        }
    }

    fn parse_let_statement(&mut self) -> Result<Statement> {
        let statement = match &self.peek_token {
            Some(Token::Ident(ident)) => Ok(Statement::Let(ident.clone())),
            Some(token) => Err(anyhow!("Expected token: {:?}, Found: {:?}", Token::Ident(String::from("something")), token)),
            None => Err(anyhow!("Expected token: {:?}, Found nothing", Token::Ident(String::from("something")))),
        };


        self.next_token();

        if self.peek_token != Some(Token::Assign) {
            return Err(anyhow!("Expected token: {:?}, Found: {:?}", Token::Assign, self.peek_token));
        }

        // TODO: Skip until Semicolon 
        while self.curr_token != Some(Token::Semicolon) {
            self.next_token();
        }


        statement
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::Statement;

    use super::*;

    #[test]
    fn test_let_statements() {
        let input = String::from(
            "let x = 5;
            let y = 10;
            let foobar = 838383;",
        );

        let mut lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        if program.len() == 0 {
            panic!("parse_program returned []");
        }


        if program.len() != 3 {
            panic!("program statemens doesn't contain 3 elements, got {}", program.len());
        }

        let expected_ident = [String::from("x"), String::from("y"), String::from("foobar")];

        for (i ,ident) in expected_ident.iter().enumerate() {
            let statement = &program[i];
            match statement {
                Statement::Let(identifier) => assert_eq!(identifier, ident),
                _ => unreachable!()
            }
        } 
    }
}
