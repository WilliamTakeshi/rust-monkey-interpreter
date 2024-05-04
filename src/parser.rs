use crate::ast::{Expression, Prefix, Program, Statement};
use crate::lexer::Lexer;
use crate::token::Token;
use anyhow::{anyhow, Result};

enum Priority {
    Lowest = 1,
    Equals = 2,      // ==
    Lessgreater = 3, // > or <
    Sum = 4,         // +
    Product = 5,     // *
    Prefix = 6,      // -X or !X
    Call = 7,        // myFunction(X)
}

#[derive(Debug)]
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
                }
            }

            self.next_token();
        }
        program
    }

    fn parse_statement(&mut self) -> Result<Statement> {
        match self.curr_token {
            Some(Token::Let) => self.parse_let_statement(),
            Some(Token::Return) => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_let_statement(&mut self) -> Result<Statement> {
        let statement = match &self.peek_token {
            Some(Token::Ident(ident)) => Ok(Statement::Let(ident.clone())),
            Some(token) => Err(anyhow!(
                "Expected token: {:?}, Found: {:?}",
                Token::Ident(String::from("something")),
                token
            )),
            None => Err(anyhow!(
                "Expected token: {:?}, Found nothing",
                Token::Ident(String::from("something"))
            )),
        };

        self.next_token();

        if self.peek_token != Some(Token::Assign) {
            return Err(anyhow!(
                "Expected token: {:?}, Found: {:?}",
                Token::Assign,
                self.peek_token
            ));
        }

        // TODO: Skip until Semicolon
        while self.curr_token != Some(Token::Semicolon) {
            self.next_token();
        }

        statement
    }

    fn parse_return_statement(&mut self) -> Result<Statement> {
        let statement = match &self.peek_token {
            Some(_) => Ok(Statement::Return),
            None => Err(anyhow!(
                "Expected token: {:?}, Found nothing",
                Token::Ident(String::from("something"))
            )),
        };

        self.next_token();

        // TODO: Skip until Semicolon
        while self.curr_token != Some(Token::Semicolon) {
            self.next_token();
        }

        statement
    }

    fn parse_expression_statement(&mut self) -> Result<Statement> {
        let statement = match &self.curr_token {
            Some(Token::Ident(_)) | Some(Token::Int(_)) => {
                let expr = self.parse_expression(Priority::Lowest)?;
                Ok(Statement::ExpressionStmt(expr))
            }
            Some(Token::Bang) | Some(Token::Minus) => {
                let expr = self.parse_prefix_expression()?;
                Ok(Statement::ExpressionStmt(expr))
            }
            None => Err(anyhow!(
                "Expected token: {:?}, Found nothing",
                Token::Ident(String::from("something"))
            )),
            _ => todo!(),
        };

        self.next_token();

        // TODO: Skip until Semicolon
        while self.curr_token != Some(Token::Semicolon) {
            self.next_token();
        }

        statement
    }

    fn parse_expression(&mut self, _priority: Priority) -> Result<Expression> {
        let prefix = match &self.curr_token {
            Some(Token::Ident(ident)) => Expression::Ident(String::from(ident)),
            Some(Token::Int(num)) => Expression::Literal(*num),
            _ => todo!(),
        };

        Ok(prefix)
    }

    fn parse_prefix_expression(&mut self) -> Result<Expression> {
        let curr_token = self.curr_token.clone();

        self.next_token();

        let right = self.parse_expression(Priority::Prefix)?;
        match curr_token {
            Some(Token::Bang) => Ok(Expression::PrefixExpr(Prefix::Bang, Box::from(right))),
            Some(Token::Minus) => Ok(Expression::PrefixExpr(Prefix::Minus, Box::from(right))),
            _ => unreachable!(),
        }
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

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        if program.len() != 3 {
            panic!(
                "program statemens doesn't contain 3 elements, got {}",
                program.len()
            );
        }

        let expected_ident = [String::from("x"), String::from("y"), String::from("foobar")];

        for (i, ident) in expected_ident.iter().enumerate() {
            let statement = &program[i];
            match statement {
                Statement::Let(identifier) => assert_eq!(identifier, ident),
                _ => unreachable!(),
            }
        }
    }

    #[test]
    fn test_return_statements() {
        let input = String::from(
            "return 5;
            return 10;
            return 993322;",
        );

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        if program.len() != 3 {
            panic!(
                "program statemens doesn't contain 3 elements, got {}",
                program.len()
            );
        }

        for i in 0..3 {
            let statement = &program[i];
            match statement {
                Statement::Return => assert!(true),
                _ => unreachable!(),
            }
        }
    }

    #[test]
    fn test_ident_expression() {
        let input = String::from("foobar;");

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        if program.len() != 1 {
            panic!(
                "program statemens doesn't contain 1 elements, got {}",
                program.len()
            );
        }

        match &program[0] {
            Statement::ExpressionStmt(Expression::Ident(ident)) => {
                assert_eq!(ident, &String::from("foobar"))
            }
            _ => assert!(false),
        }
    }

    #[test]
    fn test_int_literal_expression() {
        let input = String::from("5;");

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        if program.len() != 1 {
            panic!(
                "program statemens doesn't contain 1 elements, got {}",
                program.len()
            );
        }

        match &program[0] {
            Statement::ExpressionStmt(Expression::Literal(num)) => assert_eq!(num, &5),
            _ => assert!(false),
        }
    }

    #[test]
    fn test_prefix_expression() {
        let input = String::from(
            "
            !5;
            -15;
            !7;
        ",
        );

        let expected_expressions = [
            Expression::PrefixExpr(Prefix::Bang, Box::from(Expression::Literal(5))),
            Expression::PrefixExpr(Prefix::Minus, Box::from(Expression::Literal(15))),
            Expression::PrefixExpr(Prefix::Bang, Box::from(Expression::Literal(7))),
        ];

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        if program.len() != 3 {
            panic!(
                "program statemens doesn't contain 3 elements, got {}",
                program.len()
            );
        }

        for (idx, expected_expr) in expected_expressions.iter().enumerate() {
            match &program[idx] {
                Statement::ExpressionStmt(expr) => {
                    assert_eq!(expr, expected_expr)
                }
                _ => assert!(false),
            }
        }
    }
}
