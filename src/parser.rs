use crate::ast::ast::{Block, Expression, Infix, Prefix, Program, Statement};
use crate::lexer::Lexer;
use crate::token::Token;
use anyhow::{anyhow, Result};

#[derive(PartialEq, PartialOrd, Debug)]
enum Precedence {
    Lowest = 1,
    Equals = 2,      // ==
    Lessgreater = 3, // > or <
    Sum = 4,         // +
    Product = 5,     // *
    Prefix = 6,      // -X or !X
    Call = 7,        // myFunction(X)
    Index = 8,       // array[index]
}

#[derive(Debug)]
pub struct Parser {
    lexer: Lexer,
    curr_token: Option<Token>,
    peek_token: Option<Token>,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
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

    pub fn parse_program(&mut self) -> Program {
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
        let ident = match &self.peek_token {
            Some(Token::Ident(ident)) => ident.clone(),
            Some(token) => {
                return Err(anyhow!(
                    "Expected token: {:?}, Found: {:?}",
                    Token::Ident(String::from("something")),
                    token
                ))
            }
            None => {
                return Err(anyhow!(
                    "Expected token: {:?}, Found nothing",
                    Token::Ident(String::from("something"))
                ))
            }
        };

        self.next_token();

        if self.peek_token != Some(Token::Assign) {
            return Err(anyhow!(
                "Expected token: {:?}, Found: {:?}",
                Token::Assign,
                self.peek_token
            ));
        }
        self.next_token();
        self.next_token();

        let value = self.parse_expression(Precedence::Lowest)?;

        if self.peek_token == Some(Token::Semicolon) {
            self.next_token();
        }

        Ok(Statement::Let(ident, value))
    }

    fn parse_return_statement(&mut self) -> Result<Statement> {
        self.next_token();

        let value = self.parse_expression(Precedence::Lowest)?;

        if self.peek_token == Some(Token::Semicolon) {
            self.next_token();
        }

        Ok(Statement::Return(value))
    }

    fn parse_expression_statement(&mut self) -> Result<Statement> {
        let expr = self.parse_expression(Precedence::Lowest)?;

        if self.peek_token == Some(Token::Semicolon) {
            self.next_token();
        }

        Ok(Statement::ExpressionStmt(expr))
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression> {
        let mut left = match &self.curr_token {
            Some(Token::Ident(ident)) => Expression::Ident(String::from(ident)),
            Some(Token::Int(num)) => Expression::IntLiteral(*num),
            Some(Token::String(s)) => Expression::StringLiteral(s.clone()),
            Some(Token::True) => Expression::Boolean(true),
            Some(Token::False) => Expression::Boolean(false),
            Some(Token::Lparen) => self.parse_grouped_expression()?,
            Some(Token::Bang) | Some(Token::Minus) => self.parse_prefix_expression()?,
            Some(Token::If) => self.parse_if_expression()?,
            Some(Token::Function) => self.parse_fn_literal_expression()?,
            Some(Token::Lbracket) => self.parse_array_literal()?,
            Some(Token::Lbrace) => self.parse_hash_literal()?,
            _ => todo!(),
        };

        while self.peek_token != Some(Token::Semicolon)
            && precedence < get_precedence(&self.peek_token.clone().unwrap())
        {
            self.next_token();

            left = match &self.curr_token {
                Some(Token::Lparen) => self.parse_call_expression(left)?,
                Some(Token::Lbracket) => self.parse_index_expression(left)?,
                _ => self.parse_infix_expression(left)?,
            }
        }

        Ok(left)
    }

    fn parse_prefix_expression(&mut self) -> Result<Expression> {
        let curr_token = self.curr_token.clone();

        self.next_token();

        let right = self.parse_expression(Precedence::Prefix)?;
        match curr_token {
            Some(Token::Bang) => Ok(Expression::PrefixExpr(Prefix::Bang, Box::from(right))),
            Some(Token::Minus) => Ok(Expression::PrefixExpr(Prefix::Minus, Box::from(right))),
            _ => unreachable!(),
        }
    }

    fn parse_grouped_expression(&mut self) -> Result<Expression> {
        self.next_token();

        let expression = self.parse_expression(Precedence::Lowest);

        if self.peek_token != Some(Token::Rparen) {
            return Err(anyhow!(
                "Expected token: {:?}, Found: {:?}",
                Token::Rparen,
                self.peek_token
            ));
        }

        self.next_token();

        expression
    }

    fn parse_if_expression(&mut self) -> Result<Expression> {
        if self.peek_token != Some(Token::Lparen) {
            return Err(anyhow!(
                "Expected token: {:?}, Found: {:?}",
                Token::Lparen,
                self.peek_token
            ));
        }
        self.next_token();
        self.next_token();

        let condition = self.parse_expression(Precedence::Lowest)?;

        if self.peek_token != Some(Token::Rparen) {
            return Err(anyhow!(
                "Expected token: {:?}, Found: {:?}",
                Token::Rparen,
                self.peek_token
            ));
        }
        self.next_token();

        if self.peek_token != Some(Token::Lbrace) {
            return Err(anyhow!(
                "Expected token: {:?}, Found: {:?}",
                Token::Lbrace,
                self.peek_token
            ));
        }
        self.next_token();

        let consequence = self.parse_block_statement()?;
        let mut auternative = vec![];

        if self.peek_token == Some(Token::Else) {
            self.next_token();

            if self.peek_token != Some(Token::Lbrace) {
                return Err(anyhow!(
                    "Expected token: {:?}, Found: {:?}",
                    Token::Lbrace,
                    self.peek_token
                ));
            }
            self.next_token();

            auternative = self.parse_block_statement()?;
        }

        Ok(Expression::IfExpr(
            Box::from(condition),
            consequence,
            auternative,
        ))
    }

    fn parse_fn_literal_expression(&mut self) -> Result<Expression> {
        if self.peek_token != Some(Token::Lparen) {
            return Err(anyhow!(
                "Expected token: {:?}, Found: {:?}",
                Token::Lparen,
                self.peek_token
            ));
        }
        self.next_token();

        let parameters = self.parse_fn_parameters()?;

        if self.peek_token != Some(Token::Lbrace) {
            return Err(anyhow!(
                "Expected token: {:?}, Found: {:?}",
                Token::Lbrace,
                self.peek_token
            ));
        }
        self.next_token();

        let body = self.parse_block_statement()?;
        Ok(Expression::FnLiteral(parameters, body))
    }

    fn parse_fn_parameters(&mut self) -> Result<Vec<Expression>> {
        let mut idents = vec![];

        if self.peek_token == Some(Token::Rparen) {
            self.next_token();
            return Ok(idents);
        }
        self.next_token();

        match &self.curr_token {
            Some(Token::Ident(ident)) => idents.push(Expression::Ident(ident.clone())),
            _ => {
                return Err(anyhow!(
                    "Expected token of type identifier inside function parameters, found: {:?}",
                    self.curr_token
                ))
            }
        }

        while self.peek_token == Some(Token::Comma) {
            self.next_token();
            self.next_token();

            match &self.curr_token {
                Some(Token::Ident(ident)) => idents.push(Expression::Ident(ident.clone())),
                _ => {
                    return Err(anyhow!(
                        "Expected token of type identifier inside function parameters, found: {:?}",
                        self.curr_token
                    ))
                }
            }
        }

        if self.peek_token != Some(Token::Rparen) {
            return Err(anyhow!(
                "Expected token: {:?}, Found: {:?}",
                Token::Rparen,
                self.peek_token
            ));
        }
        self.next_token();

        Ok(idents)
    }

    fn parse_block_statement(&mut self) -> Result<Block> {
        self.next_token();

        let mut statements = vec![];
        while self.curr_token != Some(Token::Rbrace) && self.curr_token != Some(Token::Eof) {
            let stmt = self.parse_statement()?;
            statements.push(stmt);
            self.next_token();
        }

        Ok(statements)
    }

    fn parse_call_expression(&mut self, function: Expression) -> Result<Expression> {
        let arguments = self.parse_expression_list(Token::Rparen)?;

        Ok(Expression::Call(Box::from(function), arguments))
    }

    fn parse_index_expression(&mut self, left: Expression) -> Result<Expression> {
        self.next_token();

        let index = self.parse_expression(Precedence::Lowest)?;

        if self.peek_token != Some(Token::Rbracket) {
            return Err(anyhow!(
                "Expected token: {:?}, Found: {:?}",
                Token::Rbracket,
                self.peek_token
            ));
        }
        self.next_token();

        Ok(Expression::IndexExpression(
            Box::from(left),
            Box::from(index),
        ))
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Result<Expression> {
        let curr_token = self.curr_token.clone();
        let precedence = get_precedence(&curr_token.clone().unwrap());
        self.next_token();

        let right = self.parse_expression(precedence)?;
        match &curr_token {
            Some(Token::Plus) => Ok(Expression::InfixExpr(
                Infix::Plus,
                Box::from(left),
                Box::from(right),
            )),
            Some(Token::Minus) => Ok(Expression::InfixExpr(
                Infix::Minus,
                Box::from(left),
                Box::from(right),
            )),
            Some(Token::Slash) => Ok(Expression::InfixExpr(
                Infix::Slash,
                Box::from(left),
                Box::from(right),
            )),
            Some(Token::Asterisk) => Ok(Expression::InfixExpr(
                Infix::Asterisk,
                Box::from(left),
                Box::from(right),
            )),
            Some(Token::Gt) => Ok(Expression::InfixExpr(
                Infix::Gt,
                Box::from(left),
                Box::from(right),
            )),
            Some(Token::Lt) => Ok(Expression::InfixExpr(
                Infix::Lt,
                Box::from(left),
                Box::from(right),
            )),
            Some(Token::Eq) => Ok(Expression::InfixExpr(
                Infix::Eq,
                Box::from(left),
                Box::from(right),
            )),
            Some(Token::Neq) => Ok(Expression::InfixExpr(
                Infix::Neq,
                Box::from(left),
                Box::from(right),
            )),
            _ => unreachable!(),
        }
    }

    fn parse_array_literal(&mut self) -> Result<Expression> {
        let elements = self.parse_expression_list(Token::Rbracket)?;

        Ok(Expression::ArrayLiteral(elements))
    }

    fn parse_hash_literal(&mut self) -> Result<Expression> {
        let mut res = vec![];

        while self.peek_token != Some(Token::Rbrace) {
            self.next_token();
            let key = self.parse_expression(Precedence::Lowest)?;

            if self.peek_token != Some(Token::Colon) {
                return Err(anyhow!(
                    "Expected token: {:?}, Found: {:?}",
                    Token::Colon,
                    self.peek_token
                ));
            }
            self.next_token();
            self.next_token();

            let value = self.parse_expression(Precedence::Lowest)?;

            res.push((key, value));

            if self.peek_token != Some(Token::Rbrace) && self.peek_token != Some(Token::Comma) {
                return Err(anyhow!(
                    "Expected token: {:?}, Found: {:?}",
                    Token::Comma,
                    self.peek_token
                ));
            }
            if self.peek_token == Some(Token::Comma) {
                self.next_token();
            }
        }

        if self.peek_token != Some(Token::Rbrace) {
            return Err(anyhow!(
                "Expected token: {:?}, Found: {:?}",
                Token::Colon,
                self.peek_token
            ));
        }
        self.next_token();

        Ok(Expression::HashLiteral(res))
    }

    fn parse_expression_list(&mut self, end: Token) -> Result<Vec<Expression>> {
        let mut result = vec![];

        if self.peek_token == Some(end.clone()) {
            self.next_token();
            return Ok(result);
        }

        self.next_token();
        result.push(self.parse_expression(Precedence::Lowest)?);

        while self.peek_token == Some(Token::Comma) {
            self.next_token();
            self.next_token();
            result.push(self.parse_expression(Precedence::Lowest)?);
        }

        if self.peek_token != Some(end.clone()) {
            return Err(anyhow!(
                "Expected token: {:?}, Found: {:?}",
                end,
                self.peek_token
            ));
        }

        self.next_token();

        Ok(result)
    }
}

fn get_precedence(tok: &Token) -> Precedence {
    match tok {
        Token::Lparen => Precedence::Call,
        Token::Lbracket => Precedence::Index,
        Token::Eq | Token::Neq => Precedence::Equals,
        Token::Lt | Token::Gt => Precedence::Lessgreater,
        Token::Plus | Token::Minus => Precedence::Sum,
        Token::Asterisk | Token::Slash => Precedence::Product,
        _ => Precedence::Lowest,
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::ast::Statement;

    use super::*;

    #[test]
    fn test_let_statements() {
        let input = String::from(
            "let x = 5;
            let y = 10;
            let foobar = 838383;",
        );

        let expected_statements = [
            Statement::Let(String::from("x"), Expression::IntLiteral(5)),
            Statement::Let(String::from("y"), Expression::IntLiteral(10)),
            Statement::Let(String::from("foobar"), Expression::IntLiteral(838383)),
        ];

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        if program.len() != expected_statements.len() {
            panic!(
                "program statemens doesn't contain {} elements, got {}",
                expected_statements.len(),
                program.len()
            );
        }

        for i in 0..expected_statements.len() {
            assert_eq!(program[i], expected_statements[i]);
        }
    }

    #[test]
    fn test_return_statements() {
        let input = String::from(
            "return 5;
            return 10;
            return 993322;
            return 5+6;",
        );

        let expected_statements = [
            Statement::Return(Expression::IntLiteral(5)),
            Statement::Return(Expression::IntLiteral(10)),
            Statement::Return(Expression::IntLiteral(993322)),
            Statement::Return(Expression::InfixExpr(
                Infix::Plus,
                Box::from(Expression::IntLiteral(5)),
                Box::from(Expression::IntLiteral(6)),
            )),
        ];

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        if program.len() != expected_statements.len() {
            panic!(
                "program statemens doesn't contain {} elements, got {}",
                expected_statements.len(),
                program.len()
            );
        }

        for i in 0..expected_statements.len() {
            assert_eq!(program[i], expected_statements[i]);
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
            Statement::ExpressionStmt(Expression::IntLiteral(num)) => assert_eq!(num, &5),
            _ => assert!(false),
        }
    }

    #[test]
    fn test_prefix_expression() {
        let input = String::from(
            "!5;
            -15;
            !7;",
        );

        let expected_expressions = [
            Expression::PrefixExpr(Prefix::Bang, Box::from(Expression::IntLiteral(5))),
            Expression::PrefixExpr(Prefix::Minus, Box::from(Expression::IntLiteral(15))),
            Expression::PrefixExpr(Prefix::Bang, Box::from(Expression::IntLiteral(7))),
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

    #[test]
    fn test_infix_expression() {
        let input = String::from(
            "1+5;
            1-5;
            1*5;
            1/5;
            1>5;
            1<5;
            1==5;
            1!=5;",
        );

        let expected_expressions = [
            Expression::InfixExpr(
                Infix::Plus,
                Box::from(Expression::IntLiteral(1)),
                Box::from(Expression::IntLiteral(5)),
            ),
            Expression::InfixExpr(
                Infix::Minus,
                Box::from(Expression::IntLiteral(1)),
                Box::from(Expression::IntLiteral(5)),
            ),
            Expression::InfixExpr(
                Infix::Asterisk,
                Box::from(Expression::IntLiteral(1)),
                Box::from(Expression::IntLiteral(5)),
            ),
            Expression::InfixExpr(
                Infix::Slash,
                Box::from(Expression::IntLiteral(1)),
                Box::from(Expression::IntLiteral(5)),
            ),
            Expression::InfixExpr(
                Infix::Gt,
                Box::from(Expression::IntLiteral(1)),
                Box::from(Expression::IntLiteral(5)),
            ),
            Expression::InfixExpr(
                Infix::Lt,
                Box::from(Expression::IntLiteral(1)),
                Box::from(Expression::IntLiteral(5)),
            ),
            Expression::InfixExpr(
                Infix::Eq,
                Box::from(Expression::IntLiteral(1)),
                Box::from(Expression::IntLiteral(5)),
            ),
            Expression::InfixExpr(
                Infix::Neq,
                Box::from(Expression::IntLiteral(1)),
                Box::from(Expression::IntLiteral(5)),
            ),
        ];

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        if program.len() != 8 {
            panic!(
                "program statemens doesn't contain 8 elements, got {}",
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

    #[test]
    fn test_complex_expression() {
        let input = String::from(
            "1+5+7;
            1-5/6;
            1*5+2;
            1*-5+2;",
        );

        let expected_expressions = [
            Expression::InfixExpr(
                Infix::Plus,
                Box::from(Expression::InfixExpr(
                    Infix::Plus,
                    Box::from(Expression::IntLiteral(1)),
                    Box::from(Expression::IntLiteral(5)),
                )),
                Box::from(Expression::IntLiteral(7)),
            ),
            Expression::InfixExpr(
                Infix::Minus,
                Box::from(Expression::IntLiteral(1)),
                Box::from(Expression::InfixExpr(
                    Infix::Slash,
                    Box::from(Expression::IntLiteral(5)),
                    Box::from(Expression::IntLiteral(6)),
                )),
            ),
            Expression::InfixExpr(
                Infix::Plus,
                Box::from(Expression::InfixExpr(
                    Infix::Asterisk,
                    Box::from(Expression::IntLiteral(1)),
                    Box::from(Expression::IntLiteral(5)),
                )),
                Box::from(Expression::IntLiteral(2)),
            ),
            Expression::InfixExpr(
                Infix::Plus,
                Box::from(Expression::InfixExpr(
                    Infix::Asterisk,
                    Box::from(Expression::IntLiteral(1)),
                    Box::from(Expression::PrefixExpr(
                        Prefix::Minus,
                        Box::from(Expression::IntLiteral(5)),
                    )),
                )),
                Box::from(Expression::IntLiteral(2)),
            ),
        ];

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        if program.len() != expected_expressions.len() {
            panic!(
                "program statemens doesn't contain {} elements, got {}",
                expected_expressions.len(),
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

    #[test]
    fn test_boolean_expression() {
        let input = String::from(
            "true;
            false;
            3 > 5 == false;
            3 < 5 != true;",
        );

        let expected_expressions = [
            Expression::Boolean(true),
            Expression::Boolean(false),
            Expression::InfixExpr(
                Infix::Eq,
                Box::from(Expression::InfixExpr(
                    Infix::Gt,
                    Box::from(Expression::IntLiteral(3)),
                    Box::from(Expression::IntLiteral(5)),
                )),
                Box::from(Expression::Boolean(false)),
            ),
            Expression::InfixExpr(
                Infix::Neq,
                Box::from(Expression::InfixExpr(
                    Infix::Lt,
                    Box::from(Expression::IntLiteral(3)),
                    Box::from(Expression::IntLiteral(5)),
                )),
                Box::from(Expression::Boolean(true)),
            ),
        ];

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        if program.len() != expected_expressions.len() {
            panic!(
                "program statemens doesn't contain {} elements, got {}",
                expected_expressions.len(),
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

    #[test]
    fn test_precedence_expression() {
        let input = String::from(
            "
            1 + (2 + 3) + 4;
            1 + ((2 + 3) + 4);
            1 + (2 + 3 + 4) + 5;
            -(5 + 6);
        ",
        );

        let expected_expressions = [
            Expression::InfixExpr(
                Infix::Plus,
                Box::from(Expression::InfixExpr(
                    Infix::Plus,
                    Box::from(Expression::IntLiteral(1)),
                    Box::from(Expression::InfixExpr(
                        Infix::Plus,
                        Box::from(Expression::IntLiteral(2)),
                        Box::from(Expression::IntLiteral(3)),
                    )),
                )),
                Box::from(Expression::IntLiteral(4)),
            ),
            Expression::InfixExpr(
                Infix::Plus,
                Box::from(Expression::IntLiteral(1)),
                Box::from(Expression::InfixExpr(
                    Infix::Plus,
                    Box::from(Expression::InfixExpr(
                        Infix::Plus,
                        Box::from(Expression::IntLiteral(2)),
                        Box::from(Expression::IntLiteral(3)),
                    )),
                    Box::from(Expression::IntLiteral(4)),
                )),
            ),
            Expression::InfixExpr(
                Infix::Plus,
                Box::from(Expression::InfixExpr(
                    Infix::Plus,
                    Box::from(Expression::IntLiteral(1)),
                    Box::from(Expression::InfixExpr(
                        Infix::Plus,
                        Box::from(Expression::InfixExpr(
                            Infix::Plus,
                            Box::from(Expression::IntLiteral(2)),
                            Box::from(Expression::IntLiteral(3)),
                        )),
                        Box::from(Expression::IntLiteral(4)),
                    )),
                )),
                Box::from(Expression::IntLiteral(5)),
            ),
            Expression::PrefixExpr(
                Prefix::Minus,
                Box::from(Expression::InfixExpr(
                    Infix::Plus,
                    Box::from(Expression::IntLiteral(5)),
                    Box::from(Expression::IntLiteral(6)),
                )),
            ),
        ];

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        if program.len() != expected_expressions.len() {
            panic!(
                "program statemens doesn't contain {} elements, got {}",
                expected_expressions.len(),
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

    #[test]
    fn test_if_expression() {
        let input = String::from(
            "if (x < y) { x; };
            if (x < y) { x } else { y };",
        );

        let expected_expressions = [
            Expression::IfExpr(
                Box::from(Expression::InfixExpr(
                    Infix::Lt,
                    Box::from(Expression::Ident(String::from("x"))),
                    Box::from(Expression::Ident(String::from("y"))),
                )),
                vec![Statement::ExpressionStmt(Expression::Ident(String::from(
                    "x",
                )))],
                vec![],
            ),
            Expression::IfExpr(
                Box::from(Expression::InfixExpr(
                    Infix::Lt,
                    Box::from(Expression::Ident(String::from("x"))),
                    Box::from(Expression::Ident(String::from("y"))),
                )),
                vec![Statement::ExpressionStmt(Expression::Ident(String::from(
                    "x",
                )))],
                vec![Statement::ExpressionStmt(Expression::Ident(String::from(
                    "y",
                )))],
            ),
        ];

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        if program.len() != expected_expressions.len() {
            panic!(
                "program statemens doesn't contain {} elements, got {}",
                expected_expressions.len(),
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

    #[test]
    fn test_fn_expression() {
        let input = String::from("fn(x, y) { x + y };");

        let expected_expressions = [Expression::FnLiteral(
            vec![
                Expression::Ident(String::from("x")),
                Expression::Ident(String::from("y")),
            ],
            vec![Statement::ExpressionStmt(Expression::InfixExpr(
                Infix::Plus,
                Box::from(Expression::Ident(String::from("x"))),
                Box::from(Expression::Ident(String::from("y"))),
            ))],
        )];

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        if program.len() != expected_expressions.len() {
            panic!(
                "program statemens doesn't contain {} elements, got {}",
                expected_expressions.len(),
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

    #[test]
    fn test_fn_parameter_parsing_expression() {
        let input = String::from(
            "fn() {};
            fn(x) {};
            fn(x, y, z) {};",
        );

        let expected_expressions = [
            Expression::FnLiteral(vec![], vec![]),
            Expression::FnLiteral(vec![Expression::Ident(String::from("x"))], vec![]),
            Expression::FnLiteral(
                vec![
                    Expression::Ident(String::from("x")),
                    Expression::Ident(String::from("y")),
                    Expression::Ident(String::from("z")),
                ],
                vec![],
            ),
        ];

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        if program.len() != expected_expressions.len() {
            panic!(
                "program statemens doesn't contain {} elements, got {}",
                expected_expressions.len(),
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

    #[test]
    fn test_call_parsing_expression() {
        let input = String::from("add(1, 2 * 3, 4 + 5);");

        let expected_expressions = [Expression::Call(
            Box::from(Expression::Ident(String::from("add"))),
            vec![
                Expression::IntLiteral(1),
                Expression::InfixExpr(
                    Infix::Asterisk,
                    Box::from(Expression::IntLiteral(2)),
                    Box::from(Expression::IntLiteral(3)),
                ),
                Expression::InfixExpr(
                    Infix::Plus,
                    Box::from(Expression::IntLiteral(4)),
                    Box::from(Expression::IntLiteral(5)),
                ),
            ],
        )];

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        if program.len() != expected_expressions.len() {
            panic!(
                "program statemens doesn't contain {} elements, got {}",
                expected_expressions.len(),
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

    #[test]
    fn test_operator_precendence_parsing() {
        let input = String::from(
            "a + add(b * c) + d;
            add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8));
            add(a + b + c * d / f + g);
            a * [1, 2, 3, 4][b * c] * d;
            add(a * b[2], b[1], 2 * [1, 2][1]);",
        );

        let expected_result = String::from(
            "((a + add((b * c))) + d);
            add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)));
            add((((a + b) + ((c * d) / f)) + g));
            ((a * ([1, 2, 3, 4][(b * c)])) * d);
            add((a * (b[2])), (b[1]), (2 * ([1, 2][1])));",
        );

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        let lexer2 = Lexer::new(expected_result);
        let mut parser2 = Parser::new(lexer2);
        let program2 = parser2.parse_program();

        if program.len() != program2.len() {
            panic!(
                "program statemens doesn't contain {} elements, got {}",
                program2.len(),
                program.len()
            );
        }

        for i in 0..program.len() {
            assert_eq!(program[i], program2[i])
        }
    }

    #[test]
    fn test_string_literal_expression() {
        let input = String::from(r#""hello world";"#);

        let expected_expressions = [Expression::StringLiteral(String::from("hello world"))];

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        if program.len() != expected_expressions.len() {
            panic!(
                "program statemens doesn't contain {} elements, got {}",
                expected_expressions.len(),
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

    #[test]
    fn test_parse_array_literal() {
        let input = String::from(
            r#"
            [1, 2*3, 4 + 5]
            "#,
        );

        let expected_expressions = [Expression::ArrayLiteral(vec![
            Expression::IntLiteral(1),
            Expression::InfixExpr(
                Infix::Asterisk,
                Box::from(Expression::IntLiteral(2)),
                Box::from(Expression::IntLiteral(3)),
            ),
            Expression::InfixExpr(
                Infix::Plus,
                Box::from(Expression::IntLiteral(4)),
                Box::from(Expression::IntLiteral(5)),
            ),
        ])];

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        if program.len() != expected_expressions.len() {
            panic!(
                "program statemens doesn't contain {} elements, got {}",
                expected_expressions.len(),
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

    #[test]
    fn test_parse_index_expression() {
        let input = String::from(
            r#"
            myArray[4+5];
            "#,
        );

        let expected_expressions = [Expression::IndexExpression(
            Box::from(Expression::Ident(String::from("myArray"))),
            Box::from(Expression::InfixExpr(
                Infix::Plus,
                Box::from(Expression::IntLiteral(4)),
                Box::from(Expression::IntLiteral(5)),
            )),
        )];

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        if program.len() != expected_expressions.len() {
            panic!(
                "program statemens doesn't contain {} elements, got {}",
                expected_expressions.len(),
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

    #[test]
    fn test_parse_hash_literal() {
        let input = String::from(
            r#"
            {"one": 1, "two": 2, "three": 3};
            "#,
        );

        let expected_expressions = [Expression::HashLiteral(vec![
            (
                Expression::StringLiteral(String::from("one")),
                Expression::IntLiteral(1),
            ),
            (
                Expression::StringLiteral(String::from("two")),
                Expression::IntLiteral(2),
            ),
            (
                Expression::StringLiteral(String::from("three")),
                Expression::IntLiteral(3),
            ),
        ])];

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        if program.len() != expected_expressions.len() {
            panic!(
                "program statemens doesn't contain {} elements, got {}",
                expected_expressions.len(),
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

    #[test]
    fn test_parse_empty_hash_literal() {
        let input = String::from(
            r#"
            {};
            "#,
        );

        let expected_expressions = [Expression::HashLiteral(vec![])];

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        if program.len() != expected_expressions.len() {
            panic!(
                "program statemens doesn't contain {} elements, got {}",
                expected_expressions.len(),
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

    #[test]
    fn test_parse_hash_literal_with_expression() {
        let input = String::from(
            r#"
            {"one": 0 + 1, "two": 10 - 8, "three": 15 / 5};
            "#,
        );

        let expected_expressions = [Expression::HashLiteral(vec![
            (
                Expression::StringLiteral(String::from("one")),
                Expression::InfixExpr(
                    Infix::Plus,
                    Box::from(Expression::IntLiteral(0)),
                    Box::from(Expression::IntLiteral(1)),
                ),
            ),
            (
                Expression::StringLiteral(String::from("two")),
                Expression::InfixExpr(
                    Infix::Minus,
                    Box::from(Expression::IntLiteral(10)),
                    Box::from(Expression::IntLiteral(8)),
                ),
            ),
            (
                Expression::StringLiteral(String::from("three")),
                Expression::InfixExpr(
                    Infix::Slash,
                    Box::from(Expression::IntLiteral(15)),
                    Box::from(Expression::IntLiteral(5)),
                ),
            ),
        ])];

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        if program.len() != expected_expressions.len() {
            panic!(
                "program statemens doesn't contain {} elements, got {}",
                expected_expressions.len(),
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
