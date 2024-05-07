use std::os::linux::raw::stat;

use crate::ast::{self, Expression, Program};
use crate::object::Object;
// use crate::token::Token;

pub struct Evaluator {}

impl Evaluator {
    pub fn new() -> Self {
        Self {}
    }

    pub fn eval_program(&mut self, program: Program) -> Object {
        match program.len() {
            0 => Object::Null,
            _ => self.eval_statements(program),
        }
    }

    fn eval_statements(&mut self, statements: Vec<ast::Statement>) -> Object {
        let mut result = Object::Null;
        for statement in statements {
            result = self.eval_statement(statement)
        }

        result
    }

    fn eval_statement(&mut self, statement: ast::Statement) -> Object {
        match statement {
            ast::Statement::ExpressionStmt(expr) => self.eval_expr(expr),
            ast::Statement::Let(_, _) => todo!(),
            ast::Statement::Return(_) => todo!(),
        }
    }

    fn eval_expr(&mut self, expr: ast::Expression) -> Object {
        match expr {
            ast::Expression::Literal(num) => Object::Integer(num as i64),
            // Ident(String) =>
            // Boolean(bool) =>
            // PrefixExpr(Prefix, Box<Expression>) =>
            // InfixExpr(Infix, Box<Expression>, Box<Expression>) =>
            // IfExpr(Box<Expression>, Block, Block) =>
            // FnLiteral(Vec<Expression>, Block) =>
            // Call(Box<Expression>, Vec<Expression>) =>
            _ => todo!()
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    #[test]
    fn test_eval_integer_expression() {
        let tests = vec![
            (String::from("5"), Object::Integer(5)),
            (String::from("10"), Object::Integer(10)),
        ];

        for i in 0..tests.len() {
            let evaluated = test_eval(tests[i].0.clone());

            assert_eq!(evaluated, tests[i].1);
        }
    }

    fn test_eval(input: String) -> Object {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        let mut evaluator = Evaluator::new();

        evaluator.eval_program(program)
    }
}
