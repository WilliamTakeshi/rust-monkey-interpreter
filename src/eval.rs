use std::ops::RemAssign;
use std::os::linux::raw::stat;
use std::path::Prefix;

use crate::ast::{self, Block, Expression, Program};
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
            ast::Expression::Boolean(bool) => Object::Boolean(bool),
            ast::Expression::PrefixExpr(op, right) => {
                let right = self.eval_expr(*right);
                self.eval_prefix_expression(op, right)
            }
            ast::Expression::InfixExpr(op, left, right) => {
                let left = self.eval_expr(*left);
                let right = self.eval_expr(*right);

                self.eval_infix_expression(op, left, right)
            }
            ast::Expression::IfExpr(condition, consequence, alternative) => {
                self.eval_if_expression(*condition, consequence, alternative)
            }

            // Ident(String) =>
            // IfExpr(Box<Expression>, Block, Block) =>
            // FnLiteral(Vec<Expression>, Block) =>
            // Call(Box<Expression>, Vec<Expression>) =>
            _ => todo!(),
        }
    }

    fn eval_prefix_expression(&mut self, op: ast::Prefix, right: Object) -> Object {
        match op {
            ast::Prefix::Bang => match right {
                Object::Boolean(true) => Object::Boolean(false),
                Object::Boolean(false) => Object::Boolean(true),
                Object::Null => Object::Boolean(true),
                _ => Object::Boolean(false),
            },
            ast::Prefix::Minus => match right {
                Object::Integer(num) => Object::Integer(-num),
                obj => panic!("Function not defined: -{:?}", obj),
            },
        }
    }

    fn eval_infix_expression(&mut self, op: ast::Infix, left: Object, right: Object) -> Object {
        match (left, right) {
            (Object::Integer(l), Object::Integer(r)) => {
                self.eval_integer_infix_expression(op, l, r)
            }
            (Object::Boolean(l), Object::Boolean(r)) => self.eval_bool_infix_expression(op, l, r),
            _ => Object::Null,
        }
    }

    fn eval_integer_infix_expression(&self, op: ast::Infix, left: i64, right: i64) -> Object {
        match op {
            ast::Infix::Eq => Object::Boolean(left == right),
            ast::Infix::Neq => Object::Boolean(left != right),
            ast::Infix::Lt => Object::Boolean(left < right),
            ast::Infix::Gt => Object::Boolean(left > right),
            ast::Infix::Plus => Object::Integer(left + right),
            ast::Infix::Minus => Object::Integer(left - right),
            ast::Infix::Slash => Object::Integer(left / right),
            ast::Infix::Asterisk => Object::Integer(left * right),
        }
    }

    fn eval_bool_infix_expression(&self, op: ast::Infix, left: bool, right: bool) -> Object {
        match op {
            ast::Infix::Eq => Object::Boolean(left == right),
            ast::Infix::Neq => Object::Boolean(left != right),
            obj => panic!("Function not defined: {left:?} {obj:?} {right:?}"),
        }
    }

    fn eval_if_expression(&mut self, condition: Expression, consequence: Block, auternative: Block) -> Object {
        let condition = self.eval_expr(condition);
        if self.is_truthy(condition) {
            self.eval_statements(consequence)
        } else {
            self.eval_statements(auternative)
        }
    }

    fn is_truthy(&self, obj: Object) -> bool {
        match obj {
            Object::Null => false,
            Object::Boolean(b) => b,
            _ => true,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    fn test_eval(input: String) -> Object {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        let mut evaluator = Evaluator::new();

        evaluator.eval_program(program)
    }

    #[test]
    fn test_eval_integer_expression() {
        let tests = vec![
            ("5", Object::Integer(5)),
            ("10", Object::Integer(10)),
            ("5 + 5 + 5 + 5 - 10", Object::Integer(10)),
            ("2 * 2 * 2 * 2 * 2", Object::Integer(32)),
            ("-50 + 100 + -50", Object::Integer(0)),
            ("5 * 2 + 10", Object::Integer(20)),
            ("5 + 2 * 10", Object::Integer(25)),
            ("20 + 2 * -10", Object::Integer(0)),
            ("50 / 2 * 2 + 10", Object::Integer(60)),
            ("2 * (5 + 10)", Object::Integer(30)),
            ("3 * 3 * 3 + 10", Object::Integer(37)),
            ("3 * (3 * 3) + 10", Object::Integer(37)),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", Object::Integer(50)),
        ];

        for i in 0..tests.len() {
            let evaluated = test_eval(String::from(tests[i].0));

            assert_eq!(evaluated, tests[i].1);
        }
    }

    #[test]
    fn test_eval_boolean_expression() {
        let tests = vec![
            ("true", Object::Boolean(true)),
            ("false", Object::Boolean(false)),
            ("1 < 2", Object::Boolean(true)),
            ("1 > 2", Object::Boolean(false)),
            ("1 < 1", Object::Boolean(false)),
            ("1 > 1", Object::Boolean(false)),
            ("1 == 1", Object::Boolean(true)),
            ("1 != 1", Object::Boolean(false)),
            ("1 == 2", Object::Boolean(false)),
            ("1 != 2", Object::Boolean(true)),
            ("true == true", Object::Boolean(true)),
            ("false == false", Object::Boolean(true)),
            ("true == false", Object::Boolean(false)),
            ("true != false", Object::Boolean(true)),
            ("false != true", Object::Boolean(true)),
            ("(1 < 2) == true", Object::Boolean(true)),
            ("(1 < 2) == false", Object::Boolean(false)),
            ("(1 > 2) == true", Object::Boolean(false)),
            ("(1 > 2) == false", Object::Boolean(true)),
        ];

        for i in 0..tests.len() {
            let evaluated = test_eval(String::from(tests[i].0));
            assert_eq!(evaluated, tests[i].1);
        }
    }

    #[test]
    fn test_bang_operator() {
        let tests = vec![
            ("!true", Object::Boolean(false)),
            ("!false", Object::Boolean(true)),
            ("!5", Object::Boolean(false)),
            ("!!true", Object::Boolean(true)),
            ("!!false", Object::Boolean(false)),
            ("!!5", Object::Boolean(true)),
        ];

        for i in 0..tests.len() {
            let evaluated = test_eval(String::from(tests[i].0));

            assert_eq!(evaluated, tests[i].1);
        }
    }

    #[test]
    fn test_minus_operator() {
        let tests = vec![
            ("5", Object::Integer(5)),
            ("10", Object::Integer(10)),
            ("-5", Object::Integer(-5)),
            ("-10", Object::Integer(-10)),
        ];

        for i in 0..tests.len() {
            let evaluated = test_eval(String::from(tests[i].0));

            assert_eq!(evaluated, tests[i].1);
        }
    }

    #[test]
    fn test_if_else_expression() {
        let tests = vec![
            ("if (true) { 10 }", Object::Integer(10)),
            ("if (false) { 10 }", Object::Null),
            ("if (1) { 10 }", Object::Integer(10)),
            ("if (1 < 2) { 10 }", Object::Integer(10)),
            ("if (1 > 2) { 10 }", Object::Null),
            ("if (1 > 2) { 10; } else { 20 }", Object::Integer(20)),
            ("if (1 < 2) { 10; } else { 20 }", Object::Integer(10)),
        ];

        for i in 0..tests.len() {
            let evaluated = test_eval(String::from(tests[i].0));

            assert_eq!(evaluated, tests[i].1);
        }
    }
}
