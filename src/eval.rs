use std::cell::RefCell;
use std::ops::RemAssign;
use std::os::linux::raw::stat;
use std::path::Prefix;
use std::rc::Rc;

use crate::object::environment::Environment;
use crate::ast::{self, Block, Expression, Program};
use crate::object::object::Object;
// use crate::token::Token;

pub struct Evaluator {
    environment: Rc<RefCell<Environment>>,
}

impl Evaluator {
    pub fn new() -> Self {
        Self {
            environment: Rc::new(RefCell::new(Environment::new()))
        }
    }

    pub fn eval_program(&mut self, program: Program) -> Object {
        match program.len() {
            0 => Object::Null,
            1 => {
                let obj = self.eval_statement(program[0].clone());
                match obj {
                    Object::Return(obj) => return *obj,
                    _ => obj,
                }
            }
            _ => self.eval_statements(program),
        }
    }

    fn eval_statements(&mut self, statements: Vec<ast::Statement>) -> Object {
        let mut result = Object::Null;
        for statement in statements {
            result = self.eval_statement(statement.clone());

            result = match result {
                Object::Return(obj) => return *obj,
                Object::Err(_) => return result,
                _ => result
            }
        }

        result
    }

    fn eval_block(&mut self, statements: Vec<ast::Statement>) -> Object {
        let mut result = Object::Null;
        for statement in statements {
            result = self.eval_statement(statement.clone());

            result = match result {
                Object::Return(_) => return result,
                Object::Err(_) => return result,
                _ => result
            }
        }

        result
    }

    fn eval_statement(&mut self, statement: ast::Statement) -> Object {
        match statement {
            ast::Statement::ExpressionStmt(expr) => self.eval_expr(expr),
            ast::Statement::Let(ident, expression) => {
                let val = self.eval_expr(expression);
                if self.is_error(&val) {
                    return val
                }
                let mut env = self.environment.borrow_mut();
                env.set(ident, val)
            },
            ast::Statement::Return(expr) => {
                let val = self.eval_expr(expr);
                if self.is_error(&val) {
                    return val;
                }
                Object::Return(Box::from(val))},
        }
    }

    fn eval_expr(&mut self, expr: Expression) -> Object {
        match expr {
            Expression::Literal(num) => Object::Integer(num as i64),
            Expression::Boolean(bool) => Object::Boolean(bool),
            Expression::PrefixExpr(op, right) => {
                let right = self.eval_expr(*right);
                if self.is_error(&right) {
                    return right;
                }
                self.eval_prefix_expression(op, right)
            }
            Expression::InfixExpr(op, left, right) => {
                let left = self.eval_expr(*left);
                if self.is_error(&left) {
                    return left;
                }
                let right = self.eval_expr(*right);
                if self.is_error(&right) {
                    return right;
                }
                self.eval_infix_expression(op, left, right)
            }
            Expression::IfExpr(condition, consequence, alternative) => {
                self.eval_if_expression(*condition, consequence, alternative)
            }

            Expression::Ident(ident) => {
                let env = self.environment.borrow();
                match env.get(ident.clone()) {
                    Some(obj) => obj.clone(),
                    None => Object::Err(format!("identifier not found: {}", ident)),
                }
            }
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
                obj => Object::Err(format!("unknown operator: -{}", obj.obj_type())),
            },
        }
    }

    fn eval_infix_expression(&mut self, op: ast::Infix, left: Object, right: Object) -> Object {
        match (left, right) {
            (Object::Integer(l), Object::Integer(r)) => {
                self.eval_integer_infix_expression(op, l, r)
            }
            (Object::Boolean(l), Object::Boolean(r)) => self.eval_bool_infix_expression(op, l, r),
            (left, right) => Object::Err(format!("type mismatch: {} {} {}", left.obj_type(), op.to_string(), right.obj_type())),
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
        match &op {
            ast::Infix::Eq => Object::Boolean(left == right),
            ast::Infix::Neq => Object::Boolean(left != right),
            _ => Object::Err(format!("unknown operator: BOOLEAN {} BOOLEAN", op.to_string())),

        }
    }

    fn eval_if_expression(
        &mut self,
        condition: Expression,
        consequence: Block,
        auternative: Block,
    ) -> Object {
        let condition = self.eval_expr(condition);
        if self.is_error(&condition) {
            return condition;
        }
        if self.is_truthy(condition) {
            self.eval_block(consequence)
        } else {
            self.eval_block(auternative)
        }
    }

    fn is_truthy(&self, obj: Object) -> bool {
        match obj {
            Object::Null => false,
            Object::Boolean(b) => b,
            _ => true,
        }
    }

    fn is_error(&self, obj: &Object) -> bool {
        match obj {
            Object::Err(_) => true,
            _ => false,
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

    #[test]
    fn test_return_statements() {
        let tests = vec![
            ("return 10;", Object::Integer(10)),
            ("return 10; 9;", Object::Integer(10)),
            ("return 2 * 5; 9;", Object::Integer(10)),
            ("9; return 2 * 5; 9;", Object::Integer(10)),
            (
                "if (10 > 1) {
                if (10 > 1) {
                    return 10;
                }
                return 1;
            })",
                Object::Integer(10),
            ),
        ];

        for i in 0..tests.len() {
            let evaluated = test_eval(String::from(tests[i].0));

            assert_eq!(evaluated, tests[i].1);
        }
    }

    #[test]
    fn test_error_handling() {
        let tests = vec![
            ("5 + true;", "type mismatch: INTEGER + BOOLEAN"),
            ("5 + true; 5;", "type mismatch: INTEGER + BOOLEAN"),
            ("-true", "unknown operator: -BOOLEAN"),
            ("true + false;", "unknown operator: BOOLEAN + BOOLEAN"),
            ("5; true + false; 5", "unknown operator: BOOLEAN + BOOLEAN"),
            (
                "if (10 > 1) { true + false; }",
                "unknown operator: BOOLEAN + BOOLEAN",
            ),
            (
                "if (10 > 1) {
                    if (10 > 1) {
                        return true + false;
                    }
                    return 1;
                }",
                "unknown operator: BOOLEAN + BOOLEAN",
            ),
        ];

        for i in 0..tests.len() {
            let evaluated = test_eval(String::from(tests[i].0));

            assert_eq!(evaluated, Object::Err(String::from(tests[i].1)));
        }
    }

    #[test]
    fn test_let_statements() {
        let tests = vec![
            ("let a = 5; a;", Object::Integer(5)),
            ("let a = 5 * 5; a;", Object::Integer(25)),
            ("let a = 5; let b = a; b;", Object::Integer(5)),
            ("let a = 5; let b = a; let c = a + b + 5; c;", Object::Integer(15)),
            ("foobar", Object::Err(String::from("identifier not found: foobar"))),
        ];

        for i in 0..tests.len() {
            let evaluated = test_eval(String::from(tests[i].0));

            assert_eq!(evaluated, tests[i].1);
        }
    }
}
