use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::ast::ast::{Block, Expression, Infix, Prefix, Program, Statement};
use crate::object::buildin::get_builtin_functions;
use crate::object::environment::Environment;
use crate::object::object::Object;

use crate::evaluator::quote_unquote::quote;
// use crate::token::Token;

pub struct Evaluator {
    pub environment: Rc<RefCell<Environment>>,
}

impl Evaluator {
    pub fn new() -> Self {
        Self {
            environment: Rc::new(RefCell::new(Environment::new())),
        }
    }

    pub fn new_with_env(environment: Environment) -> Self {
        Self {
            environment: Rc::new(RefCell::new(environment)),
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

    fn eval_statements(&mut self, statements: Vec<Statement>) -> Object {
        let mut result = Object::Null;
        for statement in statements {
            result = self.eval_statement(statement.clone());

            result = match result {
                Object::Return(obj) => return *obj,
                Object::Err(_) => return result,
                _ => result,
            }
        }

        result
    }

    fn eval_block(&mut self, statements: Vec<Statement>) -> Object {
        let mut result = Object::Null;
        for statement in statements {
            result = self.eval_statement(statement.clone());

            result = match result {
                Object::Return(_) => return result,
                Object::Err(_) => return result,
                _ => result,
            }
        }

        result
    }

    fn eval_statement(&mut self, statement: Statement) -> Object {
        match statement {
            Statement::ExpressionStmt(expr) => self.eval_expr(expr),
            Statement::Let(ident, expression) => {
                let val = self.eval_expr(expression);
                if self.is_error(&val) {
                    return val;
                }
                let mut env = self.environment.borrow_mut();
                env.set(ident, val);
                Object::Null
            }
            Statement::Return(expr) => {
                let val = self.eval_expr(expr);
                if self.is_error(&val) {
                    return val;
                }
                Object::Return(Box::from(val))
            }
        }
    }

    pub fn eval_expr(&mut self, expr: Expression) -> Object {
        match expr {
            Expression::IntLiteral(num) => Object::Integer(num as i64),
            Expression::Boolean(bool) => Object::Boolean(bool),
            Expression::StringLiteral(s) => Object::String(s),
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
            Expression::Ident(ident) => self.eval_ident(ident),
            Expression::FnLiteral(parameters, body) => Object::Fn {
                parameters: parameters,
                body: body,
                env: self.environment.clone(),
            },
            Expression::Call(function, arguments) => {
                self.eval_call_expression(*function, arguments)
            }
            Expression::ArrayLiteral(exprs) => {
                let elements = self.eval_expressions(exprs);
                if elements.len() == 1 && self.is_error(&elements[0]) {
                    return elements[0].clone();
                }

                Object::Array(elements)
            }
            Expression::IndexExpression(left, index) => {
                let left = self.eval_expr(*left);
                if self.is_error(&left) {
                    return left;
                }

                let index = self.eval_expr(*index);
                if self.is_error(&index) {
                    return index;
                }

                self.eval_index_expression(left, index)
            }
            Expression::HashLiteral(nodes) => self.eval_hash_literal(nodes),
            Expression::MacroLiteral(_, _) => todo!(),
        }
    }

    fn eval_hash_literal(&mut self, nodes: Vec<(Expression, Expression)>) -> Object {
        let mut pairs = HashMap::new();

        for (key, value) in nodes {
            let key = self.eval_expr(key);
            if self.is_error(&key) {
                return key;
            }

            let value = self.eval_expr(value);
            if self.is_error(&value) {
                return value;
            }

            pairs.insert(key, value);
        }
        Object::Hash(pairs)
    }

    fn eval_ident(&mut self, ident: String) -> Object {
        let env = self.environment.borrow();

        if let Some(obj) = env.get(ident.clone()) {
            return obj.clone();
        }

        let hm = get_builtin_functions();
        if let Some(obj) = hm.get(&ident) {
            return obj.clone();
        }
        Object::Err(format!("identifier not found: {}", ident))
    }

    fn eval_prefix_expression(&mut self, op: Prefix, right: Object) -> Object {
        match op {
            Prefix::Bang => match right {
                Object::Boolean(true) => Object::Boolean(false),
                Object::Boolean(false) => Object::Boolean(true),
                Object::Null => Object::Boolean(true),
                _ => Object::Boolean(false),
            },
            Prefix::Minus => match right {
                Object::Integer(num) => Object::Integer(-num),
                obj => Object::Err(format!("unknown operator: -{}", obj.obj_type())),
            },
        }
    }

    fn eval_infix_expression(&mut self, op: Infix, left: Object, right: Object) -> Object {
        match (left, right) {
            (Object::Integer(l), Object::Integer(r)) => {
                self.eval_integer_infix_expression(op, l, r)
            }
            (Object::Boolean(l), Object::Boolean(r)) => self.eval_bool_infix_expression(op, l, r),
            (Object::String(l), Object::String(r)) => self.eval_string_infix_expression(op, l, r),
            (left, right) => Object::Err(format!(
                "type mismatch: {} {} {}",
                left.obj_type(),
                op.to_string(),
                right.obj_type()
            )),
        }
    }

    fn eval_string_infix_expression(&self, op: Infix, left: String, right: String) -> Object {
        match op {
            Infix::Plus => Object::String(format!("{}{}", left, right)),
            _ => Object::Err(format!(
                "unknown operator: STRING {} STRING",
                op.to_string()
            )),
        }
    }

    fn eval_integer_infix_expression(&self, op: Infix, left: i64, right: i64) -> Object {
        match op {
            Infix::Eq => Object::Boolean(left == right),
            Infix::Neq => Object::Boolean(left != right),
            Infix::Lt => Object::Boolean(left < right),
            Infix::Gt => Object::Boolean(left > right),
            Infix::Plus => Object::Integer(left + right),
            Infix::Minus => Object::Integer(left - right),
            Infix::Slash => Object::Integer(left / right),
            Infix::Asterisk => Object::Integer(left * right),
        }
    }

    fn eval_bool_infix_expression(&self, op: Infix, left: bool, right: bool) -> Object {
        match &op {
            Infix::Eq => Object::Boolean(left == right),
            Infix::Neq => Object::Boolean(left != right),
            _ => Object::Err(format!(
                "unknown operator: BOOLEAN {} BOOLEAN",
                op.to_string()
            )),
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

    fn eval_call_expression(&mut self, function: Expression, args: Vec<Expression>) -> Object {
        if Expression::Ident(String::from("quote")) == function {
            let env = self.environment.borrow();

            return quote(args[0].clone(), env);
        }

        let function_obj = self.eval_expr(function);
        if self.is_error(&function_obj) {
            return function_obj;
        }

        let args = self.eval_expressions(args);
        if args.len() == 1 && self.is_error(&args[0]) {
            return args[0].clone();
        }

        self.apply_function(function_obj, args)
    }

    fn apply_function(&mut self, function: Object, args: Vec<Object>) -> Object {
        match function {
            Object::Fn {
                parameters,
                body,
                env,
            } => {
                if parameters.len() != args.len() {
                    return Object::Err(format!(
                        "wrong number of arguments, got {} instead of {}",
                        args.len(),
                        parameters.len()
                    ));
                }
                let extended_env = self.extend_function_env(env, parameters, args);
                let mut evaluator = Evaluator::new_with_env(extended_env);
                let evaluated = evaluator.eval_statements(body);
                self.unwrap_return_value(evaluated)
            }
            Object::Buildin(_, param_num, function) => {
                if args.len() == param_num as usize {
                    function(args)
                } else {
                    Object::Err(format!(
                        "wrong number of arguments. got={}, want={}",
                        args.len(),
                        param_num
                    ))
                }
            }
            _ => Object::Err(format!("not a function: {}", function.obj_type())),
        }
    }

    fn extend_function_env(
        &mut self,
        outer: Rc<RefCell<Environment>>,
        function: Vec<Expression>,
        args: Vec<Object>,
    ) -> Environment {
        let mut environment = Environment::new_enclosed_environment(outer);

        for (idx, arg) in args.iter().enumerate() {
            match &function[idx] {
                Expression::Ident(ident) => {
                    environment.set(ident.clone(), arg.clone());
                }
                _ => panic!("TODO:"),
            }
        }

        environment
    }

    fn eval_expressions(&mut self, exprs: Vec<Expression>) -> Vec<Object> {
        let mut result = vec![];

        for expr in exprs {
            let evaluated = self.eval_expr(expr);
            if self.is_error(&evaluated) {
                return vec![evaluated];
            }

            result.push(evaluated);
        }

        result
    }

    fn eval_index_expression(&self, left: Object, index: Object) -> Object {
        match (left.clone(), index) {
            (Object::Array(arr), Object::Integer(idx)) => {
                let max = (arr.len() - 1) as i64;
                if idx < 0 || idx > max {
                    return Object::Null;
                }
                arr[idx as usize].clone()
            }
            (Object::Hash(hm), index) => match index {
                Object::Integer(_) | Object::String(_) | Object::Boolean(_) => {
                    match hm.get(&index) {
                        None => Object::Null,
                        Some(o) => o.clone(),
                    }
                }
                obj => Object::Err(format!("unusable as hash key: {}", obj.obj_type())),
            },
            _ => Object::Err(format!("index operator not supported: {}", left.obj_type())),
        }
    }

    fn unwrap_return_value(&self, obj: Object) -> Object {
        match obj {
            Object::Return(v) => *v,
            obj => obj,
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
    use std::collections::HashMap;

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
            ("if (1 > 2) { 10 } else { 20 }", Object::Integer(20)),
            ("if (1 < 2) { 10 } else { 20 }", Object::Integer(10)),
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
                }",
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
                "if (10 > 1) { true + false }",
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
            (r#""hello" - "world""#, "unknown operator: STRING - STRING"),
            (
                r#"{"name": "Monkey"}[fn(x) { x }];"#,
                "unusable as hash key: FUNCTION",
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
            (
                "let a = 5; let b = a; let c = a + b + 5; c;",
                Object::Integer(15),
            ),
            (
                "foobar",
                Object::Err(String::from("identifier not found: foobar")),
            ),
        ];

        for i in 0..tests.len() {
            let evaluated = test_eval(String::from(tests[i].0));

            assert_eq!(evaluated, tests[i].1);
        }
    }

    #[test]
    fn test_function_object() {
        let tests = vec![(
            "fn(x) { x + 2; };",
            Object::Fn {
                parameters: vec![Expression::Ident(String::from("x"))],
                body: vec![Statement::ExpressionStmt(Expression::InfixExpr(
                    Infix::Plus,
                    Box::from(Expression::Ident(String::from("x"))),
                    Box::from(Expression::IntLiteral(2)),
                ))],
                env: Rc::new(RefCell::new(Environment::new())),
            },
        )];

        for i in 0..tests.len() {
            let evaluated = test_eval(String::from(tests[i].0));

            assert_eq!(evaluated, tests[i].1);
        }
    }

    #[test]
    fn test_function_application() {
        let tests = vec![
            (
                "let identity = fn(x) { x; }; identity(5);",
                Object::Integer(5),
            ),
            (
                "let identity = fn(x) { return x; }; identity(5);",
                Object::Integer(5),
            ),
            (
                "let double = fn(x) { x * 2; }; double(5);",
                Object::Integer(10),
            ),
            (
                "let add = fn(x, y) { x + y; }; add(5, 5);",
                Object::Integer(10),
            ),
            (
                "let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));",
                Object::Integer(20),
            ),
            ("fn(x) { x; }(5)", Object::Integer(5)),
        ];

        for i in 0..tests.len() {
            let evaluated = test_eval(String::from(tests[i].0));

            assert_eq!(evaluated, tests[i].1);
        }
    }
    #[test]
    fn test_closures() {
        let tests = vec![(
            "let newAdder = fn(x) {
                    fn(y) { x + y };
                };
                let addTwo = newAdder(2);
                addTwo(2);",
            Object::Integer(4),
        )];

        for i in 0..tests.len() {
            let evaluated = test_eval(String::from(tests[i].0));

            assert_eq!(evaluated, tests[i].1);
        }
    }

    #[test]
    fn test_string_literal() {
        let tests = vec![(
            r#"
            "hello world!";
            "#,
            Object::String(String::from("hello world!")),
        )];

        for i in 0..tests.len() {
            let evaluated = test_eval(String::from(tests[i].0));

            assert_eq!(evaluated, tests[i].1);
        }
    }

    #[test]
    fn test_string_concat() {
        let tests = vec![(
            r#"
            "hello" + " world!";
            "#,
            Object::String(String::from("hello world!")),
        )];

        for i in 0..tests.len() {
            let evaluated = test_eval(String::from(tests[i].0));

            assert_eq!(evaluated, tests[i].1);
        }
    }

    #[test]
    fn test_builtin_functions() {
        let tests = vec![
            (r#"len("");"#, Object::Integer(0)),
            (r#"len("four");"#, Object::Integer(4)),
            (r#"len("hello world");"#, Object::Integer(11)),
            (
                r#"len(1);"#,
                Object::Err(String::from("argument to 'len' not supported, got INTEGER")),
            ),
            (
                r#"len("one", "two");"#,
                Object::Err(String::from("wrong number of arguments. got=2, want=1")),
            ),
        ];

        for i in 0..tests.len() {
            let evaluated = test_eval(String::from(tests[i].0));

            assert_eq!(evaluated, tests[i].1);
        }
    }

    #[test]
    fn test_array_literals() {
        let tests = vec![(
            "[1, 2 * 3, 4 + 5];",
            Object::Array(vec![
                Object::Integer(1),
                Object::Integer(6),
                Object::Integer(9),
            ]),
        )];

        for i in 0..tests.len() {
            let evaluated = test_eval(String::from(tests[i].0));

            assert_eq!(evaluated, tests[i].1);
        }
    }

    #[test]
    fn test_index_expressions() {
        let tests = vec![
            ("[1, 2, 3][0];", Object::Integer(1)),
            ("[1, 2, 3][1];", Object::Integer(2)),
            ("[1, 2, 3][2];", Object::Integer(3)),
            ("let i=0; [1][i];", Object::Integer(1)),
            ("[1, 2, 3][1+1];", Object::Integer(3)),
            ("let myArray = [1, 2, 3]; myArray[2];", Object::Integer(3)),
            (
                "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];",
                Object::Integer(6),
            ),
            (
                "let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i];",
                Object::Integer(2),
            ),
            ("[1, 2, 3][3];", Object::Null),
            ("[1, 2, 3][-1];", Object::Null),
        ];

        for i in 0..tests.len() {
            let evaluated = test_eval(String::from(tests[i].0));

            assert_eq!(evaluated, tests[i].1);
        }
    }

    #[test]
    fn test_array_buildins() {
        let tests = vec![
            (
                r#"
            let map = fn(arr, f) {
                let iter = fn(arr, accumulated) {
                    if (len(arr) == 0) {
                        accumulated
                    } else {
                        iter(rest(arr), push(accumulated, f(first(arr))));
                    }
                };
                iter(arr, []);
            };
            let a = [1, 2, 3, 4];
            let double = fn(x) { x * 2 };
            map(a, double);"#,
                Object::Array(vec![
                    Object::Integer(2),
                    Object::Integer(4),
                    Object::Integer(6),
                    Object::Integer(8),
                ]),
            ),
            (
                r#"
            let reduce = fn(arr, initial, f) {
                let iter = fn(arr, result) {
                    if (len(arr) == 0) {
                        result
                    } else {
                        iter(rest(arr), f(result, first(arr)));
                    }
                };
                iter(arr, initial);
            };
            let sum = fn(arr) {
                reduce(arr, 0, fn(initial, el) { initial + el });
            };
            sum([1, 2, 3, 4, 5]);"#,
                Object::Integer(15),
            ),
        ];

        for i in 0..tests.len() {
            let evaluated = test_eval(String::from(tests[i].0));

            assert_eq!(evaluated, tests[i].1);
        }
    }

    #[test]
    fn test_hash_literals() {
        let tests = vec![(
            r#"
                let two = "two";
                {
                    "one": 10 - 9,
                    two: 1 + 1,
                    "thr" + "ee": 6 / 2,
                    4: 4,
                    true: 5,
                    false: 6
                };
            "#,
            Object::Hash(HashMap::from([
                (Object::String(String::from("one")), Object::Integer(1)),
                (Object::String(String::from("two")), Object::Integer(2)),
                (Object::String(String::from("three")), Object::Integer(3)),
                (Object::Integer(4), Object::Integer(4)),
                (Object::Boolean(true), Object::Integer(5)),
                (Object::Boolean(false), Object::Integer(6)),
            ])),
        )];

        for i in 0..tests.len() {
            let evaluated = test_eval(String::from(tests[i].0));
            assert_eq!(evaluated, tests[i].1);
        }
    }

    #[test]
    fn test_hash_index_expressions() {
        let tests = vec![
            (r#"{"foo": 5}["foo"]"#, Object::Integer(5)),
            (r#"{"foo": 5}["bar"]"#, Object::Null),
            (r#"let key = "foo"; {"foo": 5}[key]"#, Object::Integer(5)),
            (r#"{}["foo"]"#, Object::Null),
            (r#"{5: 5}[5]"#, Object::Integer(5)),
            (r#"{true: 5}[true]"#, Object::Integer(5)),
            (r#"{false: 5}[false]"#, Object::Integer(5)),
        ];

        for i in 0..tests.len() {
            let evaluated = test_eval(String::from(tests[i].0));
            assert_eq!(evaluated, tests[i].1);
        }
    }

    #[test]
    fn test_quote() {
        let tests = vec![
            ("quote(5)", Object::Quote(Expression::IntLiteral(5))),
            (
                "quote(5 + 8)",
                Object::Quote(Expression::InfixExpr(
                    Infix::Plus,
                    Box::from(Expression::IntLiteral(5)),
                    Box::from(Expression::IntLiteral(8)),
                )),
            ),
            (
                "quote(foobar)",
                Object::Quote(Expression::Ident(String::from("foobar"))),
            ),
            (
                "quote(foobar + barfoo)",
                Object::Quote(Expression::InfixExpr(
                    Infix::Plus,
                    Box::from(Expression::Ident(String::from("foobar"))),
                    Box::from(Expression::Ident(String::from("barfoo"))),
                )),
            ),
        ];
        for i in 0..tests.len() {
            let evaluated = test_eval(String::from(tests[i].0));
            assert_eq!(evaluated, tests[i].1);
        }
    }

    #[test]
    fn test_quote_unquote() {
        let tests = vec![
            (
                "quote(unquote(4))",
                Object::Quote(Expression::IntLiteral(4)),
            ),
            (
                "quote(unquote(4 + 4))",
                Object::Quote(Expression::IntLiteral(8)),
            ),
            (
                "quote(8 + unquote(4 + 5))",
                Object::Quote(Expression::InfixExpr(
                    Infix::Plus,
                    Box::from(Expression::IntLiteral(8)),
                    Box::from(Expression::IntLiteral(9)),
                )),
            ),
            (
                "quote(unquote(4 + 5)+ 8)",
                Object::Quote(Expression::InfixExpr(
                    Infix::Plus,
                    Box::from(Expression::IntLiteral(9)),
                    Box::from(Expression::IntLiteral(8)),
                )),
            ),
            (
                "let foobar = 8;
                quote(foobar);",
                Object::Quote(Expression::Ident(String::from("foobar"))),
            ),
            (
                "let foobar = 8;
                quote(unquote(foobar));",
                Object::Quote(Expression::IntLiteral(8)),
            ),
            (
                "quote(unquote(true));",
                Object::Quote(Expression::Boolean(true)),
            ),
            (
                "quote(unquote(true == false));",
                Object::Quote(Expression::Boolean(false)),
            ),
            (
                "quote(unquote(quote(4 + 4)));",
                Object::Quote(Expression::InfixExpr(
                    Infix::Plus,
                    Box::from(Expression::IntLiteral(4)),
                    Box::from(Expression::IntLiteral(4)),
                )),
            ),
            (
                "let quotedInfixExpression = quote(4 + 4);
                quote(unquote(4 + 4) + unquote(quotedInfixExpression));",
                Object::Quote(Expression::InfixExpr(
                    Infix::Plus,
                    Box::from(Expression::IntLiteral(8)),
                    Box::from(Expression::InfixExpr(
                        Infix::Plus,
                        Box::from(Expression::IntLiteral(4)),
                        Box::from(Expression::IntLiteral(4)),
                    )),
                )),
            ),
        ];
        for i in 0..tests.len() {
            let evaluated = test_eval(String::from(tests[i].0));
            assert_eq!(evaluated, tests[i].1);
        }
    }
}
