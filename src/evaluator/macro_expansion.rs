use std::{cell::RefCell, rc::Rc};

use crate::{
    ast::ast::{Expression, Program, Statement},
    object::{environment::Environment, object::Object},
};

fn define_macros(mut program: Program, mut env: Environment) -> (Program, Environment) {
    let mut definitions = vec![];

    for (i, statement) in program.iter().enumerate() {
        if is_macro_definition(statement) {
            env = add_macro(statement.clone(), env);
            definitions.push(i)
        }
    }

    for i in definitions.iter().rev() {
        program.remove(*i);
    }

    (program, env)
}

fn is_macro_definition(stmt: &Statement) -> bool {
    match stmt {
        Statement::Let(_, Expression::MacroLiteral(_, _)) => true,
        _ => false,
    }
}

fn add_macro(stmt: Statement, mut env: Environment) -> Environment {
    if let Statement::Let(name, Expression::MacroLiteral(parameters, body)) = stmt {
        let macro_obj = Object::Macro {
            parameters: parameters,
            body: body,
            env: Rc::new(RefCell::new(env.clone())),
        };
        env.set(name, macro_obj);
    }

    env
}

#[cfg(test)]
mod tests {
    use crate::{ast::ast::Infix, lexer::Lexer, parser::Parser};

    use super::*;

    fn test_parse_program(input: String) -> Program {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        parser.parse_program()
    }

    #[test]
    fn test_define_macros() {
        let input = "
            let number = 1;
            let function = fn(x, y) { x + y };
            let mymacro = macro(x, y) { x + y; };
        "
        .to_string();

        let program = test_parse_program(input);

        let (program, env) = define_macros(program, Environment::new());

        assert_eq!(program.len(), 2);

        assert!(env.get(String::from("number")).is_none());
        assert!(env.get(String::from("function")).is_none());
        assert!(env.get(String::from("mymacro")).is_some());

        dbg!(env.get(String::from("mymacro")));

        assert_eq!(
            env.get(String::from("mymacro")).unwrap(),
            Object::Macro {
                parameters: vec![
                    Expression::Ident(String::from("x"),),
                    Expression::Ident(String::from("y"),),
                ],
                body: vec![Statement::ExpressionStmt(Expression::InfixExpr(
                    Infix::Plus,
                    Box::from(Expression::Ident(String::from("x"),)),
                    Box::from(Expression::Ident(String::from("y"),)),
                ),),],
                env: Rc::new(RefCell::new(Environment::new())),
            },
        )
    }
}
