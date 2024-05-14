use std::{cell::RefCell, rc::Rc};

use crate::{
    ast::{
        ast::{Expression, Program, Statement},
        modify::modify_program,
    },
    object::{environment::Environment, object::Object},
};

use super::eval::Evaluator;

pub fn define_macros(mut program: Program, mut env: Environment) -> (Program, Environment) {
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
    matches!(stmt, Statement::Let(_, Expression::MacroLiteral(_, _)))
}

fn add_macro(stmt: Statement, mut env: Environment) -> Environment {
    if let Statement::Let(name, Expression::MacroLiteral(parameters, body)) = stmt {
        let macro_obj = Object::Macro {
            parameters,
            body,
            env: Rc::new(RefCell::new(env.clone())),
        };
        env.set(name, macro_obj);
    }

    env
}

pub fn expand_macros(program: Program, env: Environment) -> Vec<Statement> {
    let modify_fn = |expr| {
        let env = env.clone();
        if let Expression::Call(_, _) = expr {
            let (macro_obj, ok) = is_macro_call(expr.clone(), env);
            if !ok {
                return expr;
            }

            let args = quote_args(expr.clone());

            let eval_env = extend_macro_env(macro_obj.clone(), args);

            let evaluated_quote = match macro_obj {
                Object::Macro { body, .. } => {
                    let mut evaluator = Evaluator::new_with_env(eval_env);

                    evaluator.eval_program(body)
                }
                _ => Object::Err("Macro not found".to_string()),
            };

            let evaluated = match evaluated_quote {
                Object::Quote(expr) => expr,
                _ => panic!("Not a quote()"),
            };
            return evaluated;
        }

        expr
    };

    modify_program(program, &modify_fn)
}

fn is_macro_call(expr: Expression, env: Environment) -> (Object, bool) {
    let ident = match expr {
        Expression::Call(expr, _) => match *expr {
            Expression::Ident(ident) => ident,
            _ => {
                return (
                    Object::Err(format!("Error evaluating macro call {:?}", expr)),
                    false,
                )
            }
        },
        _ => {
            return (
                Object::Err(format!("Error evaluating macro call {:?}", expr)),
                false,
            )
        }
    };

    let obj = env.get(ident.clone());

    let obj = match obj {
        Some(Object::Macro {
            parameters,
            body,
            env,
        }) => Object::Macro {
            parameters,
            body,
            env,
        },
        Some(_) => {
            return (
                Object::Err(format!("Not found a macro with name {:?}", &ident)),
                false,
            )
        }
        None => {
            return (
                Object::Err(format!("Not found a macro with name {:?}", &ident)),
                false,
            )
        }
    };

    (obj, true)
}

fn quote_args(expr: Expression) -> Vec<Object> {
    let mut res = vec![];

    if let Expression::Call(_, args) = expr {
        for arg in args {
            res.push(Object::Quote(arg))
        }
    }

    res
}

fn extend_macro_env(macro_obj: Object, args: Vec<Object>) -> Environment {
    if let Object::Macro {
        parameters,
        body: _,
        env,
    } = macro_obj
    {
        let mut extended = env.borrow().clone();

        for (idx, param) in parameters.iter().enumerate() {
            if let Expression::Ident(s) = param {
                extended.set(s.clone(), args[idx].clone());
            }
        }

        extended
    } else {
        panic!("Called extend_macro_env without an macro_object")
    }
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

    #[test]
    fn test_expand_macros() {
        let tests = vec![
            (
                String::from(
                    "let infixExpression = macro() { quote(1 + 2); };
                infixExpression();",
                ),
                String::from("1+2"),
            ),
            (
                String::from(
                    "let reverse = macro(a, b) { quote(unquote(b) - unquote(a)); };
                    reverse(2 + 2, 10 - 5);",
                ),
                String::from("(10 - 5) - (2 + 2)"),
            ),
            (
                String::from(
                    r#"
                    let unless = macro(condition, consequence, alternative) {
                        quote(if (!(unquote(condition))) {
                            unquote(consequence);
                        } else {
                            unquote(alternative);
                        });
                    };
                    unless(10 > 5, puts("not greater"), puts("greater"));"#,
                ),
                String::from(r#"if (!(10 > 5)) { puts("not greater") } else { puts("greater") }"#),
            ),
        ];

        for test in tests {
            let program = test_parse_program(test.0);
            let expected = test_parse_program(test.1);

            let (program, env) = define_macros(program, Environment::new());
            let expanded = expand_macros(program, env);

            assert_eq!(expanded, expected)
        }
    }
}
