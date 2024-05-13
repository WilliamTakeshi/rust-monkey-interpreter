use std::cell::Ref;

use crate::{
    ast::{ast::Expression, modify::modify_expr},
    object::{environment::Environment, object::Object},
};

use super::eval::Evaluator;

pub fn quote(expr: Expression, env: Ref<Environment>) -> Object {
    dbg!("quote");
    let expr = eval_unquote_calls(expr, env);

    return Object::Quote(expr);
}

fn eval_unquote_calls(quoted: Expression, env: Ref<Environment>) -> Expression {
    dbg!("eval_unquote_calls");
    let modify_func = |expr| {
        if !is_unquote_call(&expr) {
            return expr;
        }

        if let Expression::Call(_, ref args) = expr {
            if args.len() != 1 {
                return expr;
            }
            let mut evaluator = Evaluator::new_with_env(env.clone());
            let obj = evaluator.eval_expr(args[0].clone());

            convert_object_to_ast_node(obj)
        } else {
            expr
        }
    };

    modify_expr(quoted, &modify_func)
}

fn is_unquote_call(expr: &Expression) -> bool {
    if let Expression::Call(function, _) = expr {
        **function == Expression::Ident(String::from("unquote"))
    } else {
        false
    }
}

fn convert_object_to_ast_node(obj: Object) -> Expression {
    match obj {
        Object::Integer(n) => Expression::IntLiteral(n as u64),
        Object::Boolean(b) => Expression::Boolean(b),
        Object::Quote(expr) => expr,
        _ => todo!(),
    }
}
