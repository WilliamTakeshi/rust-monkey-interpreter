use super::object::Object;
use crate::ast::Expression;
use std::collections::HashMap;

pub type BuildinFunction = fn(Vec<Object>) -> Object;

pub fn get_builtin_functions() -> HashMap<String, Object> {
    HashMap::from([create_buildin("len", 1, buildin_len)])
}

fn create_buildin(name: &str, param_num: u32, function: BuildinFunction) -> (String, Object) {
    (
        String::from(name),
        Object::Buildin(Expression::Ident(String::from(name)), param_num, function),
    )
}

fn buildin_len(input: Vec<Object>) -> Object {
    let input_object = &input[0];
    match input_object {
        // Object::Array(elem) => Ok(Object::Integer(elem.len() as i32)),
        Object::String(s) => Object::Integer(s.len() as i64),
        _ => Object::Err(format!(
            "argument to 'len' not supported, got {}",
            input_object.obj_type()
        )),
    }
}
