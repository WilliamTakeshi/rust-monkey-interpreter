use super::object::Object;
use crate::ast::ast::Expression;
use std::collections::HashMap;

pub type BuildinFunction = fn(Vec<Object>) -> Object;

pub fn get_builtin_functions() -> HashMap<String, Object> {
    HashMap::from([
        create_buildin("len", 1, builtin_len),
        create_buildin("first", 1, builtin_first),
        create_buildin("last", 1, builtin_last),
        create_buildin("rest", 1, builtin_rest),
        create_buildin("puts", 1, builtin_puts),
        create_buildin("push", 2, builtin_push),
    ])
}

fn create_buildin(name: &str, param_num: u32, function: BuildinFunction) -> (String, Object) {
    (
        String::from(name),
        Object::Buildin(Expression::Ident(String::from(name)), param_num, function),
    )
}

fn builtin_len(input: Vec<Object>) -> Object {
    let input_object = &input[0];
    match input_object {
        Object::Array(elem) => Object::Integer(elem.len() as i64),
        Object::String(s) => Object::Integer(s.len() as i64),
        _ => Object::Err(format!(
            "argument to 'len' not supported, got {}",
            input_object.obj_type()
        )),
    }
}

fn builtin_first(input: Vec<Object>) -> Object {
    let input_object = &input[0];
    match input_object {
        Object::Array(elem) => {
            if elem.len() > 0 {
                elem[0].clone()
            } else {
                Object::Null
            }
        }
        _ => Object::Err(format!(
            "argument to 'first' not supported, got {}",
            input_object.obj_type()
        )),
    }
}

fn builtin_last(input: Vec<Object>) -> Object {
    let input_object = &input[0];
    match input_object {
        Object::Array(elem) => {
            if elem.len() > 0 {
                elem[elem.len() - 1].clone()
            } else {
                Object::Null
            }
        }
        _ => Object::Err(format!(
            "argument to 'last' not supported, got {}",
            input_object.obj_type()
        )),
    }
}

fn builtin_push(input: Vec<Object>) -> Object {
    let input_object = &input[0];
    match input_object {
        Object::Array(arr) => {
            let mut arr = arr.clone();
            arr.push(input[1].clone());
            Object::Array(arr)
        }
        _ => Object::Err(format!(
            "argument to 'last' not supported, got {}",
            input_object.obj_type()
        )),
    }
}

fn builtin_rest(input: Vec<Object>) -> Object {
    let input_object = &input[0];
    match input_object {
        Object::Array(elem) => {
            if elem.len() > 0 {
                let mut new_elem = elem.clone();
                new_elem.remove(0);
                Object::Array(new_elem)
            } else {
                Object::Null
            }
        }
        _ => Object::Err(format!(
            "argument to 'rest' not supported, got {}",
            input_object.obj_type()
        )),
    }
}

fn builtin_puts(input: Vec<Object>) -> Object {
    for object in input.iter() {
        println!("{:?}", object);
    }

    Object::Null
}
