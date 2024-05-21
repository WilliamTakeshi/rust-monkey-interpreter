use crate::{
    ast::ast::{Block, Expression},
    code::code::Instructions,
};
use core::hash::{Hash, Hasher};
use std::{cell::RefCell, collections::HashMap, rc::Rc};

use super::{buildin::BuildinFunction, environment::Environment};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Object {
    Boolean(bool),
    Integer(i64),
    String(String),
    Buildin(Expression, u32, BuildinFunction),
    Array(Vec<Object>),
    Hash(HashMap<Object, Object>),
    Quote(Expression),
    Null,
    Return(Box<Object>),
    Err(String),
    Fn {
        parameters: Vec<Expression>,
        body: Block,
        env: Rc<RefCell<Environment>>,
    },
    Macro {
        parameters: Vec<Expression>,
        body: Block,
        env: Rc<RefCell<Environment>>,
    },
    CompiledFunction {
        instructions: Instructions,
    },
}

impl Hash for Object {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Object::Integer(i) => i.hash(state),
            Object::Boolean(b) => b.hash(state),
            Object::String(s) => s.hash(state),
            _ => "".hash(state),
        }
    }
}

impl Object {
    pub fn obj_type(&self) -> String {
        match &self {
            Self::Boolean(_) => String::from("BOOLEAN"),
            Self::Integer(_) => String::from("INTEGER"),
            Self::String(_) => String::from("STRING"),
            Self::Buildin(_, _, _) => String::from("BUILDIN"),
            Self::Null => String::from("NULL"),
            Self::Return(_) => String::from("RETURN"),
            Self::Err(_) => String::from("ERROR"),
            Self::Fn { .. } => String::from("FUNCTION"),
            Self::Array { .. } => String::from("ARRAY"),
            Self::Hash { .. } => String::from("HASH"),
            Self::Quote { .. } => String::from("QUOTE"),
            Self::Macro { .. } => String::from("MACRO"),
            Self::CompiledFunction { .. } => String::from("COMPILED_FUNCTION"),
        }
    }
}
