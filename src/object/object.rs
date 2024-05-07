use std::{cell::RefCell, rc::Rc};

use crate::ast::{Block, Expression};

use super::environment::Environment;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Object {
    Boolean(bool),
    Integer(i64),
    Null,
    Return(Box<Object>),
    Err(String),
    Fn {
        parameters: Vec<Expression>,
        body: Block,
        env: Rc<RefCell<Environment>>,
    },
}

impl Object {
    pub fn obj_type(&self) -> String {
        match &self {
            Self::Boolean(_) => String::from("BOOLEAN"),
            Self::Integer(_) => String::from("INTEGER"),
            Self::Null => String::from("NULL"),
            Self::Return(_) => String::from("RETURN"),
            Self::Err(_) => String::from("ERROR"),
            Self::Fn { .. } => String::from("FUNCTION"),
        }
    }
}
