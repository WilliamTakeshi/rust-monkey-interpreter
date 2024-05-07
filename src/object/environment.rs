use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::object::object::Object;


#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Environment {
    store: HashMap<String, Object>,
    outer: Option<Rc<RefCell<Environment>>>
}

impl Environment {
    pub fn new() -> Self {
        Self {
            store: HashMap::default(),
            outer: None,
        }
    }

    pub fn new_enclosed_environment(outer: Rc<RefCell<Self>>) -> Self {
        Self {
            store: HashMap::new(),
            outer: Some(outer),
        }
    }

    pub fn get(&self, key: String) -> Option<Object> {
        match self.store.get(&key) {
            Some(val) => Some(val.clone()),
            None => match &self.outer {
                Some(outer_env) => {
                    outer_env.borrow().get(key).clone()
                } 
                None => None
            }
        }

    } 

    pub fn set(&mut self, key: String, val: Object) -> Object {
        self.store.insert(key, val.clone());
        val
    }

}