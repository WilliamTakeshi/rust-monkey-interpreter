use std::collections::HashMap;

use crate::object::object::Object;



pub struct Environment {
    store: HashMap<String, Object>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            store: HashMap::default()
        }
    }

    pub fn get(&self, key: String) -> Option<&Object> {
        self.store.get(&key)
    } 

    pub fn set(&mut self, key: String, val: Object) -> Object {
        self.store.insert(key, val.clone());
        val
    }
}