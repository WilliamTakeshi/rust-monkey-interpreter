use crate::{code::code::Instructions, object::object::Object};

#[derive(Debug, Clone)]
pub struct Frame {
    function: Object,
    pub ip: usize,
}

impl Frame {
    pub fn new(function: Object) -> Self {
        Frame {
            function,
            // Weird fix, I am using a usize::max_value() to indicate that the frame is new (-1)
            ip: usize::max_value(),
        }
    }

    pub fn instructions(&self) -> &Instructions {
        match &self.function {
            Object::CompiledFunction { instructions } => instructions,
            _ => panic!("Expected CompiledFunction, got {:?}", self.function),
        }
    }
}
