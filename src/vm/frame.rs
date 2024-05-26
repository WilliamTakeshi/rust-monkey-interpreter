use crate::{code::code::Instructions, object::object::Object};

#[derive(Debug, Clone)]
pub struct Frame {
    pub cl: Object,
    pub ip: usize,
    pub base_pointer: usize,
}

impl Frame {
    pub fn new(cl: Object, base_pointer: usize) -> Self {
        Frame {
            cl,
            // Weird fix, I am using a usize::max_value() to indicate that the frame is new (-1)
            ip: usize::max_value(),
            base_pointer: base_pointer,
        }
    }

    pub fn instructions(&self) -> &Instructions {
        match &self.cl {
            Object::Closure { fn_obj, .. } => match &**fn_obj {
                Object::CompiledFunction { instructions, .. } => instructions,
                _ => panic!("Expected CompiledFunction, got {:?}", fn_obj),
            },
            _ => panic!("Expected Closure, got {:?}", self.cl),
        }
    }
}
