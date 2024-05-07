#[derive(Clone, Debug, PartialEq)]
pub enum Object {
    Boolean(bool),
    Integer(i64),
    Null,
    Return(Box<Object>),
    Err(String),
}


impl Object {
    pub fn obj_type(&self) -> String {
        match &self {
            Self::Boolean(_) => String::from("BOOLEAN"),
            Self::Integer(_) => String::from("INTEGER"),
            Self::Null => String::from("NULL"),
            Self::Return(_) => String::from("RETURN"),
            Self::Err(_) => String::from("ERROR"),
        }
    }
}