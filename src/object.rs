#[derive(Debug, PartialEq)]
pub enum Object {
    Boolean(bool),
    Integer(i64),
    Null,
    Return(Box<Object>),
}