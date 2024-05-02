// const ILLEGAL: &'static str = "ILLEGAL";

#[derive(PartialEq, Eq, Debug)]
pub enum Token {
    Illegal,
    Eof,

    // Identifiers (add, foobar, x, y)
    Ident(String),

    // Integer
    Int(u64),

    // Operators
    Assign,
    Plus,

    // Delimiters
    Comma,
    Semicolon,

    Lparen,
    Rparen,
    Lbrace,
    Rbrace,

    // Keywords
    Function,
    Let,
}
