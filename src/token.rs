// const ILLEGAL: &'static str = "ILLEGAL";

#[derive(PartialEq, Eq, Debug)]
pub enum Token {
    Illegal,
    Eof,

    // Identifiers (add, foobar, x, y)
    Ident,

    // Integer
    Int,

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
