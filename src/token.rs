use phf::phf_map;

static KEYWORDS: phf::Map<&'static str, Token> = phf_map! {
    "fn" => Token::Function,
    "let" => Token::Let,
    "true" => Token::True,
    "false" => Token::False,
    "if" => Token::If,
    "else" => Token::Else,
    "return" => Token::Return,
    "==" => Token::Eq,
    "!=" => Token::Neq,
    "macro" => Token::Macro,
};

pub fn lookup_ident(ident: &str) -> Token {
    if let Some(tok) = KEYWORDS.get(ident).cloned() {
        tok
    } else {
        Token::Ident(ident.to_owned())
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Token {
    Illegal,
    Eof,

    // Identifiers (add, foobar, x, y)
    Ident(String),

    // Integer
    Int(u64),

    // String
    String(String),

    // Array
    Lbracket,
    Rbracket,

    // Operators
    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,
    Lt,
    Gt,

    // Delimiters
    Comma,
    Semicolon,
    Colon,

    Lparen,
    Rparen,
    Lbrace,
    Rbrace,

    // Keywords
    Function,
    Let,
    True,
    False,
    If,
    Else,
    Return,
    Eq,
    Neq,
    Macro,
}
