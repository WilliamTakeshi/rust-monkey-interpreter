use crate::token;
use crate::token::Token;
use std::num::ParseIntError;
use std::str::FromStr;
#[derive(Debug)]
pub struct Lexer {
    input: String,
    position: usize,      // current position in input (point to current char)
    read_position: usize, // current reading position in input (after current char)
    ch: Option<char>,     // current char under examination
}

impl Lexer {
    pub fn new(input: String) -> Self {
        let mut l = Lexer {
            input,
            position: 0,
            read_position: 0,
            ch: None,
        };

        l.read_char();
        l
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = None;
        } else {
            self.ch = self.input.chars().nth(self.read_position);
        }
        self.position = self.read_position;
        self.read_position += 1;
    }

    fn peek_char(&self) -> Option<char> {
        if self.read_position >= self.input.len() {
            None
        } else {
            self.input.chars().nth(self.read_position)
        }
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        let token: Token = match self.ch {
            Some('=') => {
                if self.peek_char() == Some('=') {
                    self.read_char();
                    Token::Eq
                } else {
                    Token::Assign
                }
            }
            Some('!') => {
                if self.peek_char() == Some('=') {
                    self.read_char();
                    Token::Neq
                } else {
                    Token::Bang
                }
            }
            Some(';') => Token::Semicolon,
            Some('(') => Token::Lparen,
            Some(')') => Token::Rparen,
            Some(',') => Token::Comma,
            Some('+') => Token::Plus,
            Some('{') => Token::Lbrace,
            Some('}') => Token::Rbrace,
            Some('-') => Token::Minus,
            Some('*') => Token::Asterisk,
            Some('/') => Token::Slash,
            Some('<') => Token::Lt,
            Some('>') => Token::Gt,
            Some('[') => Token::Lbracket,
            Some(']') => Token::Rbracket,
            Some(':') => Token::Colon,
            Some('"') => Token::String(String::from(self.read_string())),
            None => Token::Eof,
            Some(c) => {
                if is_letter(Some(c)) {
                    token::lookup_ident(self.read_identifier())
                } else if is_digit(Some(c)) {
                    if let Ok(number) = self.read_number() {
                        Token::Int(number)
                    } else {
                        Token::Illegal
                    }
                } else {
                    Token::Illegal
                }
            }
        };

        self.read_char();
        token
    }

    fn read_identifier(&mut self) -> &str {
        let position = self.position;
        while is_letter(self.peek_char()) {
            self.read_char();
        }
        &self.input[position..=self.position]
    }

    fn read_string(&mut self) -> &str {
        let position = self.position + 1;

        loop {
            self.read_char();
            if self.ch == Some('"') || self.ch.is_none() {
                break;
            }
        }

        &self.input[position..self.position]
    }

    fn skip_whitespace(&mut self) {
        while self.ch == Some(' ')
            || self.ch == Some('\t')
            || self.ch == Some('\n')
            || self.ch == Some('\r')
        {
            self.read_char()
        }
    }

    fn read_number(&mut self) -> Result<u64, ParseIntError> {
        let position = self.position;
        while is_digit(self.peek_char()) {
            self.read_char();
        }
        u64::from_str(&self.input[position..=self.position])
    }
}

fn is_letter(ch: Option<char>) -> bool {
    ch.map(|c| c.is_ascii_lowercase() || c.is_ascii_uppercase() || c == '_')
        .unwrap_or_else(|| false)
}

fn is_digit(ch: Option<char>) -> bool {
    ch.map(|c| c.is_ascii_digit()).unwrap_or_else(|| false)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_next_token() {
        let input = String::from("=+(){},;");
        let expected_response = vec![
            Token::Assign,
            Token::Plus,
            Token::Lparen,
            Token::Rparen,
            Token::Lbrace,
            Token::Rbrace,
            Token::Comma,
            Token::Semicolon,
            Token::Eof,
        ];

        let mut lexer = Lexer::new(input);

        for expected_token in expected_response {
            let token = lexer.next_token();

            assert_eq!(token, expected_token);
        }
    }

    #[test]
    fn test_next_token_2() {
        let input = String::from(
            r#"let five = 5;
            let ten = 10;

            let add = fn(x, y) {
            x + y;
            };

            let result = add(five, ten);
            !-/*5;
            5 < 10 > 5;
            
            if (5 < 10) {
                return true;
            } else {
                return false;
            }
            
            10 == 10;
            10 != 9;
            "foobar"
            "foo bar"
            [1, 2];
            {"foo": "bar"}
            macro(x, y) { x + y; };
            "#,
        );
        let expected_response = vec![
            Token::Let,
            Token::Ident(String::from("five")),
            Token::Assign,
            Token::Int(5),
            Token::Semicolon,
            Token::Let,
            Token::Ident(String::from("ten")),
            Token::Assign,
            Token::Int(10),
            Token::Semicolon,
            Token::Let,
            Token::Ident(String::from("add")),
            Token::Assign,
            Token::Function,
            Token::Lparen,
            Token::Ident(String::from("x")),
            Token::Comma,
            Token::Ident(String::from("y")),
            Token::Rparen,
            Token::Lbrace,
            Token::Ident(String::from("x")),
            Token::Plus,
            Token::Ident(String::from("y")),
            Token::Semicolon,
            Token::Rbrace,
            Token::Semicolon,
            Token::Let,
            Token::Ident(String::from("result")),
            Token::Assign,
            Token::Ident(String::from("add")),
            Token::Lparen,
            Token::Ident(String::from("five")),
            Token::Comma,
            Token::Ident(String::from("ten")),
            Token::Rparen,
            Token::Semicolon,
            Token::Bang,
            Token::Minus,
            Token::Slash,
            Token::Asterisk,
            Token::Int(5),
            Token::Semicolon,
            Token::Int(5),
            Token::Lt,
            Token::Int(10),
            Token::Gt,
            Token::Int(5),
            Token::Semicolon,
            Token::If,
            Token::Lparen,
            Token::Int(5),
            Token::Lt,
            Token::Int(10),
            Token::Rparen,
            Token::Lbrace,
            Token::Return,
            Token::True,
            Token::Semicolon,
            Token::Rbrace,
            Token::Else,
            Token::Lbrace,
            Token::Return,
            Token::False,
            Token::Semicolon,
            Token::Rbrace,
            Token::Int(10),
            Token::Eq,
            Token::Int(10),
            Token::Semicolon,
            Token::Int(10),
            Token::Neq,
            Token::Int(9),
            Token::Semicolon,
            Token::String(String::from("foobar")),
            Token::String(String::from("foo bar")),
            Token::Lbracket,
            Token::Int(1),
            Token::Comma,
            Token::Int(2),
            Token::Rbracket,
            Token::Semicolon,
            Token::Lbrace,
            Token::String(String::from("foo")),
            Token::Colon,
            Token::String(String::from("bar")),
            Token::Rbrace,
            Token::Macro,
            Token::Lparen,
            Token::Ident(String::from("x")),
            Token::Comma,
            Token::Ident(String::from("y")),
            Token::Rparen,
            Token::Lbrace,
            Token::Ident(String::from("x")),
            Token::Plus,
            Token::Ident(String::from("y")),
            Token::Semicolon,
            Token::Rbrace,
            Token::Semicolon,
            Token::Eof,
        ];

        let mut lexer = Lexer::new(input);

        for expected_token in expected_response {
            let token = lexer.next_token();

            assert_eq!(token, expected_token);
        }
    }
}
