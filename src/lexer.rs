use crate::token;
use crate::token::Token;
use std::num::ParseIntError;
use std::str::FromStr;
struct Lexer {
    input: String,
    position: usize,      // current position in input (point to current char)
    read_position: usize, // current reading position in input (after current char)
    ch: Option<char>,     // current char under examination
}

impl Lexer {
    fn new(input: String) -> Self {
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

    fn next_token(&mut self) -> Token {
        self.skip_whitespace();
        dbg!(&self.position);
        dbg!(&self.read_position);
        dbg!(&self.ch);

        let token: Token = match self.ch {
            Some('=') => Token::Assign,
            Some(';') => Token::Semicolon,
            Some('(') => Token::Lparen,
            Some(')') => Token::Rparen,
            Some(',') => Token::Comma,
            Some('+') => Token::Plus,
            Some('{') => Token::Lbrace,
            Some('}') => Token::Rbrace,
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
        if token == Token::Lparen
            || token == Token::Assign
            || token == Token::Semicolon
            || token == Token::Rparen
            || token == Token::Comma
            || token == Token::Plus
            || token == Token::Lbrace
            || token == Token::Rbrace
            || token == Token::Eof
        {
            self.read_char();
        }
        token
    }

    fn read_identifier(&mut self) -> &str {
        let position = self.position;
        while is_letter(self.ch) {
            self.read_char();
        }
        // dbg!(&position);
        // dbg!(&self.position);
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
        while is_digit(self.ch) {
            self.read_char();
        }
        dbg!(&self.input[position..self.position]);
        u64::from_str(&self.input[position..self.position])
    }
}

fn is_letter(ch: Option<char>) -> bool {
    ch.map(|c| 'a' <= c && c <= 'z' || 'A' <= c && c <= 'Z' || c == '_')
        .unwrap_or_else(|| false)
}

fn is_digit(ch: Option<char>) -> bool {
    ch.map(|c| '0' <= c && c <= '9').unwrap_or_else(|| false)
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
            "let five = 5;
            let ten = 10;
            let add = fn(x, y) {
            x + y;
            };
            let result = add(five, ten);",
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
            Token::Eof,
        ];

        let mut lexer = Lexer::new(input);

        for expected_token in expected_response {
            let token = lexer.next_token();

            assert_eq!(token, expected_token);
        }
    }
}
