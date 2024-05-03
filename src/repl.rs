use std::io;
use anyhow::Result;
use crate::lexer::Lexer;
use crate::token::Token;

const PROMPT: &'static str = ">>";

pub fn start() -> Result<()> {
    let mut buffer = String::new();
    io::stdin().read_line(&mut buffer)?;

    let mut lexer = Lexer::new(buffer);
    let mut token = lexer.next_token();
    while token != Token::Eof {
        print!("{PROMPT}");
        println!("{token:?}");
        token = lexer.next_token();
    }
    Ok(())
}
