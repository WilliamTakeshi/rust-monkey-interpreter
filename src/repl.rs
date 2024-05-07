use std::io;
use anyhow::Result;
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::eval::Evaluator;

const PROMPT: &'static str = ">>";

pub fn start() -> Result<()> {

    while true {
        let mut buffer = String::new();
        io::stdin().read_line(&mut buffer)?;

        let mut lexer = Lexer::new(buffer.clone());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
    
        let mut evaluator = Evaluator::new();
    
        let object = evaluator.eval_program(program);
    
        println!("{object:?}");
    }

    Ok(())
}
