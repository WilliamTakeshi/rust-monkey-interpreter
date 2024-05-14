use crate::evaluator::eval::Evaluator;
use crate::evaluator::macro_expansion::{define_macros, expand_macros};
use crate::lexer::Lexer;
use crate::parser::Parser;
use anyhow::Result;
use std::io;

const PROMPT: &'static str = ">> ";

pub fn start() -> Result<()> {
    let mut evaluator = Evaluator::new();
    loop {
        // Print prompt and flush to write it to console
        print!("{}", PROMPT);
        io::Write::flush(&mut io::stdout()).expect("flush failed!");

        // Scan Input line
        let mut buffer = String::new();
        match io::stdin().read_line(&mut buffer) {
            Err(error) => {
                println!("error: {}", error);
                return Ok(());
            }
            Ok(_) => (),
        }
        // Create Lexer
        let lexer = Lexer::new(buffer);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        let env = evaluator.environment.borrow().clone();
        let (program, env) = define_macros(program, env);
        let expanded = expand_macros(program, env.clone());
        evaluator = Evaluator::new_with_env(env);
        let outcome = evaluator.eval_program(expanded);
        println!("{:?}", outcome);
    }
}
