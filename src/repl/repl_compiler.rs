use crate::compiler::compiler::Compiler;
use crate::evaluator::eval::Evaluator;
use crate::evaluator::macro_expansion::{define_macros, expand_macros};
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::vm::vm::Vm;
use anyhow::Result;
use std::io;

const PROMPT: &str = ">> ";

pub fn start() -> Result<()> {
    let mut evaluator = Evaluator::new();
    loop {
        // Print prompt and flush to write it to console
        print!("{}", PROMPT);
        io::Write::flush(&mut io::stdout()).expect("flush failed!");

        // Scan Input line
        let mut buffer = String::new();

        if let Err(error) = io::stdin().read_line(&mut buffer) {
            println!("error: {}", error);
            return Ok(());
        }

        // Create Lexer
        let lexer = Lexer::new(buffer);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        let mut compiler = Compiler::new();
        compiler.compile_program(program);

        let mut vm = Vm::new(compiler.bytecode());
        vm.run()?;

        let stack_top = vm.stack_top();
        println!("{:?}", stack_top);
    }
}
