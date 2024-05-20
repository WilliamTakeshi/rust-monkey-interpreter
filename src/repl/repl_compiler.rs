use crate::compiler::compiler::Compiler;
use crate::compiler::symbol_table::SymbolTable;
use crate::lexer::Lexer;
use crate::object::object::Object;
use crate::parser::Parser;
use crate::vm::vm::{Vm, GLOBAL_SIZE, NULL};
use anyhow::Result;
use std::cell::RefCell;
use std::io;
use std::rc::Rc;

const PROMPT: &str = ">> ";

pub fn start() -> Result<()> {
    let mut constants: Vec<Object> = vec![];
    let mut globals = Rc::new(RefCell::new([NULL; GLOBAL_SIZE]));
    let symbol_table = Rc::new(RefCell::new(SymbolTable::new()));
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

        let mut compiler = Compiler::new_with_state(symbol_table.clone(), constants.clone());
        let result = compiler.compile_program(program);

        if let Err(err) = result {
            println!("Woops! Compilation failed: {}", err);
            continue;
        }

        let code = compiler.bytecode();
        constants = code.clone().constants;

        let mut vm = Vm::new_with_global_store(code, globals.clone());

        vm.run()?;

        let stack_top = vm.last_popped_stack_elem();
        println!("{:?}", stack_top);
    }
}
