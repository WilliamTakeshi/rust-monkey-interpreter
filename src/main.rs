mod ast;
mod code;
mod compiler;
mod evaluator;
mod lexer;
mod object;
mod parser;
mod repl;
mod token;
mod vm;

fn main() {
    // let _ = repl::repl_interpreter::start();
    let _ = repl::repl_compiler::start();
}
