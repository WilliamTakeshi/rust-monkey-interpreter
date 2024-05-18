mod ast;
mod evaluator;
mod lexer;
mod object;
mod parser;
mod repl;
mod token;
mod code;
mod compiler;
mod vm;

fn main() {
    let _ = repl::start();
}
