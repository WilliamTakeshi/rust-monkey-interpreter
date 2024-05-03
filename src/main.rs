mod token;
mod lexer;
mod repl;
mod ast;
mod parser;

fn main() {
    let _ = repl::start();
}
