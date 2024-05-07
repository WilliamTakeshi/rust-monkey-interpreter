mod token;
mod lexer;
mod repl;
mod ast;
mod parser;
mod object;
mod eval;

fn main() {
    let _ = repl::start();
}
