mod token;
mod lexer;
mod repl;
mod ast;
mod parser;
mod obj;
mod eval;
mod env;

fn main() {
    repl::start()
}
