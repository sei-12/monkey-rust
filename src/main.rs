mod token;
mod lexer;
mod repl;
mod ast;
mod parser;
mod obj;
mod eval;

fn main() {
    repl::start()
}
