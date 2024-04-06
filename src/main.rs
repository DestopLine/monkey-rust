use crate::repl::start;

mod ast;
mod builtins;
mod environment;
mod evaluator;
mod lexer;
mod object;
mod parser;
mod repl;
mod token;

fn main() {
    println!("Welcome to the Monkey programming language!");
    start();
}
