use crate::repl::start;

pub mod ast;
pub mod lexer;
pub mod parser;
pub mod repl;
pub mod token;

fn main() {
    println!("Welcome to the Monkey programming language!");
    start();
}
