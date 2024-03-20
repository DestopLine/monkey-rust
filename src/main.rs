use crate::repl::repl::start;



pub mod lexer;
pub mod repl;
pub mod token;

fn main() {
    println!("Welcome to the Monkey programming language!");
    start();
}
