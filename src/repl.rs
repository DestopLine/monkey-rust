use std::io::{stdin, stdout, Write};

use crate::{
    evaluator::eval, lexer::Lexer, parser::Parser
};

static PROMPT: &str = "> ";

pub fn start() {
    loop {
        let mut input = String::new();
        print!("{}", PROMPT);
        let _ = stdout().flush();
        match stdin().read_line(&mut input) {
            Ok(_) => (),
            Err(e) => println!("Error: {e}"),
        }
        input = input.trim().to_string();
        if input.is_empty() {
            return;
        }
        let lexer = Lexer::new(input.clone());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        if parser.errors().len() != 0 {
            print_parser_errors(parser.errors());
            continue;
        }

        match eval(crate::ast::Node::Program(program)) {
            Ok(evaluated) => println!("{}", evaluated.inspect()),
            Err(error) => println!("{}", error.inspect()),
        }
    }
}

fn print_parser_errors(errors: &Vec<String>) {
    println!("Parser errors:");
    for msg in errors {
        println!("\t{msg}");
    }
}
