use std::io::{stdin, stdout, Write};

use crate::{
    lexer::Lexer,
    parser::Parser,
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
        if input.len() < 2 {
            return;
        }
        let lexer = Lexer::new(input.clone());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        if parser.errors().len() != 0 {
            print_parser_errors(parser.errors());
            continue;
        }

        println!("{program:#?}");
    }
}

fn print_parser_errors(errors: &Vec<String>) {
    println!("Parser errors:");
    for msg in errors {
        println!("\t{msg}");
    }
}
