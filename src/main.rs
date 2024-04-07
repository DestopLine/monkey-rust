use std::io::ErrorKind;

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
    let args = std::env::args().collect::<Vec<_>>();
    let Some(path) = args.get(1) else {
        println!("Welcome to the Monkey programming language!");
        repl::start();
        return
    };

    match std::fs::read_to_string(path) {
        Ok(src) => {
            let lexer = lexer::Lexer::new(src);
            let mut parser = parser::Parser::new(lexer);
            let program = parser.parse_program();

            if parser.errors().len() != 0 {
                repl::print_parser_errors(parser.errors());
                return;
            }

            let env = environment::new_env();
            let res = evaluator::eval(&program, &env);

            if let Err(error) = res {
                eprintln!("{error}");
            }
        }
        Err(e) => {
            let error_msg = match e.kind() {
                ErrorKind::NotFound => "file does not exist",
                ErrorKind::PermissionDenied => "missing permissions",
                ErrorKind::InvalidInput => "invalid file path",
                err @ _ => {
                    eprintln!("Unexpected error: {err:?}");
                    return;
                },
            };
            eprintln!("Could not open the specified file: {error_msg}");
        }
    }
}
