use std::io::{self, Write};

use crate::lexer::Lexer;

pub fn start() {
    let mut input = String::new();

    loop {
        print!("> ");
        io::stdout().flush().unwrap();
        let result = io::stdin().read_line(&mut input);

        if result.is_ok() && !input.is_empty() {
            let lexer = Lexer::new(&input);

            for tok in lexer {
                println!("{tok:?}")
            }
        }

        input.clear();
    }
}
