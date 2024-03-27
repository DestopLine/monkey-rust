use std::io::{stdin, stdout, Write};

use crate::{
    lexer::Lexer,
    token::{Token, TokenType},
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
        let mut lexer = Lexer::new(input.clone());
        loop {
            let tok = lexer.next_token();
            match tok {
                Token {
                    toktype: TokenType::EOF,
                    literal: _,
                } => break,
                _ => println!("{:?}", tok),
            }
        }
    }
}
