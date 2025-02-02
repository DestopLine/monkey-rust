use std::{iter::Peekable, str::Chars};

use crate::token::Token;

macro_rules! letter_pat {
    () => {
        'a'..='z' | 'A'..='Z' | '_'
    };
}

macro_rules! whitespace_pat {
    () => {
        ' ' | '\t' | '\n' | '\r'
    };
}

macro_rules! digit_pat {
    () => {
        '0'..='9'
    };
}

pub struct Lexer<'a> {
    source: Source<'a>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut lexer = Self {
            source: Source::new(&input),
        };
        lexer.source.read_char();
        lexer
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        let token = match self.source.current {
            None => Token::Eof,
            Some(c) => match c {
                '=' => {
                    if let Some('=') = self.source.peek() {
                        self.source.read_char();
                        Token::Equals
                    } else {
                        Token::Assign
                    }
                }
                '+' => Token::Plus,
                '-' => Token::Minus,
                '!' => {
                    if let Some('=') = self.source.peek() {
                        self.source.read_char();
                        Token::NotEquals
                    } else {
                        Token::Bang
                    }
                }
                '/' => Token::Slash,
                '*' => Token::Asterisk,
                '<' => Token::LessThan,
                '>' => Token::GreaterThan,
                ';' => Token::Semicolon,
                '(' => Token::LeftParen,
                ')' => Token::RightParen,
                ',' => Token::Comma,
                '{' => Token::LeftBrace,
                '}' => Token::RightBrace,
                letter_pat!() => return Token::from_ident(self.read_identifier()),
                digit_pat!() => return Token::Int(self.read_number()),
                _ => Token::Illegal,
            },
        };

        self.source.read_char();
        token
    }

    fn read_identifier(&mut self) -> String {
        let mut identifier = String::new();
        while let Some(c @ letter_pat!()) = self.source.current {
            identifier.push(c);
            self.source.read_char();
        }
        identifier
    }

    fn skip_whitespace(&mut self) {
        while let Some(whitespace_pat!()) = self.source.current {
            self.source.read_char();
        }
    }

    fn read_number(&mut self) -> i64 {
        let mut number = String::new();
        while let Some(c @ digit_pat!()) = self.source.current {
            number.push(c);
            self.source.read_char();
        }
        number
            .parse()
            .expect("digit_pat!() should always be a valid number")
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        match self.next_token() {
            Token::Eof => None,
            tok @ _ => Some(tok),
        }
    }
}

struct Source<'a> {
    chars: Peekable<Chars<'a>>,
    current: Option<char>,
}

impl<'a> Source<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            chars: input.chars().peekable(),
            current: None,
        }
    }

    pub fn peek(&mut self) -> Option<char> {
        self.chars.peek().map(|c| c.clone())
    }

    pub fn read_char(&mut self) {
        self.current = self.chars.next();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_next_token() {
        let input = "
            let five = 5;
            let ten = 10;

            let add = fn(x, y) {
                x + y;
            };

            let result = add(five, ten);
            !-/*5;
            5 < 10 > 5;

            if (5 < 10) {
                return true;
            } else {
                return false;
            }

            10 == 10;
            10 != 9;
        ";

        let tests = [
            Token::Let,
            Token::Ident("five".to_string()),
            Token::Assign,
            Token::Int(5),
            Token::Semicolon,
            Token::Let,
            Token::Ident("ten".to_string()),
            Token::Assign,
            Token::Int(10),
            Token::Semicolon,
            Token::Let,
            Token::Ident("add".to_string()),
            Token::Assign,
            Token::Function,
            Token::LeftParen,
            Token::Ident("x".to_string()),
            Token::Comma,
            Token::Ident("y".to_string()),
            Token::RightParen,
            Token::LeftBrace,
            Token::Ident("x".to_string()),
            Token::Plus,
            Token::Ident("y".to_string()),
            Token::Semicolon,
            Token::RightBrace,
            Token::Semicolon,
            Token::Let,
            Token::Ident("result".to_string()),
            Token::Assign,
            Token::Ident("add".to_string()),
            Token::LeftParen,
            Token::Ident("five".to_string()),
            Token::Comma,
            Token::Ident("ten".to_string()),
            Token::RightParen,
            Token::Semicolon,
            Token::Bang,
            Token::Minus,
            Token::Slash,
            Token::Asterisk,
            Token::Int(5),
            Token::Semicolon,
            Token::Int(5),
            Token::LessThan,
            Token::Int(10),
            Token::GreaterThan,
            Token::Int(5),
            Token::Semicolon,
            Token::If,
            Token::LeftParen,
            Token::Int(5),
            Token::LessThan,
            Token::Int(10),
            Token::RightParen,
            Token::LeftBrace,
            Token::Return,
            Token::True,
            Token::Semicolon,
            Token::RightBrace,
            Token::Else,
            Token::LeftBrace,
            Token::Return,
            Token::False,
            Token::Semicolon,
            Token::RightBrace,
            Token::Int(10),
            Token::Equals,
            Token::Int(10),
            Token::Semicolon,
            Token::Int(10),
            Token::NotEquals,
            Token::Int(9),
            Token::Semicolon,
            Token::Eof,
        ];

        let mut lexer = Lexer::new(input);

        for expected_token in tests.into_iter() {
            let actual_token = lexer.next_token();

            assert_eq!(actual_token, expected_token);
        }
    }
}
