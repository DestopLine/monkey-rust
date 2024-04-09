use std::{
    iter::{Enumerate, Peekable},
    str::Chars,
};

use crate::token::{lookup_ident, Token, TokenKind};

macro_rules! letter {
    () => {
        'a'..='z' | 'A'..='Z' | '_'
    };
}

macro_rules! digit {
    () => {
        '0'..='9'
    };
}

macro_rules! whitespace {
    () => {
        ' ' | '\t' | '\n' | '\r'
    };
}

struct Source<'a> {
    it: Peekable<Enumerate<Chars<'a>>>,
    pos: usize,
    ch: char,
}

impl<'a> Source<'a> {
    fn new(src: &'a str) -> Self {
        Self {
            it: src.chars().enumerate().peekable().into_iter(),
            pos: 0,
            ch: '\0',
        }
    }

    fn pos(&self) -> usize {
        self.pos
    }

    fn ch(&self) -> char {
        self.ch
    }

    fn peek_ch(&mut self) -> Option<char> {
        let (_, ch) = self.it.peek()?;
        Some(*ch)
    }
}

impl<'a> Iterator for Source<'a> {
    type Item = (usize, char);

    fn next(&mut self) -> Option<Self::Item> {
        let next = self.it.next()?;
        (self.pos, self.ch) = next;
        Some(next)
    }
}

pub struct Lexer<'a> {
    input: &'a str,
    src: Source<'a>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            input,
            src: Source::new(input),
        }
    }

    fn read_identifier(&mut self) -> String {
        let mut ident = String::from(self.src.ch());
        while let Some(ch @ letter!()) = self.src.peek_ch() {
            ident.push(ch);
            self.src.next();
        }
        ident
    }

    fn read_number(&mut self) -> String {
        let mut num = String::from(self.src.ch());
        while let Some(ch @ digit!()) = self.src.peek_ch() {
            num.push(ch);
            self.src.next();
        }
        num
    }

    fn read_string(&mut self) -> String {
        let mut string = String::new();
        loop {
            self.src.next();
            match self.src.ch() {
                '"' => break,
                '\0' => todo!(),
                '\\' => {
                    self.src.next();
                    match self.src.ch() {
                        'n' => string.push('\n'),
                        't' => string.push('\t'),
                        'r' => string.push('\r'),
                        '"' => string.push('"'),
                        '\\' => string.push('\\'),
                        _ => todo!(),
                    }
                }
                c @ _ => string.push(c),
            }
        }
        string
    }

    fn skip_whitespace(&mut self) {
        while let Some(whitespace!()) = self.src.peek_ch() {
            self.src.next();
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.skip_whitespace();

        let (start, ch) = self.src.next()?;

        let kind = match ch {
            '=' => match self.src.peek_ch()? {
                '=' => {
                    self.src.next();
                    TokenKind::Eq
                }
                _ => TokenKind::Assign,
            },
            '!' => match self.src.peek_ch()? {
                '=' => {
                    self.src.next();
                    TokenKind::NotEq
                }
                _ => TokenKind::Bang,
            },
            '+' => TokenKind::Plus,
            '-' => TokenKind::Minus,
            '*' => TokenKind::Asterisk,
            '/' => TokenKind::Slash,
            '<' => TokenKind::Lt,
            '>' => TokenKind::Gt,
            '(' => TokenKind::OpenParen,
            ')' => TokenKind::CloseParen,
            '[' => TokenKind::OpenBracket,
            ']' => TokenKind::CloseBracket,
            '{' => TokenKind::OpenBrace,
            '}' => TokenKind::CloseBrace,
            ',' => TokenKind::Comma,
            ':' => TokenKind::Colon,
            ';' => TokenKind::Semicolon,
            '"' => TokenKind::String(self.read_string()),
            letter!() => lookup_ident(&self.read_identifier()),
            digit!() => TokenKind::Int(self.read_number().parse().unwrap()),
            _ => TokenKind::Illegal,
        };

        let end = self.src.pos() + 1;
        Some(Token::new(kind, start, end))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn single_char_tokens() {
        let input = "=+(){},;";

        let tests = [
            Token::new(TokenKind::Assign, 0, 1),
            Token::new(TokenKind::Plus, 1, 2),
            Token::new(TokenKind::OpenParen, 2, 3),
            Token::new(TokenKind::CloseParen, 3, 4),
            Token::new(TokenKind::OpenBrace, 4, 5),
            Token::new(TokenKind::CloseBrace, 5, 6),
            Token::new(TokenKind::Comma, 6, 7),
            Token::new(TokenKind::Semicolon, 7, 8),
        ];

        let mut lexer = Lexer::new(input);

        for expected in tests {
            let token = lexer.next().unwrap();
            assert_eq!(token.kind, expected.kind);
            assert_eq!(token.range, expected.range);
        }
    }

    #[test]
    fn multi_char_tokens() {
        let input = "let five = 5;
let ten = 10;

let add = fn(x, y) {
    x + y;
};

let result = add(five, ten);
";
        let tests = [
            Token::new(TokenKind::Let, 0, 3),
            Token::new(TokenKind::Ident("five".to_string()), 4, 8),
            Token::new(TokenKind::Assign, 9, 10),
            Token::new(TokenKind::Int(5), 11, 12),
            Token::new(TokenKind::Semicolon, 12, 13),
            Token::new(TokenKind::Let, 14, 17),
            Token::new(TokenKind::Ident("ten".to_string()), 18, 21),
            Token::new(TokenKind::Assign, 22, 23),
            Token::new(TokenKind::Int(10), 24, 26),
            Token::new(TokenKind::Semicolon, 26, 27),
            Token::new(TokenKind::Let, 29, 32),
            Token::new(TokenKind::Ident("add".to_string()), 33, 36),
            Token::new(TokenKind::Assign, 37, 38),
            Token::new(TokenKind::Function, 39, 41),
            Token::new(TokenKind::OpenParen, 41, 42),
            Token::new(TokenKind::Ident("x".to_string()), 42, 43),
            Token::new(TokenKind::Comma, 43, 44),
            Token::new(TokenKind::Ident("y".to_string()), 45, 46),
            Token::new(TokenKind::CloseParen, 46, 47),
            Token::new(TokenKind::OpenBrace, 48, 49),
            Token::new(TokenKind::Ident("x".to_string()), 54, 55),
            Token::new(TokenKind::Plus, 56, 57),
            Token::new(TokenKind::Ident("y".to_string()), 58, 59),
            Token::new(TokenKind::Semicolon, 59, 60),
            Token::new(TokenKind::CloseBrace, 61, 62),
            Token::new(TokenKind::Semicolon, 62, 63),
            Token::new(TokenKind::Let, 65, 68),
            Token::new(TokenKind::Ident("result".to_string()), 69, 75),
            Token::new(TokenKind::Assign, 76, 77),
            Token::new(TokenKind::Ident("add".to_string()), 78, 81),
            Token::new(TokenKind::OpenParen, 81, 82),
            Token::new(TokenKind::Ident("five".to_string()), 82, 86),
            Token::new(TokenKind::Comma, 86, 87),
            Token::new(TokenKind::Ident("ten".to_string()), 88, 91),
            Token::new(TokenKind::CloseParen, 91, 92),
            Token::new(TokenKind::Semicolon, 92, 93),
        ];

        let mut lexer = Lexer::new(input);

        for expected in tests {
            let token = lexer.next().unwrap();
            assert_eq!(token.kind, expected.kind);
            assert_eq!(token.range, expected.range);
        }
    }

        #[test]
        fn more_tokens() {
            let input = r#"!-/*5;
5 < 10 > 5;

if (5 < 10) {
    return true;
} else {
    return false;
}
[1, 2];
{"foo": "bar"}"#;
            let tests = [
                Token::new(TokenKind::Bang, 0, 1),
                Token::new(TokenKind::Minus, 1, 2),
                Token::new(TokenKind::Slash, 2, 3),
                Token::new(TokenKind::Asterisk, 3, 4),
                Token::new(TokenKind::Int(5), 4, 5),
                Token::new(TokenKind::Semicolon, 5, 6),
                Token::new(TokenKind::Int(5), 7, 8),
                Token::new(TokenKind::Lt, 9, 10),
                Token::new(TokenKind::Int(10), 11, 13),
                Token::new(TokenKind::Gt, 14, 15),
                Token::new(TokenKind::Int(5), 16, 17),
                Token::new(TokenKind::Semicolon, 17, 18),
                Token::new(TokenKind::If, 20, 22),
                Token::new(TokenKind::OpenParen, 23, 24),
                Token::new(TokenKind::Int(5), 24, 25),
                Token::new(TokenKind::Lt, 26, 27),
                Token::new(TokenKind::Int(10), 28, 30),
                Token::new(TokenKind::CloseParen, 30, 31),
                Token::new(TokenKind::OpenBrace, 32, 33),
                Token::new(TokenKind::Return, 38, 44),
                Token::new(TokenKind::True, 45, 49),
                Token::new(TokenKind::Semicolon, 49, 50),
                Token::new(TokenKind::CloseBrace, 51, 52),
                Token::new(TokenKind::Else, 53, 57),
                Token::new(TokenKind::OpenBrace, 58, 59),
                Token::new(TokenKind::Return, 64, 70),
                Token::new(TokenKind::False, 71, 76),
                Token::new(TokenKind::Semicolon, 76, 77),
                Token::new(TokenKind::CloseBrace, 78, 79),
                Token::new(TokenKind::OpenBracket, 80, 81),
                Token::new(TokenKind::Int(1), 81, 82),
                Token::new(TokenKind::Comma, 82, 83),
                Token::new(TokenKind::Int(2), 84, 85),
                Token::new(TokenKind::CloseBracket, 85, 86),
                Token::new(TokenKind::Semicolon, 86, 87),
                Token::new(TokenKind::OpenBrace, 88, 89),
                Token::new(TokenKind::String(String::from("foo")), 89, 94),
                Token::new(TokenKind::Colon, 94, 95),
                Token::new(TokenKind::String(String::from("bar")), 96, 101),
                Token::new(TokenKind::CloseBrace, 101, 102),
            ];

            let mut lexer = Lexer::new(input);

            for expected in tests {
                let token = lexer.next().unwrap();
                assert_eq!(token.kind, expected.kind);
                assert_eq!(token.range, expected.range);
            }
        }

        #[test]
        fn double_char_tokens() {
            let input = "10 == 10;\n10 != 9;";
            let tests = [
                Token::new(TokenKind::Int(10), 0, 2),
                Token::new(TokenKind::Eq, 3, 5),
                Token::new(TokenKind::Int(10), 6, 8),
                Token::new(TokenKind::Semicolon, 8, 9),
                Token::new(TokenKind::Int(10), 10, 12),
                Token::new(TokenKind::NotEq, 13, 15),
                Token::new(TokenKind::Int(9), 16, 17),
                Token::new(TokenKind::Semicolon, 17, 18),
            ];

            let mut lexer = Lexer::new(input);

            for expected in tests {
                let token = lexer.next().unwrap();
                assert_eq!(token.kind, expected.kind);
                assert_eq!(token.range, expected.range);
            }
        }

        #[test]
        fn string_tokens() {
            let input = r#""foobar" "foo bar""#;

            let tests = [
                Token::new(TokenKind::String(String::from("foobar")), 0, 8),
                Token::new(TokenKind::String(String::from("foo bar")), 9, 18),
            ];

            let mut lexer = Lexer::new(input);

            for expected in tests {
                let token = lexer.next().unwrap();
                assert_eq!(token, expected);
            }
        }

        #[test]
        fn string_escapes() {
            let input = r#""foo\nbar"
"\t\tfoo"
"foo \"bar\" foo"
"foo\\bar""#;

            let tests = [
                Token::new(TokenKind::String(String::from("foo\nbar")), 0, 10),
                Token::new(TokenKind::String(String::from("\t\tfoo")), 11, 20),
                Token::new(TokenKind::String(String::from("foo \"bar\" foo")), 21, 38),
                Token::new(TokenKind::String(String::from("foo\\bar")), 39, 49),
            ];

            let mut lexer = Lexer::new(input);

            for expected in tests {
                let token = lexer.next().unwrap();
                assert_eq!(token, expected);
            }
        }
}
