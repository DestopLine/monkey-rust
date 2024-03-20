use crate::token::token::{lookup_ident, Token, TokenType};

fn is_letter(ch: &char) -> bool {
    matches!(ch, 'a'..='z' | 'A'..='Z' | '_')
}

fn is_digit(ch: &char) -> bool {
    matches!(ch, '0'..='9')
}

pub struct Lexer {
    input: String,
    position: usize,
    read_position: usize,
    ch: char,
}

impl Lexer {
    pub fn new(input: String) -> Self {
        let mut lexer = Self {
            input,
            position: 0,
            read_position: 1,
            ch: '\0',
        };
        lexer.ch = lexer.input.chars().next().unwrap();
        lexer
    }

    pub fn read_char(&mut self) {
        self.ch = if self.read_position >= self.input.len() {
            '\0'
        } else {
            self.input.chars().nth(self.read_position).unwrap()
        };
        self.position = self.read_position;
        self.read_position += 1;
    }

    pub fn read_identifier(&mut self) -> String {
        let position = self.position;
        while is_letter(&self.ch) {
            self.read_char();
        }
        self.input[position..self.position].to_string()
    }

    fn read_number(&mut self) -> String {
        let position = self.position;
        while is_digit(&self.ch) {
            self.read_char();
        }
        self.input[position..self.position].to_string()
    }

    fn skip_whitespace(&mut self) {
        while matches!(self.ch, ' ' | '\t' | '\n' | '\r') {
            self.read_char();
        }
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        let mut literal = self.ch.to_string();

        let toktype = match self.ch {
            '=' => TokenType::Assign,
            ';' => TokenType::Semicolon,
            '(' => TokenType::OpenParen,
            ')' => TokenType::CloseParen,
            ',' => TokenType::Comma,
            '+' => TokenType::Plus,
            '{' => TokenType::OpenBrace,
            '}' => TokenType::CloseBrace,
            '\0' => {
                literal = "".to_string();
                TokenType::EOF
            }
            _ => {
                if is_letter(&self.ch) {
                    literal = self.read_identifier();
                    return Token::new(lookup_ident(&literal), literal);
                } else if is_digit(&self.ch) {
                    literal = self.read_number();
                    return Token::new(TokenType::Int, literal);
                } else {
                    TokenType::Illegal
                }
            }
        };

        self.read_char();
        Token::new(toktype, literal)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_is_letter() {
        assert!(is_letter(&'a'));
        assert!(is_letter(&'h'));
        assert!(is_letter(&'z'));
        assert!(is_letter(&'A'));
        assert!(is_letter(&'Q'));
        assert!(is_letter(&'Z'));
        assert!(is_letter(&'_'));
        assert!(!is_letter(&'á'));
        assert!(!is_letter(&'8'));
        assert!(!is_letter(&'='));
    }

    #[test]
    fn test_is_digit() {
        assert!(is_digit(&'0'));
        assert!(is_digit(&'1'));
        assert!(is_digit(&'6'));
        assert!(is_digit(&'9'));
        assert!(!is_digit(&'²'));
        assert!(!is_digit(&'{'));
        assert!(!is_digit(&'-'));
        assert!(!is_digit(&'j'));
    }

    #[test]
    fn single_char_tokens() {
        let input = "=+(){},;";

        let tests = [
            Token::new(TokenType::Assign, "=".to_string()),
            Token::new(TokenType::Plus, "+".to_string()),
            Token::new(TokenType::OpenParen, "(".to_string()),
            Token::new(TokenType::CloseParen, ")".to_string()),
            Token::new(TokenType::OpenBrace, "{".to_string()),
            Token::new(TokenType::CloseBrace, "}".to_string()),
            Token::new(TokenType::Comma, ",".to_string()),
            Token::new(TokenType::Semicolon, ";".to_string()),
        ];

        let mut l = Lexer::new(input.to_string());

        for expected in tests {
            let tok = l.next_token();
            assert_eq!(tok.toktype, expected.toktype);
            assert_eq!(tok.literal, expected.literal);
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
            Token::new(TokenType::Let, "let".to_string()),
            Token::new(TokenType::Ident, "five".to_string()),
            Token::new(TokenType::Assign, "=".to_string()),
            Token::new(TokenType::Int, "5".to_string()),
            Token::new(TokenType::Semicolon, ";".to_string()),
            Token::new(TokenType::Let, "let".to_string()),
            Token::new(TokenType::Ident, "ten".to_string()),
            Token::new(TokenType::Assign, "=".to_string()),
            Token::new(TokenType::Int, "10".to_string()),
            Token::new(TokenType::Semicolon, ";".to_string()),
            Token::new(TokenType::Let, "let".to_string()),
            Token::new(TokenType::Ident, "add".to_string()),
            Token::new(TokenType::Assign, "=".to_string()),
            Token::new(TokenType::Function, "fn".to_string()),
            Token::new(TokenType::OpenParen, "(".to_string()),
            Token::new(TokenType::Ident, "x".to_string()),
            Token::new(TokenType::Comma, ",".to_string()),
            Token::new(TokenType::Ident, "y".to_string()),
            Token::new(TokenType::CloseParen, ")".to_string()),
            Token::new(TokenType::OpenBrace, "{".to_string()),
            Token::new(TokenType::Ident, "x".to_string()),
            Token::new(TokenType::Plus, "+".to_string()),
            Token::new(TokenType::Ident, "y".to_string()),
            Token::new(TokenType::Semicolon, ";".to_string()),
            Token::new(TokenType::CloseBrace, "}".to_string()),
            Token::new(TokenType::Semicolon, ";".to_string()),
            Token::new(TokenType::Let, "let".to_string()),
            Token::new(TokenType::Ident, "result".to_string()),
            Token::new(TokenType::Assign, "=".to_string()),
            Token::new(TokenType::Ident, "add".to_string()),
            Token::new(TokenType::OpenParen, "(".to_string()),
            Token::new(TokenType::Ident, "five".to_string()),
            Token::new(TokenType::Comma, ",".to_string()),
            Token::new(TokenType::Ident, "ten".to_string()),
            Token::new(TokenType::CloseParen, ")".to_string()),
            Token::new(TokenType::Semicolon, ";".to_string()),
            Token::new(TokenType::EOF, "".to_string()),
        ];

        let mut l = Lexer::new(input.to_string());

        for expected in tests {
            let tok = l.next_token();
            assert_eq!(tok.toktype, expected.toktype);
            assert_eq!(tok.literal, expected.literal);
            println!("{}", tok.literal);
        }
    }
}
