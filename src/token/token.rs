#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
    Illegal,
    EOF,

    // Identifiers + literals
    Ident,
    Int,

    // Operators
    Assign,
    Plus,

    // Delimiters
    Comma,
    Semicolon,

    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,

    // Keywords
    Function,
    Let,
}

pub struct Token {
    pub toktype: TokenType,
    pub literal: String,
}

impl Token {
    pub fn new(toktype: TokenType, literal: String) -> Self {
        Self { toktype, literal }
    }
}

pub fn lookup_ident(ident: &String) -> TokenType {
    match ident as &str {
        "fn" => TokenType::Function,
        "let" => TokenType::Let,
        _ => TokenType::Ident,
    }
}
