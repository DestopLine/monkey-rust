#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum TokenType {
    Illegal,
    EOF,

    // Identifiers + literals
    Ident,
    Int,
    String,

    // Operators
    Assign,
    Plus,
    Minus,
    Asterisk,
    Slash,
    Lt,
    Gt,
    Eq,
    NotEq,
    Bang,

    // Delimiters
    Comma,
    Semicolon,
    Colon,

    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    OpenBracket,
    CloseBracket,

    // Keywords
    Function,
    Let,
    True,
    False,
    If,
    Else,
    Return,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
        "true" => TokenType::True,
        "false" => TokenType::False,
        "if" => TokenType::If,
        "else" => TokenType::Else,
        "return" => TokenType::Return,
        _ => TokenType::Ident,
    }
}
