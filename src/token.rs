use std::fmt::Display;

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum TokenKind {
    Illegal,
    EOF,

    // Identifiers + literals
    Ident(String),
    Int(i64),
    String(String),

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

impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Self::Illegal => "",
            Self::EOF => "",
            Self::Ident(s) => return write!(f, "{s}"),
            Self::Int(n) => return write!(f, "{n}"),
            Self::String(s) => return write!(f, "{s}"),
            Self::Assign => "=",
            Self::Plus => "+",
            Self::Minus => "-",
            Self::Asterisk => "*",
            Self::Slash => "/",
            Self::Lt => "<",
            Self::Gt => ">",
            Self::Eq => "==",
            Self::NotEq => "!=",
            Self::Bang => "!",
            Self::Comma => ",",
            Self::Semicolon => ";",
            Self::Colon => ":",
            Self::OpenParen => "(",
            Self::CloseParen => ")",
            Self::OpenBrace => "{",
            Self::CloseBrace => "}",
            Self::OpenBracket => "[",
            Self::CloseBracket => "]",
            Self::Function => "fn",
            Self::Let => "let",
            Self::True => "true",
            Self::False => "false",
            Self::If => "if",
            Self::Else => "else",
            Self::Return => "return",
        };
        write!(f, "{s}")
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Token {
    pub kind: TokenKind,
    pub range: Range,
}

impl Token {
    pub fn new(kind: TokenKind, start: usize, end: usize) -> Self {
        let range = Range { start, end };
        Self {
            kind,
            range,
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Range {
    pub start: usize,
    pub end: usize,
}

pub fn lookup_ident(ident: &String) -> TokenKind {
    match ident as &str {
        "fn" => TokenKind::Function,
        "let" => TokenKind::Let,
        "true" => TokenKind::True,
        "false" => TokenKind::False,
        "if" => TokenKind::If,
        "else" => TokenKind::Else,
        "return" => TokenKind::Return,
        _ => TokenKind::Ident(ident.to_string()),
    }
}
