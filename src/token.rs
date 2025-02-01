#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Illegal,
    Eof,
    Ident(String),
    Int(i64),
    Assign,
    Plus,
    Comma,
    Semicolon,
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Function,
    Let,
}

impl Token {
    pub fn from_ident(ident: String) -> Self {
        match ident.as_str() {
            "fn" => Self::Function,
            "let" => Self::Let,
            _ => Self::Ident(ident),
        }
    }
}
