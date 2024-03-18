enum TokenType {
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

struct Token {
    typeof: TokenType,
    literal: string,
}
