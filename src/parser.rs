use crate::{lexer::Lexer, token::{Token, TokenType}, ast};
// use crate::{ast::{Program, Statement}, lexer::Lexer, token::{Token, TokenType}};

pub struct Parser {
    lexer: Lexer,
    curr_token: Token,
    peek_token: Token,
    errors: Vec<String>,
}

impl Parser {
    pub fn new(mut lexer: Lexer) -> Self {
        let curr_token = lexer.next_token();
        let peek_token = lexer.next_token();
        Self {
            lexer,
            curr_token,
            peek_token,
            errors: Vec::new(),
        }
    }

    pub fn next_token(&mut self) {
        self.curr_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    fn parse_statement(&mut self) -> Option<ast::Statement> {
        match self.curr_token.toktype {
            TokenType::Let => self.parse_let_statement(),
            _ => None,
        }
    }

    fn curr_token_is(&self, tok: &TokenType) -> bool {
        &self.curr_token.toktype == tok
    }

    fn peek_token_is(&self, tok: &TokenType) -> bool {
        &self.peek_token.toktype == tok
    }

    fn expect_peek(&mut self, tok: TokenType) -> bool {
        if self.peek_token_is(&tok) {
            self.next_token();
            true
        } else {
            self.peek_error(tok);
            false
        }
    }

    fn parse_let_statement(&mut self) -> Option<ast::Statement> {
        let token = self.curr_token.clone();

        if !self.expect_peek(TokenType::Ident) {
            return None;
        }

        let name = ast::Identifier {
            token: self.curr_token.clone(),
            value: self.curr_token.literal.clone(),
        };

        if !self.expect_peek(TokenType::Assign) {
            return None;
        }

        while !self.curr_token_is(&TokenType::Semicolon) {
            self.next_token();
        }

        Some(ast::Statement::LetStatement(ast::LetStatement {
            token,
            name,
            value: ast::Expression::None
        }))
    }

    pub fn parse_program(&mut self) -> ast::Program {
        let mut program = ast::Program::new();

        while !self.curr_token_is(&TokenType::EOF) {
            if let Some(stmt) = self.parse_statement() {
                program.statements.push(stmt);
            }
            self.next_token();
        }
        program
    }

    pub fn errors(&self) -> &Vec<String> {
        &self.errors
    }

    pub fn peek_error(&mut self, tok: TokenType) {
        let msg = format!("Expected next token to be {tok:?}, got {:?}", self.peek_token.toktype);
        self.errors.push(msg);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Node;
    use crate::ast::Statement;

    #[test]
    fn let_statements() {
        let input = String::from(
            "
let x = 5;
let y = 10;
let foobar = 838383;
",
        );
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        check_parser_errors(&parser);

        let length = program.statements.len();
        assert_eq!(length, 3, "Expected 3 statements, got {}", length);

        let tests = ["x".to_string(), "y".to_string(), "foobar".to_string()];

        for (test, stmt) in tests.iter().zip(program.statements) {
            assert_eq!(
                stmt.token_literal(),
                "let".to_string(),
                "Expected 'let', got '{}'",
                stmt.token_literal()
            );
            if let Statement::LetStatement(let_stmt) = stmt { 
                assert_eq!(&let_stmt.name.value, test);
                assert_eq!(&let_stmt.name.token_literal(), test);
            } else {
                panic!("stmt not LetStatement, got {:?}", stmt);
            }
        }
    }

    fn check_parser_errors(parser: &Parser) {
        let errors = parser.errors();

        if errors.len() == 0 {
            return
        }

        println!("Parser has {} errors", errors.len());
        for error in errors {
            println!("Parser error: {error}");
        }
        panic!();
    }

    #[test]
    #[should_panic]
    fn errors() {
        let input = String::from(
            "
let = 5;
let y = 10;
let 838383;
",
        );
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        parser.parse_program();
        check_parser_errors(&parser);
    }
}
