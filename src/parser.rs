use crate::{ast, lexer::Lexer, token::Token};

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    cur_token: Token,
    peek_token: Token,
    errors: Vec<String>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        let mut parser = Self {
            lexer,
            cur_token: Token::Eof,
            peek_token: Token::Eof,
            errors: Vec::new(),
        };

        parser.next_token();
        parser.next_token();
        parser
    }

    pub fn parse_program(&mut self) -> ast::Program {
        let mut statements = Vec::new();

        while self.cur_token != Token::Eof {
            if let Some(statement) = self.parse_statement() {
                statements.push(statement);
            }
            self.next_token();
        }

        ast::Program { statements }
    }

    pub fn errors(&self) -> &Vec<String> {
        &self.errors
    }

    fn peek_error(&mut self, token_type: &str) {
        let msg = format!(
            "Expected next token to be {token_type}, got {:?} instead",
            self.peek_token
        );
        self.errors.push(msg);
    }

    fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    fn parse_statement(&mut self) -> Option<ast::Statement> {
        let statement = match self.cur_token {
            Token::Let => ast::Statement::Let(self.parse_let_statement()?),
            _ => return None,
        };
        Some(statement)
    }

    fn parse_let_statement(&mut self) -> Option<ast::LetStatement> {
        let token = self.cur_token.clone();

        let Token::Ident(ref ident_name) = self.peek_token else {
            self.peek_error("Ident");
            return None;
        };
        let ident_value = ident_name.clone();
        self.next_token();

        let name = ast::Identifier {
            token: self.cur_token.clone(),
            value: ident_value,
        };

        let Token::Assign = self.peek_token else {
            self.peek_error("Assign");
            return None;
        };
        self.next_token();

        while self.cur_token != Token::Semicolon {
            self.next_token();
        }

        Some(ast::LetStatement {
            token,
            name,
            value: ast::Expression::Identifier(ast::Identifier {
                token: self.cur_token.clone(),
                value: String::new(),
            }),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_let_statements() {
        let input = "
            let x = 5;
            let y = 10;
            let foobar = 838383;
        ";

        let lexer = Lexer::new(&input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        check_parser_errors(&parser);

        assert_eq!(3, program.statements.len(), "Should contain 3 values");

        let tests = [("x"), ("y"), ("foobar")];

        for (identifier, statement) in tests.iter().zip(program.statements) {
            test_let_statement(statement, identifier);
        }
    }

    fn check_parser_errors(parser: &Parser<'_>) {
        let errors = parser.errors();

        if errors.len() == 0 {
            return;
        }

        let mut msg = format!(
            "Parser has {} {}:",
            errors.len(),
            if errors.len() == 1 { "error" } else { "errors" }
        );
        for error in errors {
            msg.push_str("\n - ");
            msg.push_str(error);
        }
        panic!("{msg}");
    }

    fn test_let_statement(statement: ast::Statement, name: &str) {
        let ast::Statement::Let(statement) = statement else {
            panic!("Expected LetStatement, got {statement:?}");
        };

        assert_eq!(name, statement.name.value, "Expected {name}");
        assert_eq!(
            Token::Ident(name.to_string()),
            statement.name.token,
            "Expected identifier with name '{name}'"
        );
    }
}
