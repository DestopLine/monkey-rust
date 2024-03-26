use crate::{ast, lexer::Lexer, token::{Token, TokenType}};
// use crate::{ast::{Program, Statement}, lexer::Lexer, token::{Token, TokenType}};

#[derive(Debug, PartialEq, PartialOrd)]
enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
}

fn get_precedence(toktype: &TokenType) -> Precedence {
    match toktype {
        TokenType::Eq | TokenType::NotEq => Precedence::Equals,
        TokenType::Lt | TokenType::Gt => Precedence::LessGreater,
        TokenType::Plus | TokenType::Minus => Precedence::Sum,
        TokenType::Slash | TokenType::Asterisk => Precedence::Product,
        _ => Precedence::Lowest,
    }
}

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
            TokenType::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
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

        Some(ast::Statement::Let(ast::LetStatement {
            token,
            name,
            value: ast::Expression::None
        }))
    }

    fn parse_return_statement(&mut self) -> Option<ast::Statement> {
        let token = self.curr_token.clone();

        self.next_token();

        while !self.curr_token_is(&TokenType::Semicolon) {
            self.next_token();
        }

        Some(ast::Statement::Return(ast::ReturnStatement {
            token,
            return_value: ast::Expression::None,
        }))
    }

    fn parse_expression_statement(&mut self) -> Option<ast::Statement> {
        let stmt = ast::ExpressionStatement {
            token: self.curr_token.clone(),
            expression: self.parse_expression(Precedence::Lowest),
        };

        if self.peek_token_is(&TokenType::Semicolon) {
            self.next_token();
        }

        Some(ast::Statement::ExpressionStatement(stmt))
    }

    fn parse_prefix_expression(&mut self, toktype: &TokenType) -> ast::Expression {
        match toktype {
            TokenType::Ident => {
                ast::Expression::Identifier(ast::Identifier{
                    token: self.curr_token.clone(),
                    value: self.curr_token.literal.clone(),
                })
            }
            TokenType::Int => {
                let token = self.curr_token.clone();
                let value: i64 = match self.curr_token.literal.parse() {
                    Ok(v) => v,
                    Err(_) => {
                        let msg = format!("Could not parse {} as integer", self.curr_token.literal);
                        self.errors.push(String::from(msg));
                        return ast::Expression::None;
                    }
                };
                ast::Expression::IntegerLiteral(ast::IntegerLiteral {
                    token,
                    value,
                })
            }
            TokenType::Bang | TokenType::Minus => {
                let token = self.curr_token.clone();
                let operator = self.curr_token.literal.clone();

                self.next_token();

                ast::Expression::PrefixExpression(ast::PrefixExpression {
                    token,
                    operator,
                    right: Box::new(self.parse_expression(Precedence::Prefix)),
                })
            }
            _ => ast::Expression::None,
        }
    }

    fn parse_infix_expression(&mut self, toktype: &TokenType, left: ast::Expression) -> Result<ast::Expression, ast::Expression> {
        match toktype {
            TokenType::Plus 
            | TokenType::Minus
            | TokenType::Asterisk
            | TokenType::Slash
            | TokenType::Lt
            | TokenType::Gt
            | TokenType::Eq
            | TokenType::NotEq => {
                self.next_token();
                let token = self.curr_token.clone();
                let operator = self.curr_token.literal.clone();
                let left = Box::new(left);

                let precedence = self.curr_precedence();
                self.next_token();
                let right = Box::new(self.parse_expression(precedence));

                Ok(ast::Expression::InfixExpression(ast::InfixExpression {
                    token,
                    operator,
                    left,
                    right,
                }))
            }
            _ => Err(left),
        }
    }

    fn no_prefix_parse_error(&mut self, tok: TokenType) {
        self.errors.push(format!("No prefix parse function for {tok:?}"));
    }

    fn parse_expression(&mut self, precedence: Precedence) -> ast::Expression {
        let mut left = self.parse_prefix_expression(&self.curr_token.toktype.clone());
        if let ast::Expression::None = left {
            self.no_prefix_parse_error(self.curr_token.toktype.clone());
        }

        while !self.peek_token_is(&TokenType::Semicolon) && precedence < self.peek_precedence() {
            let infix = self.parse_infix_expression(&self.peek_token.toktype.clone(), left);

            if let Err(left) = infix {
                return left
            }

            left = infix.unwrap();
        }

        left
    }

    fn peek_precedence(&self) -> Precedence {
        get_precedence(&self.peek_token.toktype)
    }

    fn curr_precedence(&self) -> Precedence {
        get_precedence(&self.curr_token.toktype)
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
            if let Statement::Let(let_stmt) = stmt { 
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

    #[test]
    fn return_statements() {
        let input = String::from("
return 5;
return 10;
return 993322;
");
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        check_parser_errors(&parser);

        let length = program.statements.len();
        assert_eq!(length, 3, "Expected 3 statements, got {}", length);

        for stmt in program.statements {
            if let Statement::Return(return_stmt) = stmt { 
                assert_eq!(&return_stmt.token_literal(), "return");
            } else {
                panic!("Expected ReturnStatement, got {:?}", stmt);
            }
        }
    }

    #[test]
    fn identifier_expression() {
        let input = String::from("foobar;");

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(&parser);

        assert_eq!(program.statements.len(), 1);
        
        if let ast::Statement::ExpressionStatement(stmt) = &program.statements[0] {
            if let ast::Expression::Identifier(ident) = &stmt.expression {
                assert_eq!(&ident.value, "foobar");
                assert_eq!(&ident.token_literal(), "foobar");
            } else {
                panic!("Expected Identifier, got {:?} instead", stmt.expression);
            }
        } else {
            panic!("Expected ExpressionStatement, got {:?} instead", program.statements[0]);
        }
    }

    #[test]
    fn integer_literal_expression() {
        let input = String::from("5;");

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(&parser);

        assert_eq!(program.statements.len(), 1);
        
        if let ast::Statement::ExpressionStatement(stmt) = &program.statements[0] {
            if let ast::Expression::IntegerLiteral(literal) = &stmt.expression {
                assert_eq!(literal.value, 5);
                assert_eq!(&literal.token_literal(), "5");
            } else {
                panic!("Expected Identifier, got {:?} instead", stmt.expression);
            }
        } else {
            panic!("Expected ExpressionStatement, got {:?} instead", program.statements[0]);
        }
    }

    #[test]
    fn parsing_prefix_expressions() {
        let tests = [
            ("!5", "!", 5),
            ("-15", "-", 15),
        ];

        for (input, operator, value) in tests {
            let lexer = Lexer::new(input.to_string());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            check_parser_errors(&parser);

            assert_eq!(program.statements.len(), 1);

            if let ast::Statement::ExpressionStatement(stmt) = &program.statements[0] {
                if let ast::Expression::PrefixExpression(expr) = &stmt.expression {
                    assert_eq!(expr.operator, operator);
                    check_integer_literal(*expr.right.clone(), value);
                } else {
                    panic!("Expected PrefixExpression, got {:?} instead", stmt.expression);
                }
            } else {
                panic!("Expected ExpressionStatement, got {:?} instead", program.statements[0]);
            }
        }
    }

    fn check_integer_literal(expr: ast::Expression, value: i64) {
        if let ast::Expression::IntegerLiteral(int) = expr {
            assert_eq!(int.value, value);
            assert_eq!(int.token_literal(), value.to_string());
        } else {
            panic!("Expected IntegerLiteral, got {expr:?} instead");
        }
    }

    #[test]
    fn parsing_infix_expressions() {
        let tests = [
            ("5 + 5", 5, "+", 5),
            ("5 - 5", 5, "-", 5),
            ("5 * 5", 5, "*", 5),
            ("5 / 5", 5, "/", 5),
            ("5 > 5", 5, ">", 5),
            ("5 < 5", 5, "<", 5),
            ("5 == 5", 5, "==", 5),
            ("5 != 5", 5, "!=", 5),
        ];

        for (input, left, operator, right) in tests {
            let lexer = Lexer::new(input.to_string());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            check_parser_errors(&parser);

            assert_eq!(program.statements.len(), 1);

            if let ast::Statement::ExpressionStatement(stmt) = &program.statements[0] {
                if let ast::Expression::InfixExpression(expr) = &stmt.expression {
                    check_integer_literal(*expr.left.clone(), left);
                    assert_eq!(expr.operator, operator);
                    check_integer_literal(*expr.right.clone(), right);
                } else {
                    panic!("Expected InfixExpression, got {:?} instead", stmt.expression);
                }
            } else {
                panic!("Expected ExpressionStatement, got {:?} instead", program.statements[0]);
            }
        }
    }

    #[test]
    fn operator_precedence_parsing() {
        let tests = [
            (
                "-a * b",
                "((-a) * b)",
            ),
            (
                "!-a",
                "(!(-a))",
            ),
            (
                "a + b + c",
                "((a + b) + c)",
            ),
            (
                "a + b - c",
                "((a + b) - c)",
            ),
            (
                "a * b * c",
                "((a * b) * c)",
            ),
            (
                "a * b / c",
                "((a * b) / c)",
            ),
            (
                "a + b / c",
                "(a + (b / c))",
            ),
            (
                "a + b * c + d / e - f",
                "(((a + (b * c)) + (d / e)) - f)",
            ),
            (
                "3 + 4; -5 * 5",
                "(3 + 4)((-5) * 5)",
            ),
            (
                "5 > 4 == 3 < 4",
                "((5 > 4) == (3 < 4))",
            ),
            (
                "5 < 4 != 3 > 4",
                "((5 < 4) != (3 > 4))",
            ),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
        ];

        for (input, expected) in tests {
            let lexer = Lexer::new(input.to_string());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            check_parser_errors(&parser);

            dbg!(&program.statements);

            let actual = program.string();

            assert_eq!(actual, expected);
        }
    }
}
