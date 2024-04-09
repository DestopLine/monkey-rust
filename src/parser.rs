use crate::{
    ast,
    lexer::Lexer,
    token::{Token, TokenKind},
};
// use crate::{ast::{Program, Statement}, lexer::Lexer, token::{Token, TokenKind}};

#[derive(Debug, PartialEq, PartialOrd)]
enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
    Index,
}

fn get_precedence(toktype: &TokenKind) -> Precedence {
    match toktype {
        TokenKind::Eq | TokenKind::NotEq => Precedence::Equals,
        TokenKind::Lt | TokenKind::Gt => Precedence::LessGreater,
        TokenKind::Plus | TokenKind::Minus => Precedence::Sum,
        TokenKind::Slash | TokenKind::Asterisk => Precedence::Product,
        TokenKind::OpenParen => Precedence::Call,
        TokenKind::OpenBracket => Precedence::Index,
        _ => Precedence::Lowest,
    }
}

fn op_token_to_string(kind: &TokenKind) -> String {
    let op = match kind {
        TokenKind::Plus => "+",
        TokenKind::Minus => "-",
        TokenKind::Asterisk => "*",
        TokenKind::Slash => "/",
        TokenKind::Bang => "!",
        TokenKind::Eq => "==",
        TokenKind::NotEq => "!=",
        TokenKind::Lt => "<",
        TokenKind::Gt => ">",
        _ => panic!("TokenKind is not an operator"),
    };
    op.to_string()
}

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    curr_token: Token,
    peek_token: Token,
    errors: Vec<String>,
}

impl<'a> Parser<'a> {
    pub fn new(mut lexer: Lexer<'a>) -> Self {
        let curr_token = lexer.next().unwrap(); //  TODO: Handle Option
        let peek_token = lexer.next().unwrap_or(Token::new(TokenKind::EOF, 1, 2));
        Self {
            lexer,
            curr_token,
            peek_token,
            errors: Vec::new(),
        }
    }

    pub fn next_token(&mut self) {
        self.curr_token = self.peek_token.clone();
        self.peek_token = self.lexer.next().unwrap_or(Token::new(TokenKind::EOF, 0, 0));
    }

    fn parse_statement(&mut self) -> Option<ast::Statement> {
        match self.curr_token.kind {
            TokenKind::Let => self.parse_let_statement(),
            TokenKind::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn curr_token_is(&self, tok: &TokenKind) -> bool {
        std::mem::discriminant(&self.curr_token.kind) == std::mem::discriminant(tok)
    }

    fn peek_token_is(&self, tok: &TokenKind) -> bool {
        std::mem::discriminant(&self.peek_token.kind) == std::mem::discriminant(tok)
    }

    fn expect_peek(&mut self, tok: TokenKind) -> bool {
        if self.peek_token_is(&tok) {
            self.next_token();
            true
        } else {
            self.peek_error(&tok);
            false
        }
    }

    fn parse_let_statement(&mut self) -> Option<ast::Statement> {
        let token = self.curr_token.clone();

        let TokenKind::Ident(ident) = &self.peek_token.kind else {
            self.peek_error(&self.peek_token.kind.clone());
            return None;
        };

        let name = ast::Identifier {
            token: self.peek_token.clone(),
            value: ident.clone(),
        };

        self.next_token();

        if !self.expect_peek(TokenKind::Assign) {
            return None;
        }

        self.next_token();

        let value = self.parse_expression(Precedence::Lowest);

        if self.peek_token_is(&TokenKind::Semicolon) {
            self.next_token();
        }

        Some(ast::Statement::Let(ast::LetStatement {
            token,
            name,
            value,
        }))
    }

    fn parse_return_statement(&mut self) -> Option<ast::Statement> {
        let token = self.curr_token.clone();

        self.next_token();

        let return_value = self.parse_expression(Precedence::Lowest);

        if self.peek_token_is(&TokenKind::Semicolon) {
            self.next_token();
        }

        Some(ast::Statement::Return(ast::ReturnStatement {
            token,
            return_value,
        }))
    }

    fn parse_expression_statement(&mut self) -> Option<ast::Statement> {
        let stmt = ast::ExpressionStatement {
            token: self.curr_token.clone(),
            expression: self.parse_expression(Precedence::Lowest),
        };

        if self.peek_token_is(&TokenKind::Semicolon) {
            self.next_token();
        }

        Some(ast::Statement::ExpressionStatement(stmt))
    }

    fn parse_block_statement(&mut self) -> ast::BlockStatement {
        let token = self.curr_token.clone();
        let mut statements = Vec::new();

        self.next_token();

        while !self.curr_token_is(&TokenKind::CloseBrace) && !self.curr_token_is(&TokenKind::EOF) {
            if let Some(stmt) = self.parse_statement() {
                statements.push(stmt);
            }
            self.next_token();
        }

        ast::BlockStatement { token, statements }
    }

    fn parse_function_parameters(&mut self) -> Option<Vec<ast::Identifier>> {
        let mut identifiers = Vec::new();

        if self.peek_token_is(&TokenKind::CloseParen) {
            self.next_token();
            return Some(identifiers);
        }

        self.next_token();

        let TokenKind::Ident(value) = &self.curr_token.kind else {
            self.peek_error(&self.curr_token.kind.clone());
            return None;
        };

        let ident = ast::Identifier {
            token: self.curr_token.clone(),
            value: value.clone(),
        };
        identifiers.push(ident);

        while self.peek_token_is(&TokenKind::Comma) {
            self.next_token();
            self.next_token();

            let TokenKind::Ident(value) = &self.curr_token.kind else {
                self.peek_error(&self.curr_token.kind.clone());
                return None;
            };
            let ident = ast::Identifier {
                token: self.curr_token.clone(),
                value: value.clone(),
            };
            identifiers.push(ident);
        }

        if !self.expect_peek(TokenKind::CloseParen) {
            return None;
        }

        Some(identifiers)
    }

    fn parse_expression_list(&mut self, end: TokenKind) -> Option<Vec<ast::Expression>> {
        let mut args = Vec::new();

        if self.peek_token_is(&end) {
            self.next_token();
            return Some(args);
        }

        self.next_token();
        args.push(self.parse_expression(Precedence::Lowest));

        while self.peek_token_is(&TokenKind::Comma) {
            self.next_token();
            self.next_token();
            args.push(self.parse_expression(Precedence::Lowest));
        }

        if !self.expect_peek(end) {
            None
        } else {
            Some(args)
        }
    }

    fn parse_prefix_expression(&mut self, toktype: &TokenKind) -> ast::Expression {
        match toktype {
            TokenKind::Ident(value) => ast::Expression::Identifier(ast::Identifier {
                token: self.curr_token.clone(),
                value: value.clone(),
            }),
            TokenKind::Int(value) => ast::Expression::IntegerLiteral(ast::IntegerLiteral {
                token: self.curr_token.clone(),
                value: *value,
            }),
            op @ (TokenKind::Bang | TokenKind::Minus) => {
                let token = self.curr_token.clone();
                let operator = op_token_to_string(op);

                self.next_token();

                ast::Expression::PrefixExpression(ast::PrefixExpression {
                    token,
                    operator: operator.to_string(),
                    right: Box::new(self.parse_expression(Precedence::Prefix)),
                })
            }
            TokenKind::True | TokenKind::False => ast::Expression::Boolean(ast::Boolean {
                token: self.curr_token.clone(),
                value: self.curr_token_is(&TokenKind::True),
            }),
            TokenKind::OpenParen => {
                self.next_token();
                let expr = self.parse_expression(Precedence::Lowest);

                if self.expect_peek(TokenKind::CloseParen) {
                    expr
                } else {
                    ast::Expression::None
                }
            }
            TokenKind::If => {
                let token = self.curr_token.clone();

                if !self.expect_peek(TokenKind::OpenParen) {
                    return ast::Expression::None;
                }

                self.next_token();
                let condition = Box::new(self.parse_expression(Precedence::Lowest));

                if !self.expect_peek(TokenKind::CloseParen) {
                    return ast::Expression::None;
                }

                if !self.expect_peek(TokenKind::OpenBrace) {
                    return ast::Expression::None;
                }

                let consequence = self.parse_block_statement();

                let alternative = if self.peek_token_is(&TokenKind::Else) {
                    self.next_token();

                    if !self.expect_peek(TokenKind::OpenBrace) {
                        return ast::Expression::None;
                    }

                    Some(self.parse_block_statement())
                } else {
                    None
                };

                ast::Expression::IfExpression(ast::IfExpression {
                    token,
                    condition,
                    consequence,
                    alternative,
                })
            }
            TokenKind::Function => {
                let token = self.curr_token.clone();

                if !self.expect_peek(TokenKind::OpenParen) {
                    return ast::Expression::None;
                }

                let parameters = match self.parse_function_parameters() {
                    Some(v) => v,
                    None => return ast::Expression::None,
                };

                if !self.expect_peek(TokenKind::OpenBrace) {
                    return ast::Expression::None;
                }

                let body = self.parse_block_statement();

                ast::Expression::FunctionLiteral(ast::FunctionLiteral {
                    token,
                    parameters,
                    body,
                })
            }
            TokenKind::String(value) => ast::Expression::StringLiteral(ast::StringLiteral {
                token: self.curr_token.clone(),
                value: value.clone(),
            }),
            TokenKind::OpenBracket => ast::Expression::ArrayLiteral(ast::ArrayLiteral {
                token: self.curr_token.clone(),
                elements: match self.parse_expression_list(TokenKind::CloseBracket) {
                    Some(v) => v,
                    None => return ast::Expression::None,
                },
            }),
            TokenKind::OpenBrace => {
                let token = self.curr_token.clone();
                let mut pairs = Vec::new();

                while !self.peek_token_is(&TokenKind::CloseBrace) {
                    self.next_token();
                    let key = self.parse_expression(Precedence::Lowest);

                    if !self.expect_peek(TokenKind::Colon) {
                        return ast::Expression::None;
                    }

                    self.next_token();
                    let value = self.parse_expression(Precedence::Lowest);

                    pairs.push((key, value));
                    
                    if !self.peek_token_is(&TokenKind::CloseBrace) && !self.expect_peek(TokenKind::Comma) {
                        return ast::Expression::None;
                    }
                }

                if !self.expect_peek(TokenKind::CloseBrace) {
                    return ast::Expression::None;
                }

                ast::Expression::HashLiteral(ast::HashLiteral {
                    token,
                    pairs,
                })
            },
            _ => ast::Expression::None,
        }
    }

    fn parse_infix_expression(
        &mut self,
        toktype: &TokenKind,
        left: ast::Expression,
    ) -> Result<ast::Expression, ast::Expression> {
        self.next_token();
        match toktype {
            TokenKind::Plus
            | TokenKind::Minus
            | TokenKind::Asterisk
            | TokenKind::Slash
            | TokenKind::Lt
            | TokenKind::Gt
            | TokenKind::Eq
            | TokenKind::NotEq => {
                let token = self.curr_token.clone();
                let operator = op_token_to_string(&self.curr_token.kind);
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
            TokenKind::OpenParen => {
                let arguments = match self.parse_expression_list(TokenKind::CloseParen) {
                    Some(v) => v,
                    None => return Err(left),
                };
                Ok(ast::Expression::CallExpression(ast::CallExpression {
                    token: self.curr_token.clone(),
                    function: Box::new(left),
                    arguments,
                }))
            }
            TokenKind::OpenBracket => {
                let token = self.curr_token.clone();
                let left = Box::new(left);
                self.next_token();
                let index = Box::new(self.parse_expression(Precedence::Lowest));

                if self.expect_peek(TokenKind::CloseBracket) {
                    Ok(ast::Expression::IndexExpression(ast::IndexExpression {
                        token,
                        left,
                        index,
                    }))
                } else {
                    Err(*left)
                }
            }
            _ => Err(left),
        }
    }

    fn no_prefix_parse_error(&mut self, tok: TokenKind) {
        self.errors
            .push(format!("No prefix parse function for {tok:?}"));
    }

    fn parse_expression(&mut self, precedence: Precedence) -> ast::Expression {
        let mut left = self.parse_prefix_expression(&self.curr_token.kind.clone());
        if let ast::Expression::None = left {
            self.no_prefix_parse_error(self.curr_token.kind.clone());
        }

        while !self.peek_token_is(&TokenKind::Semicolon) && precedence < self.peek_precedence() {
            let infix = self.parse_infix_expression(&self.peek_token.kind.clone(), left);

            if let Err(left) = infix {
                return left;
            }

            left = infix.unwrap();
        }

        left
    }

    fn peek_precedence(&self) -> Precedence {
        get_precedence(&self.peek_token.kind)
    }

    fn curr_precedence(&self) -> Precedence {
        get_precedence(&self.curr_token.kind)
    }

    pub fn parse_program(&mut self) -> ast::Program {
        let mut program = ast::Program::new();

        while !self.curr_token_is(&TokenKind::EOF) {
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

    pub fn peek_error(&mut self, tok: &TokenKind) {
        let msg = format!(
            "Expected next token to be {tok:?}, got {:?} instead",
            self.peek_token.kind
        );
        self.errors.push(msg);
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use super::*;
    use crate::ast::MonkeyNode;
    use crate::ast::Statement;

    #[test]
    fn let_statements() {
        let tests = [
            ("let x = 5;", "x", Literal::Int(5)),
            ("let y = true;", "y", Literal::Bool(true)),
            ("let foobar = y;", "foobar", Literal::Str("y")),
        ];

        for (input, expected_identifier, expected_value) in tests {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            check_parser_errors(&parser);

            assert_eq!(program.statements.len(), 1);

            if let Statement::Let(stmt) = &program.statements[0] {
                assert_eq!(&stmt.token.to_string(), "let");
                assert_eq!(&stmt.name.token.to_string(), expected_identifier);
                assert_eq!(&stmt.name.value, expected_identifier);
                test_literal_expression(&stmt.value, expected_value)
            } else {
                panic!(
                    "Expected LetStatement, got {:?} instead",
                    program.statements[0]
                );
            }
        }
    }

    fn check_parser_errors(parser: &Parser) {
        let errors = parser.errors();

        if errors.len() == 0 {
            return;
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
        let input =
"
let = 5;
let y = 10;
let 838383;
";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        parser.parse_program();
        check_parser_errors(&parser);
    }

    #[test]
    fn return_statements() {
        let tests = [
            ("return 5;", Literal::Int(5)),
            ("return false;", Literal::Bool(false)),
            ("return foo;", Literal::Str("foo")),
        ];

        for (input, expected) in tests {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            check_parser_errors(&parser);

            assert_eq!(program.statements.len(), 1);

            if let ast::Statement::Return(stmt) = &program.statements[0] {
                assert_eq!(stmt.token.to_string(), "return");
                test_literal_expression(&stmt.return_value, expected);
            } else {
                panic!(
                    "Expected ExpressionStatement, got {:?} instead",
                    program.statements[9]
                )
            }
        }
    }

    #[test]
    fn identifier_expression() {
        let input = "foobar;";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(&parser);

        assert_eq!(program.statements.len(), 1);

        if let ast::Statement::ExpressionStatement(stmt) = &program.statements[0] {
            test_identifier(&stmt.expression, "foobar");
        } else {
            panic!(
                "Expected ExpressionStatement, got {:?} instead",
                program.statements[0]
            );
        }
    }

    #[test]
    fn integer_literal_expression() {
        let input = "5;";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(&parser);

        assert_eq!(program.statements.len(), 1);

        if let ast::Statement::ExpressionStatement(stmt) = &program.statements[0] {
            test_integer_literal(&stmt.expression, 5)
        } else {
            panic!(
                "Expected ExpressionStatement, got {:?} instead",
                program.statements[0]
            );
        }
    }

    #[test]
    fn boolean_expression() {
        let input = "true;";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(&parser);

        assert_eq!(program.statements.len(), 1);

        if let ast::Statement::ExpressionStatement(stmt) = &program.statements[0] {
            if let ast::Expression::Boolean(boolean) = &stmt.expression {
                assert_eq!(boolean.value, true);
                assert_eq!(&boolean.token.to_string(), "true");
            } else {
                panic!("Expected Boolean, got {:?} instead", stmt.expression);
            }
        } else {
            panic!(
                "Expected ExpressionStatement, got {:?} instead",
                program.statements[0]
            );
        }
    }

    #[test]
    fn parsing_prefix_expressions() {
        let tests = [
            ("!5", "!", Literal::Int(5)),
            ("-15", "-", Literal::Int(15)),
            ("!true", "!", Literal::Bool(true)),
            ("!false", "!", Literal::Bool(false)),
        ];

        for (input, operator, value) in tests {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            check_parser_errors(&parser);

            assert_eq!(program.statements.len(), 1);

            if let ast::Statement::ExpressionStatement(stmt) = &program.statements[0] {
                if let ast::Expression::PrefixExpression(expr) = &stmt.expression {
                    assert_eq!(expr.operator, operator);
                    test_literal_expression(&expr.right, value);
                } else {
                    panic!(
                        "Expected PrefixExpression, got {:?} instead",
                        stmt.expression
                    );
                }
            } else {
                panic!(
                    "Expected ExpressionStatement, got {:?} instead",
                    program.statements[0]
                );
            }
        }
    }

    #[derive(Debug)]
    enum Literal {
        Int(i64),
        Str(&'static str),
        Bool(bool),
    }

    fn test_integer_literal(expr: &ast::Expression, value: i64) {
        if let ast::Expression::IntegerLiteral(int) = expr {
            assert_eq!(int.value, value);
            assert_eq!(int.token.to_string(), value.to_string());
        } else {
            panic!("Expected IntegerLiteral, got {expr:?} instead");
        }
    }

    fn test_identifier(expr: &ast::Expression, value: &str) {
        if let ast::Expression::Identifier(ident) = expr {
            assert_eq!(ident.value, value.to_string());
            assert_eq!(ident.token.to_string(), value.to_string());
        } else {
            panic!("Expected Identifier, got {expr:?} instead");
        }
    }

    fn test_boolean_literal(expr: &ast::Expression, value: bool) {
        if let ast::Expression::Boolean(boolean) = expr {
            assert_eq!(boolean.value, value);
            assert_eq!(boolean.token.to_string(), value.to_string());
        } else {
            panic!("Expected Boolean, got {expr:?} instead");
        }
    }

    fn test_literal_expression(expr: &ast::Expression, value: Literal) {
        match value {
            Literal::Int(int) => test_integer_literal(&expr, int),
            Literal::Str(string) => test_identifier(&expr, string),
            Literal::Bool(boolean) => test_boolean_literal(&expr, boolean),
        }
    }

    fn test_infix_expression(
        expr: &ast::Expression,
        left: Literal,
        operator: &str,
        right: Literal,
    ) {
        if let ast::Expression::InfixExpression(infix) = expr {
            test_literal_expression(&infix.left, left);
            assert_eq!(&infix.operator, operator);
            test_literal_expression(&infix.right, right);
        } else {
            panic!("Expected InfixExpression, got {expr:?} instead");
        }
    }

    #[test]
    fn parsing_infix_expressions() {
        let tests = [
            ("5 + 5", Literal::Int(5), "+", Literal::Int(5)),
            ("5 - 5", Literal::Int(5), "-", Literal::Int(5)),
            ("5 * 5", Literal::Int(5), "*", Literal::Int(5)),
            ("5 / 5", Literal::Int(5), "/", Literal::Int(5)),
            ("5 > 5", Literal::Int(5), ">", Literal::Int(5)),
            ("5 < 5", Literal::Int(5), "<", Literal::Int(5)),
            ("5 == 5", Literal::Int(5), "==", Literal::Int(5)),
            ("5 != 5", Literal::Int(5), "!=", Literal::Int(5)),
            (
                "true == true",
                Literal::Bool(true),
                "==",
                Literal::Bool(true),
            ),
            (
                "true != false",
                Literal::Bool(true),
                "!=",
                Literal::Bool(false),
            ),
            (
                "false == false",
                Literal::Bool(false),
                "==",
                Literal::Bool(false),
            ),
        ];

        for (input, left, operator, right) in tests {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            check_parser_errors(&parser);

            assert_eq!(program.statements.len(), 1);

            if let ast::Statement::ExpressionStatement(stmt) = &program.statements[0] {
                test_infix_expression(&stmt.expression, left, operator, right)
            } else {
                panic!(
                    "Expected ExpressionStatement, got {:?} instead",
                    program.statements[0]
                );
            }
        }
    }

    #[test]
    fn operator_precedence_parsing() {
        let tests = [
            ("-a * b", "((-a) * b)"),
            ("!-a", "(!(-a))"),
            ("a + b + c", "((a + b) + c)"),
            ("a + b - c", "((a + b) - c)"),
            ("a * b * c", "((a * b) * c)"),
            ("a * b / c", "((a * b) / c)"),
            ("a + b / c", "(a + (b / c))"),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
            ("true", "true"),
            ("false", "false"),
            ("3 > 5 == false", "((3 > 5) == false)"),
            ("3 < 5 == true", "((3 < 5) == true)"),
            ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
            ("(5 + 5) * 2", "((5 + 5) * 2)"),
            ("2 / (5 + 5)", "(2 / (5 + 5))"),
            ("-(5 + 5)", "(-(5 + 5))"),
            ("!(true == true)", "(!(true == true))"),
            ("a + add(b * c) + 4", "((a + add((b * c))) + 4)"),
            (
                "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
                "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
            ),
            (
                "add(a + b + c * d / f + g)",
                "add((((a + b) + ((c * d) / f)) + g))",
            ),
            (
                "a * [1, 2, 3, 4][b * c] * d",
                "((a * ([1, 2, 3, 4][(b * c)])) * d)",
            ),
            (
                "add(a * b[2], b[1], 2 * [1, 2][1])",
                "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))",
            ),
        ];

        for (input, expected) in tests {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            check_parser_errors(&parser);

            let actual = program.string();

            assert_eq!(actual, expected);
        }
    }

    #[test]
    fn if_expression() {
        let input = "if (x < y) { x }";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(&parser);

        assert_eq!(program.statements.len(), 1);

        if let ast::Statement::ExpressionStatement(stmt) = &program.statements[0] {
            if let ast::Expression::IfExpression(expr) = &stmt.expression {
                test_infix_expression(&expr.condition, Literal::Str("x"), "<", Literal::Str("y"));
                assert_eq!(expr.consequence.statements.len(), 1);
                if let ast::Statement::ExpressionStatement(consequence) =
                    &expr.consequence.statements[0]
                {
                    test_identifier(&consequence.expression, "x");
                    assert!(matches!(expr.alternative, None));
                } else {
                    panic!(
                        "Expected ExpressionStatement, got {:?} instead",
                        expr.consequence.statements[0]
                    );
                }
            } else {
                panic!("Expected IfExpression, got {:?} instead", stmt.expression);
            }
        } else {
            panic!(
                "Expected ExpressionStatement, got {:?} instead",
                program.statements[0]
            );
        }
    }

    #[test]
    fn if_else_expression() {
        let input = "if (x < y) { x } else { y }";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(&parser);

        assert_eq!(program.statements.len(), 1);

        if let ast::Statement::ExpressionStatement(stmt) = &program.statements[0] {
            if let ast::Expression::IfExpression(expr) = &stmt.expression {
                test_infix_expression(&expr.condition, Literal::Str("x"), "<", Literal::Str("y"));
                assert_eq!(expr.consequence.statements.len(), 1);
                if let ast::Statement::ExpressionStatement(consequence) =
                    &expr.consequence.statements[0]
                {
                    test_identifier(&consequence.expression, "x");
                    if let Some(alt) = &expr.alternative {
                        assert_eq!(alt.statements.len(), 1);
                        if let ast::Statement::ExpressionStatement(alt_stmt) = &alt.statements[0] {
                            test_identifier(&alt_stmt.expression, "y");
                        }
                    } else {
                        panic!("Expected alternative");
                    }
                } else {
                    panic!(
                        "Expected ExpressionStatement, got {:?} instead",
                        expr.consequence.statements[0]
                    );
                }
            } else {
                panic!("Expected IfExpression, got {:?} instead", stmt.expression);
            }
        } else {
            panic!(
                "Expected ExpressionStatement, got {:?} instead",
                program.statements[0]
            );
        }
    }

    #[test]
    fn function_literal_parsing() {
        let input = "fn(x, y) { x + y; }";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(&parser);

        assert_eq!(program.statements.len(), 1);

        if let ast::Statement::ExpressionStatement(stmt) = &program.statements[0] {
            if let ast::Expression::FunctionLiteral(func) = &stmt.expression {
                assert_eq!(func.parameters.len(), 2);
                test_literal_expression(
                    &ast::Expression::Identifier(func.parameters[0].clone()),
                    Literal::Str("x"),
                );
                test_literal_expression(
                    &ast::Expression::Identifier(func.parameters[1].clone()),
                    Literal::Str("y"),
                );
                assert_eq!(func.body.statements.len(), 1);
                if let ast::Statement::ExpressionStatement(body_stmt) = &func.body.statements[0] {
                    test_infix_expression(
                        &body_stmt.expression,
                        Literal::Str("x"),
                        "+",
                        Literal::Str("y"),
                    );
                } else {
                    panic!(
                        "Expected ExpressionStatement, got {:?} instead",
                        func.body.statements[0]
                    );
                }
            } else {
                panic!(
                    "Expected FunctionLiteral, got {:?} instead",
                    stmt.expression
                );
            }
        } else {
            panic!(
                "Expected ExpressionStatement, got {:?} instead",
                program.statements[0]
            );
        }
    }

    #[test]
    fn function_parameter_parsing() {
        let tests = [
            ("fn() {};", vec![]),
            ("fn(x) {};", vec!["x"]),
            ("fn(x, y, z) {};", vec!["x", "y", "z"]),
        ];

        for (input, expected) in tests {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            check_parser_errors(&parser);

            if let ast::Statement::ExpressionStatement(stmt) = &program.statements[0] {
                if let ast::Expression::FunctionLiteral(func) = &stmt.expression {
                    assert_eq!(func.parameters.len(), expected.len());
                    for (func_param, expected_param) in func.parameters.iter().zip(expected) {
                        test_literal_expression(
                            &ast::Expression::Identifier(func_param.clone()),
                            Literal::Str(expected_param),
                        );
                    }
                } else {
                    panic!(
                        "Expected FunctionLiteral, got {:?} instead",
                        stmt.expression
                    )
                }
            } else {
                panic!(
                    "Expected ExpressionStatement, got {:?} instead",
                    program.statements[0]
                )
            }
        }
    }

    #[test]
    fn call_expression_parsing() {
        let input = "add(1, 2 * 3, 4 + 5);";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(&parser);

        assert_eq!(program.statements.len(), 1);

        if let ast::Statement::ExpressionStatement(stmt) = &program.statements[0] {
            if let ast::Expression::CallExpression(expr) = &stmt.expression {
                test_literal_expression(&expr.function, Literal::Str("add"));
                assert_eq!(expr.arguments.len(), 3);
                test_literal_expression(&expr.arguments[0], Literal::Int(1));
                test_infix_expression(&expr.arguments[1], Literal::Int(2), "*", Literal::Int(3));
                test_infix_expression(&expr.arguments[2], Literal::Int(4), "+", Literal::Int(5));
            } else {
                panic!("Expected CallExpression, got {:?} instead", stmt.expression);
            }
        } else {
            panic!(
                "Expected ExpressionStatement, got {:?} instead",
                program.statements[0]
            );
        }
    }

    #[test]
    fn string_literal_expression() {
        let input = r#" "hello world" "#;

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(&parser);

        assert_eq!(program.statements.len(), 1);

        let ast::Statement::ExpressionStatement(stmt) = &program.statements[0] else {
            panic!(
                "Expected ExpressionStatement, got {:?} instead",
                program.statements[0]
            );
        };

        let ast::Expression::StringLiteral(lit) = &stmt.expression else {
            panic!("Expected StringLiteral, got {:?} instead", stmt.expression);
        };

        assert_eq!(lit.value, "hello world".to_string());
    }

    #[test]
    fn parsing_array_literals() {
        let input = "[1, 2 * 2, 3 + 3]";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(&parser);

        assert_eq!(program.statements.len(), 1);

        let ast::Statement::ExpressionStatement(stmt) = &program.statements[0] else {
            panic!(
                "Expected ExpressionStatement, got {:?} instead",
                program.statements[0]
            );
        };

        let ast::Expression::ArrayLiteral(array) = &stmt.expression else {
            panic!("Expected ArrayLiteral, got {:?} instead", stmt.expression);
        };

        assert_eq!(array.elements.len(), 3);

        test_integer_literal(&array.elements[0], 1);
        test_infix_expression(&array.elements[1], Literal::Int(2), "*", Literal::Int(2));
        test_infix_expression(&array.elements[2], Literal::Int(3), "+", Literal::Int(3));
    }

    #[test]
    fn parsing_index_expressions() {
        let input = "myArray[1 + 1]";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(&parser);

        assert_eq!(program.statements.len(), 1);

        let ast::Statement::ExpressionStatement(stmt) = &program.statements[0] else {
            panic!(
                "Expected ExpressionStatement, got {:?} instead",
                program.statements[0]
            );
        };

        let ast::Expression::IndexExpression(index_expr) = &stmt.expression else {
            panic!(
                "Expected IndexExpression, got {:?} instead",
                stmt.expression
            );
        };

        test_identifier(&index_expr.left, "myArray");
        test_infix_expression(&index_expr.index, Literal::Int(1), "+", Literal::Int(1));
    }

    #[test]
    fn parsing_hash_literals_string_keys() {
        let input = r#" {"one": 1, "two": 2, "three": 3} "#;

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(&parser);

        assert_eq!(program.statements.len(), 1);

        let ast::Statement::ExpressionStatement(stmt) = &program.statements[0] else {
            panic!(
                "Expected ExpressionStatement, got {:?} instead",
                program.statements[0]
            );
        };

        let ast::Expression::HashLiteral(hash) = &stmt.expression else {
            panic!("Expected HashLiteral, got {stmt:?} instead")
        };

        assert_eq!(hash.pairs.len(), 3);

        let expected = HashMap::from([
            ("one", 1),
            ("two", 2),
            ("three", 3),
        ]);

        for (k, v) in &hash.pairs {
            let ast::Expression::StringLiteral(literal) = k else {
                panic!("Expected key to be StringLiteral, got {k:?} instead");
            };
            let expected_value = expected[dbg!(literal.string().as_str())];
            test_integer_literal(&v, expected_value);
        }
    }

    #[test]
    fn parsing_empty_hash_literal() {
        let input = "{}";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(&parser);

        assert_eq!(program.statements.len(), 1);

        let ast::Statement::ExpressionStatement(stmt) = &program.statements[0] else {
            panic!(
                "Expected ExpressionStatement, got {:?} instead",
                program.statements[0]
            );
        };

        let ast::Expression::HashLiteral(hash) = &stmt.expression else {
            panic!("Expected HashLiteral, got {stmt:?} instead")
        };

        assert_eq!(hash.pairs.len(), 0);
    }

    #[test]
    fn parsing_hash_literals_with_expressions() {
        let input = r#" {"one": 0 + 1, "two": 10 - 8, "three": 15 / 5} "#;

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(&parser);

        assert_eq!(program.statements.len(), 1);

        let ast::Statement::ExpressionStatement(stmt) = &program.statements[0] else {
            panic!(
                "Expected ExpressionStatement, got {:?} instead",
                program.statements[0]
            );
        };

        let ast::Expression::HashLiteral(hash) = &stmt.expression else {
            panic!("Expected HashLiteral, got {stmt:?} instead")
        };

        assert_eq!(hash.pairs.len(), 3);

        let expected = HashMap::from([
            ("one", (0, "+", 1)),
            ("two", (10, "-", 8)),
            ("three", (15, "/", 5)),
        ]);

        for (k, v) in &hash.pairs {
            let ast::Expression::StringLiteral(literal) = k else {
                panic!("Expected key to be StringLiteral, got {k:?} instead");
            };
            let (left, operator, right) = expected[literal.string().as_str()];
            test_infix_expression(&v, Literal::Int(left), operator, Literal::Int(right));
        }
    }
}
