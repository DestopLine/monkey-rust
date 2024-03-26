use crate::token::Token;
use std::fmt::Debug;

pub trait Node {
    fn token_literal(&self) -> String;
    fn string(&self) -> String;
}

#[derive(Debug)]
pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
    ExpressionStatement(ExpressionStatement),
}

impl Statement {
    fn statement_node(&self) {}
}

impl Node for Statement {
    fn token_literal(&self) -> String {
        match self {
            Self::Let(node) => node.token.literal.clone(),
            Self::Return(node) => node.token.literal.clone(),
            Self::ExpressionStatement(node) => node.token.literal.clone(),
        }
    }

    fn string(&self) -> String {
        match self {
            Self::Let(node) => node.string(),
            Self::Return(node) => node.string(),
            Self::ExpressionStatement(node) => node.string(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expression {
    Identifier(Identifier),
    IntegerLiteral(IntegerLiteral),
    PrefixExpression(PrefixExpression),
    InfixExpression(InfixExpression),
    None,
}

impl Expression {
    fn expression_node(&self) {}
}

impl Node for Expression {
    fn token_literal(&self) -> String {
        match self {
            Self::Identifier(node) => node.token.literal.clone(),
            Self::IntegerLiteral(node) => node.token.literal.clone(),
            Self::PrefixExpression(node) => node.token.literal.clone(),
            Self::InfixExpression(node) => node.token.literal.clone(),
            Self::None => String::new(),
        }
    }

    fn string(&self) -> String {
        match self {
            Self::Identifier(node) => node.string(),
            Self::IntegerLiteral(node) => node.string(),
            Self::PrefixExpression(node) => node.string(),
            Self::InfixExpression(node) => node.string(),
            Self::None => String::new(),
        }
    }
}

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Statement>,
}

impl Program {
    pub fn new() -> Self {
        Self {
            statements: Vec::new(),
        }
    }
}

impl Node for Program {
    fn token_literal(&self) -> String {
        if self.statements.len() > 0 {
            self.statements[0].token_literal()
        } else {
            String::new()
        }
    }

    fn string(&self) -> String {
        let mut out = String::new();

        for stmt in &self.statements {
            out.push_str(&stmt.string());
        }

        out
    }
}

#[derive(Debug)]
pub struct LetStatement {
    pub token: Token,
    pub name: Identifier,
    pub value: Expression,
}

impl Node for LetStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn string(&self) -> String {
        let mut out = String::new();

        out.push_str(&(self.token_literal() + " "));
        out.push_str(&self.name.string());
        out.push_str(" = ");

        if !matches!(self.value, Expression::None) {
            out.push_str(&self.value.string());
        }

        out.push_str(";");
        out
    }
}

#[derive(Debug)]
pub struct ReturnStatement {
    pub token: Token,
    pub return_value: Expression,
}

impl Node for ReturnStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn string(&self) -> String {
        let mut out = String::new();

        out.push_str(&(self.token_literal() + " "));

        if !matches!(self.return_value, Expression::None) {
            out.push_str(&self.return_value.string());
        }

        out.push_str(";");
        out
    }
}

#[derive(Debug)]
pub struct ExpressionStatement {
    pub token: Token,
    pub expression: Expression,
}

impl Node for ExpressionStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn string(&self) -> String {
        self.expression.string()
    }
}

#[derive(Debug, Clone)]
pub struct Identifier {
    pub token: Token,
    pub value: String,
}

impl Node for Identifier {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn string(&self) -> String {
        self.value.clone()
    }
}

#[derive(Debug, Clone)]
pub struct IntegerLiteral {
    pub token: Token,
    pub value: i64,
}

impl Node for IntegerLiteral {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn string(&self) -> String {
        self.token_literal()
    }
}

#[derive(Debug, Clone)]
pub struct PrefixExpression {
    pub token: Token,
    pub operator: String,
    pub right: Box<Expression>,
}

impl Node for PrefixExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn string(&self) -> String {
        format!("({}{})", self.operator, self.right.string())
    }
}

#[derive(Debug, Clone)]
pub struct InfixExpression {
    pub token: Token, // The operator token
    pub operator: String,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}

impl Node for InfixExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn string(&self) -> String {
        format!("({} {} {})", self.left.string(), self.operator, self.right.string())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::token::TokenType;

    #[test]
    fn string() {
        let program = Program {
            statements: vec![
                Statement::Let(LetStatement {
                    token: Token { toktype: TokenType::Let, literal: "let".to_string() },
                    name: Identifier {
                        token: Token { toktype: TokenType::Ident, literal: "myVar".to_string() },
                        value: "myVar".to_string(),
                    },
                    value: Expression::Identifier(Identifier {
                        token: Token { toktype: TokenType::Ident, literal: "anotherVar".to_string() },
                        value: "anotherVar".to_string(),
                    }),
                }),
            ],
        };

        assert_eq!(program.string(), String::from("let myVar = anotherVar;"));
    }
}
