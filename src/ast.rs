use crate::token::Token;
use std::fmt::Debug;

pub trait Node {
    fn token_literal(&self) -> String;
}

#[derive(Debug)]
pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
}

impl Statement {
    fn statement_node(&self) {}
}

impl Node for Statement {
    fn token_literal(&self) -> String {
        match self {
            Self::Let(node) => node.token.literal.clone(),
            Self::Return(node) => node.token.literal.clone(),
        }
    }
}

#[derive(Debug)]
pub enum Expression {
    Identifier(Identifier),
    None,
}

impl Expression {
    fn expression_node(&self) {}
}

impl Node for Expression {
    fn token_literal(&self) -> String {
        match self {
            Self::Identifier(node) => node.token.literal.clone(),
            _ => String::new(),
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
}
