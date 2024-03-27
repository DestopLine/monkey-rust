use crate::token::Token;
use std::fmt::Debug;

pub trait Node {
    fn token_literal(&self) -> String;
    fn string(&self) -> String;
}

#[derive(Debug, Clone)]
pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
    ExpressionStatement(ExpressionStatement),
    BlockStatement(BlockStatement),
}

impl Node for Statement {
    fn token_literal(&self) -> String {
        match self {
            Self::Let(node) => node.token.literal.clone(),
            Self::Return(node) => node.token.literal.clone(),
            Self::ExpressionStatement(node) => node.token.literal.clone(),
            Self::BlockStatement(node) => node.token.literal.clone(),
        }
    }

    fn string(&self) -> String {
        match self {
            Self::Let(node) => node.string(),
            Self::Return(node) => node.string(),
            Self::ExpressionStatement(node) => node.string(),
            Self::BlockStatement(node) => node.string(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expression {
    Identifier(Identifier),
    IntegerLiteral(IntegerLiteral),
    Boolean(Boolean),
    PrefixExpression(PrefixExpression),
    InfixExpression(InfixExpression),
    IfExpression(IfExpression),
    FunctionLiteral(FunctionLiteral),
    CallExpression(CallExpression),
    None,
}

impl Node for Expression {
    fn token_literal(&self) -> String {
        match self {
            Self::Identifier(node) => node.token.literal.clone(),
            Self::IntegerLiteral(node) => node.token.literal.clone(),
            Self::Boolean(node) => node.token.literal.clone(),
            Self::PrefixExpression(node) => node.token.literal.clone(),
            Self::InfixExpression(node) => node.token.literal.clone(),
            Self::IfExpression(node) => node.token.literal.clone(),
            Self::FunctionLiteral(node) => node.token.literal.clone(),
            Self::CallExpression(node) => node.token.literal.clone(),
            Self::None => String::new(),
        }
    }

    fn string(&self) -> String {
        match self {
            Self::Identifier(node) => node.string(),
            Self::IntegerLiteral(node) => node.string(),
            Self::Boolean(node) => node.string(),
            Self::PrefixExpression(node) => node.string(),
            Self::InfixExpression(node) => node.string(),
            Self::IfExpression(node) => node.string(),
            Self::FunctionLiteral(node) => node.string(),
            Self::CallExpression(node) => node.string(),
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

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
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
        format!(
            "({} {} {})",
            self.left.string(),
            self.operator,
            self.right.string()
        )
    }
}

#[derive(Debug, Clone)]
pub struct Boolean {
    pub token: Token,
    pub value: bool,
}

impl Node for Boolean {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn string(&self) -> String {
        self.token_literal()
    }
}

#[derive(Debug, Clone)]
pub struct BlockStatement {
    pub token: Token,
    pub statements: Vec<Statement>,
}

impl Node for BlockStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn string(&self) -> String {
        self.statements.iter().map(|s| s.string()).collect()
    }
}

#[derive(Debug, Clone)]
pub struct IfExpression {
    pub token: Token,
    pub condition: Box<Expression>,
    pub consequence: BlockStatement,
    pub alternative: Option<BlockStatement>,
}

impl Node for IfExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn string(&self) -> String {
        format!(
            "if {} {} {}",
            self.condition.string(),
            self.consequence.string(),
            if let Some(alt) = &self.alternative {
                format!("else {}", alt.string())
            } else {
                String::new()
            },
        )
    }
}

#[derive(Debug, Clone)]
pub struct FunctionLiteral {
    pub token: Token,
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
}

impl Node for FunctionLiteral {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn string(&self) -> String {
        let params: Vec<_> = self.parameters.iter().map(|p| p.string()).collect();

        format!(
            "{}({}) {}",
            self.token_literal(),
            params.join(", "),
            self.body.string(),
        )
    }
}

#[derive(Debug, Clone)]
pub struct CallExpression {
    pub token: Token,
    pub function: Box<Expression>,
    pub arguments: Vec<Expression>,
}

impl Node for CallExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn string(&self) -> String {
        let args: Vec<_> = self.arguments.iter().map(|p| p.string()).collect();

        format!("{}({})", self.function.string(), args.join(", "),)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::token::TokenType;

    #[test]
    fn string() {
        let program = Program {
            statements: vec![Statement::Let(LetStatement {
                token: Token {
                    toktype: TokenType::Let,
                    literal: "let".to_string(),
                },
                name: Identifier {
                    token: Token {
                        toktype: TokenType::Ident,
                        literal: "myVar".to_string(),
                    },
                    value: "myVar".to_string(),
                },
                value: Expression::Identifier(Identifier {
                    token: Token {
                        toktype: TokenType::Ident,
                        literal: "anotherVar".to_string(),
                    },
                    value: "anotherVar".to_string(),
                }),
            })],
        };

        assert_eq!(program.string(), String::from("let myVar = anotherVar;"));
    }
}
