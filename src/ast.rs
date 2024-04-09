use crate::token::Token;

pub trait MonkeyNode {
    fn string(&self) -> String;
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
    ExpressionStatement(ExpressionStatement),
    BlockStatement(BlockStatement),
}

impl MonkeyNode for Statement {
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
    StringLiteral(StringLiteral),
    ArrayLiteral(ArrayLiteral),
    HashLiteral(HashLiteral),
    PrefixExpression(PrefixExpression),
    InfixExpression(InfixExpression),
    IfExpression(IfExpression),
    FunctionLiteral(FunctionLiteral),
    CallExpression(CallExpression),
    IndexExpression(IndexExpression),
    None,
}

impl MonkeyNode for Expression {
    fn string(&self) -> String {
        match self {
            Self::Identifier(node) => node.string(),
            Self::IntegerLiteral(node) => node.string(),
            Self::Boolean(node) => node.string(),
            Self::StringLiteral(node) => node.string(),
            Self::ArrayLiteral(node) => node.string(),
            Self::HashLiteral(node) => node.string(),
            Self::PrefixExpression(node) => node.string(),
            Self::InfixExpression(node) => node.string(),
            Self::IfExpression(node) => node.string(),
            Self::FunctionLiteral(node) => node.string(),
            Self::CallExpression(node) => node.string(),
            Self::IndexExpression(node) => node.string(),
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

impl MonkeyNode for Program {
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

impl MonkeyNode for LetStatement {
    fn string(&self) -> String {
        let mut out = String::new();

        out.push_str(&(self.token.to_string() + " "));
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

impl MonkeyNode for ReturnStatement {
    fn string(&self) -> String {
        let mut out = String::new();

        out.push_str(&(self.token.to_string() + " "));

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

impl MonkeyNode for ExpressionStatement {
    fn string(&self) -> String {
        self.expression.string()
    }
}

#[derive(Debug, Clone)]
pub struct Identifier {
    pub token: Token,
    pub value: String,
}

impl MonkeyNode for Identifier {
    fn string(&self) -> String {
        self.value.clone()
    }
}

#[derive(Debug, Clone)]
pub struct IntegerLiteral {
    pub token: Token,
    pub value: i64,
}

impl MonkeyNode for IntegerLiteral {
    fn string(&self) -> String {
        self.token.to_string()
    }
}

#[derive(Debug, Clone)]
pub struct PrefixExpression {
    pub token: Token,
    pub operator: String,
    pub right: Box<Expression>,
}

impl MonkeyNode for PrefixExpression {
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

impl MonkeyNode for InfixExpression {
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

impl MonkeyNode for Boolean {
    fn string(&self) -> String {
        self.token.to_string()
    }
}

#[derive(Debug, Clone)]
pub struct StringLiteral {
    pub token: Token,
    pub value: String,
}

impl MonkeyNode for StringLiteral {
    fn string(&self) -> String {
        self.token.to_string()
    }
}

#[derive(Debug, Clone)]
pub struct ArrayLiteral {
    pub token: Token,
    pub elements: Vec<Expression>,
}

impl MonkeyNode for ArrayLiteral {
    fn string(&self) -> String {
        let elements = self
            .elements
            .iter()
            .map(|e| e.string())
            .collect::<Vec<_>>()
            .join(", ");
        format!("[{elements}]")
    }
}

#[derive(Debug, Clone)]
pub struct HashLiteral {
    pub token: Token,
    pub pairs: Vec<(Expression, Expression)>,
}

impl MonkeyNode for HashLiteral {
    fn string(&self) -> String {
        let pairs = self
            .pairs
            .iter()
            .map(|(k, v)| format!("{}: {}", k.string(), v.string()))
            .collect::<Vec<_>>()
            .join(", ");
        format!("{{{pairs}}}")
    }
}

#[derive(Debug, Clone)]
pub struct BlockStatement {
    pub token: Token,
    pub statements: Vec<Statement>,
}

impl MonkeyNode for BlockStatement {
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

impl MonkeyNode for IfExpression {
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

impl MonkeyNode for FunctionLiteral {
    fn string(&self) -> String {
        let params: Vec<_> = self.parameters.iter().map(|p| p.string()).collect();

        format!(
            "{}({}) {}",
            self.token.to_string(),
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

impl MonkeyNode for CallExpression {
    fn string(&self) -> String {
        let args: Vec<_> = self.arguments.iter().map(|p| p.string()).collect();

        format!("{}({})", self.function.string(), args.join(", "),)
    }
}

#[derive(Clone, Debug)]
pub struct IndexExpression {
    pub token: Token,
    pub left: Box<Expression>,
    pub index: Box<Expression>,
}

impl MonkeyNode for IndexExpression {
    fn string(&self) -> String {
        format!("({}[{}])", self.left.string(), self.index.string())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::token::TokenKind;

    #[test]
    fn string() {
        let program = Program {
            statements: vec![Statement::Let(LetStatement {
                token: Token::new(TokenKind::Let, 0, 3),
                name: Identifier {
                    token: Token::new(TokenKind::Ident(String::from("myVar")), 4, 9),
                    value: "myVar".to_string(),
                },
                value: Expression::Identifier(Identifier {
                    token: Token::new(TokenKind::Ident(String::from("anotherVar")), 12, 22),
                    value: "anotherVar".to_string(),
                }),
            })],
        };

        assert_eq!(program.string(), String::from("let myVar = anotherVar;"));
    }
}
