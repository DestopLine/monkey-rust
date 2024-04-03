use std::rc::Rc;

use crate::{
    ast::{self, MonkeyNode},
    environment::Env,
};

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    ReturnValue(Rc<Object>),
    Function(Function),
    Null,
}

impl Object {
    pub fn inspect(&self) -> String {
        match self {
            Self::Integer(v) => v.to_string(),
            Self::Boolean(v) => v.to_string(),
            Self::ReturnValue(v) => v.inspect(),
            Self::Function(func) => {
                let params: Vec<String> = func.parameters.iter().map(|p| p.string()).collect();

                format!("fn({}) {{\n{}\n}}", params.join(", "), func.body.string())
            }
            Self::Null => String::from("null"),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Error {
    pub message: String,
}

impl Error {
    pub fn inspect(&self) -> String {
        format!("ERROR: {}", self.message)
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    pub parameters: Vec<ast::Identifier>,
    pub body: ast::BlockStatement,
    pub env: Env,
}

impl PartialEq for Function {
    fn eq(&self, other: &Self) -> bool {
        self as *const _ == other as *const _
    }
}

impl Eq for Function {}
