use std::{fmt::Debug, rc::Rc, str::FromStr};

use crate::{
    ast::{self, MonkeyNode},
    builtins,
    environment::Env,
};

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    String(String),
    Array(Vec<Rc<Object>>),
    ReturnValue(Rc<Object>),
    Function(Function),
    Builtin(BuiltinFn),
    Null,
}

impl Object {
    pub fn inspect(&self) -> String {
        match self {
            Self::Integer(v) => v.to_string(),
            Self::Boolean(v) => v.to_string(),
            Self::String(v) => v.to_string(),
            Self::Array(v) => {
                let elements = v.iter().map(|e| e.inspect()).collect::<Vec<_>>().join(", ");
                format!("[{elements}]")
            }
            Self::ReturnValue(v) => v.inspect(),
            Self::Function(func) => {
                let params: Vec<String> = func.parameters.iter().map(|p| p.string()).collect();
                format!("fn({}) {{\n{}\n}}", params.join(", "), func.body.string())
            }
            Self::Builtin(func) => format!("<builtin `{}`>", func.name),
            Self::Null => String::from("null"),
        }
    }

    pub fn filter_singleton(obj: Rc<Self>, env: &Env) -> Rc<Self> {
        match *obj {
            Self::Boolean(b) => env.borrow().get_singleton(Some(b)),
            Self::Null => env.borrow().get_singleton(None),
            _ => obj,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Error {
    pub message: String,
}

impl Error {
    pub fn arg_num(got: usize, expected: usize) -> Self {
        Self {
            message: format!("Wrong number of arguments: got {got}, expected {expected}"),
        }
    }

    pub fn arg_type(fn_name: &str, got: &Object) -> Self {
        Self {
            message: format!("Argument to `{fn_name}` not supported, got {got:?}"),
        }
    }

    pub fn inspect(&self) -> String {
        format!("ERROR: {}", self.message)
    }
}

macro_rules! new_error {
    ($($arg:tt)*) => {{
        let res = Error { message: std::fmt::format(std::format_args!($($arg)*)) };
        res
    }};
}

pub(crate) use new_error;

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

pub struct BuiltinFn {
    pub name: String,
    pub func: fn(builtins::In) -> builtins::Out,
}

impl FromStr for BuiltinFn {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let func = match s {
            "len" => builtins::len,
            "first" => builtins::first,
            "last" => builtins::last,
            "rest" => builtins::rest,
            "push" => builtins::push,
            _ => {
                return Err(Error {
                    message: format!("Builtin not found: {s}"),
                })
            }
        };
        Ok(Self {
            name: s.to_string(),
            func,
        })
    }
}

impl Debug for BuiltinFn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "BuiltinFn {{ name: {}, func: builtin }}", self.name)
    }
}

impl PartialEq for BuiltinFn {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl Eq for BuiltinFn {}

impl Clone for BuiltinFn {
    fn clone(&self) -> Self {
        Self::from_str(&self.name).unwrap()
    }
}
