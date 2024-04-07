use std::collections::HashMap;
use std::fmt::Display;
use std::hash::{Hash, Hasher};
use std::{fmt::Debug, rc::Rc, str::FromStr};

use crate::{
    ast::{self, MonkeyNode},
    builtins,
    environment::Env,
};

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    String(String),
    Array(Vec<Rc<Object>>),
    Hash(HashObj),
    ReturnValue(Rc<Object>),
    Function(Function),
    Builtin(BuiltinFn),
    Null,
}

impl Object {
    pub fn filter_singleton(obj: Rc<Self>, env: &Env) -> Rc<Self> {
        match *obj {
            Self::Boolean(b) => env.borrow().get_singleton(Some(b)),
            Self::Null => env.borrow().get_singleton(None),
            _ => obj,
        }
    }

    pub fn filter_option(opt: Option<&Rc<Self>>, env: &Env) -> Rc<Self> {
        match opt {
            Some(obj) => Rc::clone(obj),
            None => env.borrow().get_singleton(None)
        }
    }

    pub fn is_hashable(&self) -> bool {
        match self {
            Self::Integer(_)
            | Self::Boolean(_)
            | Self::String(_)
            | Self::Array(_)
            | Self::Builtin(_)
            | Self::Null => true,
            _ => false,
        }
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Integer(v) => write!(f, "{v}"),
            Self::Boolean(v) => write!(f, "{v}"), 
            Self::String(v) => write!(f, r#""{}""#, v.escape_default()),
            Self::Array(v) => {
                let elements = v.iter().map(|e| e.to_string()).collect::<Vec<_>>().join(", ");
                write!(f, "[{elements}]")
            }
            Self::Hash(HashObj(v)) => {
                let pairs = v
                    .iter()
                    .map(|(k, v)| format!("{k}: {v}"))
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "{{{pairs}}}")
            }
            Self::ReturnValue(v) => write!(f, "{v}"),
            Self::Function(func) => {
                let params = func.parameters.iter().map(|p| p.string()).collect::<Vec<_>>().join(", ");
                write!(f, "fn({params})")
            }
            Self::Builtin(func) => write!(f, "<built-in function {}>", func.name),
            Self::Null => write!(f, "null"),
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
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "ERROR: {}", self.message)
    }
}

macro_rules! new_error {
    ($($arg:tt)*) => {{
        let res = Error { message: std::fmt::format(std::format_args!($($arg)*)) };
        res
    }};
}

pub(crate) use new_error;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HashObj(pub HashMap<Rc<Object>, Rc<Object>>);

impl Hash for HashObj {
    fn hash<H: Hasher>(&self, _state: &mut H) {
        panic!("Attempted to hash Object::Hash");
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

impl Hash for Function {
    fn hash<H: Hasher>(&self, _state: &mut H) {
        panic!("Attempted to hash Object::Function");
    }
}

#[derive(Hash)]
pub struct BuiltinFn {
    pub name: String,
    pub func: fn(builtins::In) -> builtins::Out,
}

impl FromStr for BuiltinFn {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let func = match s {
            "puts" => builtins::puts,
            "print" => builtins::print,
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

#[cfg(test)]
mod tests {
    use std::hash::DefaultHasher;

    use super::*;

    fn hash_key<T: Hash>(t: &T) -> u64 {
        let mut hasher = DefaultHasher::new();
        t.hash(&mut hasher);
        hasher.finish()
    }

    #[test]
    fn string_hash_key() {
        let hello1 = Object::String(String::from("Hello World"));
        let hello2 = Object::String(String::from("Hello World"));
        let diff1 = Rc::new(Object::String(String::from("My name is johnny")));
        let diff2 = Rc::new(Object::String(String::from("My name is johnny")));

        assert_eq!(hash_key(&hello1), hash_key(&hello2));
        assert_eq!(hash_key(&diff1), hash_key(&diff2));
        assert_ne!(hash_key(&hello1), hash_key(&diff1));
    }
}
