#[derive(Debug, PartialEq, Eq)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    ReturnValue(Box<Object>),
    Null,
}

impl Object {
    pub fn inspect(&self) -> String {
        match self {
            Self::Integer(v) => v.to_string(),
            Self::Boolean(v) => v.to_string(),
            Self::ReturnValue(v) => v.inspect(),
            Self::Null => String::from("null"),
        }
    }
}
