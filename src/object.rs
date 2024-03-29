#[derive(Debug, PartialEq, Eq)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    Null,
}

impl Object {
    pub fn inspect(&self) -> String {
        match self {
            Self::Integer(v) => v.to_string(),
            Self::Boolean(v) => v.to_string(),
            Self::Null => String::from("null"),
        }
    }
}
