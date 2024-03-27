pub enum ObjectType {
    Integer,
    Boolean,
    Null,
}

pub trait Object {
    fn obj_type(&self) -> ObjectType;
    fn inspect(&self) -> String;
}

pub struct Integer {
    value: i64,
}

impl Object for Integer {
    fn obj_type(&self) -> ObjectType {
        ObjectType::Integer
    }

    fn inspect(&self) -> String {
        format!("{}", self.value)
    }
}

pub struct Boolean {
    value: bool,
}

impl Object for Boolean {
    fn obj_type(&self) -> ObjectType {
        ObjectType::Boolean
    }

    fn inspect(&self) -> String {
        format!("{}", self.value)
    }
}

pub struct Null;

impl Object for Null {
    fn obj_type(&self) -> ObjectType {
        ObjectType::Null
    }

    fn inspect(&self) -> String {
        String::from("null")
    }
}
