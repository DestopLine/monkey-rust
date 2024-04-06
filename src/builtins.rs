use std::rc::Rc;

use crate::object::{Error, Object};

pub type In = Vec<Rc<Object>>;
pub type Out = Result<Rc<Object>, Error>;

fn check_length(expected: usize, length: usize) -> Result<(), Error> {
    if length != expected {
        Err(Error::arg_num(length, expected))
    } else {
        Ok(())
    }
}

pub fn len(args: In) -> Out {
    check_length(1, args.len())?;

    match &*args[0] {
        Object::String(string) => Ok(Rc::new(Object::Integer(string.len() as i64))),
        obj @ _ => Err(Error::arg_type("len", &obj)),
    }
}
