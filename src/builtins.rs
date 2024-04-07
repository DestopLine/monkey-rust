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

pub fn puts(args: In) -> Out {
    for arg in &*args {
        if let Object::String(s) = &**arg {
            println!("{s}")
        } else {
            println!("{arg}")
        }
    }
    Ok(Rc::new(Object::Null))
}

pub fn len(args: In) -> Out {
    check_length(1, args.len())?;

    match &*args[0] {
        Object::String(string) => Ok(Rc::new(Object::Integer(string.len() as i64))),
        Object::Array(arr) => Ok(Rc::new(Object::Integer(arr.len() as i64))),
        obj @ _ => Err(Error::arg_type("len", &obj)),
    }
}

pub fn first(args: In) -> Out {
    check_length(1, args.len())?;

    match &*args[0] {
        Object::Array(arr) => Ok(Rc::clone(&arr.get(0).unwrap_or(&Rc::new(Object::Null)))),
        obj @ _ => Err(Error::arg_type("first", &obj)),
    }
}

pub fn last(args: In) -> Out {
    check_length(1, args.len())?;

    match &*args[0] {
        Object::Array(arr) => Ok(Rc::clone(&arr.last().unwrap_or(&Rc::new(Object::Null)))),
        obj @ _ => Err(Error::arg_type("last", &obj)),
    }
}

pub fn rest(args: In) -> Out {
    check_length(1, args.len())?;

    match &*args[0] {
        Object::Array(arr) => {
            let out = if let Some(slice) = arr.get(1..) {
                Object::Array(Vec::from(slice))
            } else {
                Object::Null
            };
            Ok(Rc::new(out))
        }
        obj @ _ => Err(Error::arg_type("rest", &obj)),
    }
}

pub fn push(args: In) -> Out {
    check_length(2, args.len())?;

    let obj = &*args[0];
    let Object::Array(arr) = obj else {
        return Err(Error::arg_type("push", obj));
    };

    let mut new_vec = arr.clone();
    new_vec.push(Rc::clone(&args[1]));
    Ok(Rc::new(Object::Array(new_vec)))
}
