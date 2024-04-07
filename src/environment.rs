use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::object::Object;

pub type Env = Rc<RefCell<Environment>>;

pub fn new_env() -> Env {
    Rc::new(RefCell::new(Environment::new()))
}

#[derive(Debug, Clone)]
pub struct Environment {
    store: HashMap<String, Rc<Object>>,
    outer: Option<Env>,
}

impl Environment {
    pub fn new() -> Self {
        let mut env = Self {
            store: HashMap::new(),
            outer: None,
        };
        env.set_singletons();
        env
    }

    fn set_singletons(&mut self) {
        self.set("$true".to_string(), Rc::new(Object::Boolean(true)));
        self.set("$false".to_string(), Rc::new(Object::Boolean(false)));
        self.set("$null".to_string(), Rc::new(Object::Null));
    }

    pub fn enclosed_by(outer: Rc<RefCell<Environment>>) -> Self {
        Self {
            store: HashMap::new(),
            outer: Some(outer),
        }
    }

    pub fn get_singleton(&self, val: Option<bool>) -> Rc<Object> {
        match val {
            Some(boolean) => self.get(&format!("${boolean}")).unwrap(),
            None => self.get(&"$null".to_string()).unwrap(),
        }
    }

    pub fn get(&self, name: &String) -> Option<Rc<Object>> {
        match (self.store.get(name), &self.outer) {
            (Some(rc), _) => Some(Rc::clone(rc)),
            (None, Some(outer)) => outer.borrow().get(name),
            (None, None) => None,
        }
    }

    pub fn set(&mut self, name: String, val: Rc<Object>) -> Option<Rc<Object>> {
        self.store.insert(name, val)
    }
}
