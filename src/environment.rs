use crate::expr::value::Value;
use std::{cell::RefCell, collections::HashMap, rc::Rc};

#[derive(PartialEq, Clone, Default, Debug)]
pub struct Environment {
    pub env: Rc<Env>,
}

impl Environment {
    pub fn new(current: HashMap<String, Value>) -> Self {
        Self {
            env: Env {
                previous: None,
                current: current.into(),
            }
            .into(),
        }
    }

    pub fn define(&mut self, identifier: String, value: Value) {
        self.env.current.borrow_mut().insert(identifier, value);
    }

    pub fn get(&self, identifier: &str) -> Option<Value> {
        self.env.get(identifier)
    }

    pub fn push(&mut self) {
        self.env = Env::new_with_prev(self.env.clone()).into();
    }

    pub fn pop(&mut self) {
        self.env = self.env.previous.clone().unwrap();
    }
}

#[derive(PartialEq, Clone, Default, Debug)]
pub struct Env {
    previous: Option<Rc<Self>>,
    pub current: RefCell<HashMap<String, Value>>,
}

impl Env {
    fn new_with_prev(previous: Rc<Self>) -> Self {
        Self {
            previous: Some(previous),
            current: Default::default(),
        }
    }

    fn get(&self, key: &str) -> Option<Value> {
        if self.current.borrow().contains_key(key) {
            self.current.borrow().get(key).cloned()
        } else if self.previous.is_some() {
            self.previous.clone().unwrap().get(key)
        } else {
            None
        }
    }
}
