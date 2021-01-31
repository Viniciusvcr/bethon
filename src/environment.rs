use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{expr::value::Value, smntc_analyzer::Type, token::Token};

#[derive(PartialEq, Clone, Default, Debug)]
pub struct Environment<T> {
    pub env: Rc<Env<T>>,
}

impl<T> Environment<T>
where
    T: Clone,
{
    pub fn new(current: HashMap<String, T>) -> Self {
        Self {
            env: Env {
                previous: None,
                current: current.into(),
            }
            .into(),
        }
    }

    pub fn define(&mut self, identifier: String, value: T) -> Option<T> {
        self.env.current.borrow_mut().insert(identifier, value)
    }

    pub fn get(&self, identifier: &str) -> Option<T> {
        self.env.get(identifier)
    }

    pub fn push(&mut self) {
        self.env = Env::new_with_prev(self.env.clone()).into();
    }

    pub fn pop(&mut self) {
        self.env = self.env.previous.clone().unwrap();
    }

    pub fn declared_keys(&self) -> Vec<String> {
        self.env.declared_keys()
    }

    pub fn current(&self) -> HashMap<String, T> {
        self.env.current.borrow().clone()
    }

    pub fn current_keys(&self) -> Vec<String> {
        self.env.current_keys()
    }
}

#[derive(PartialEq, Clone, Default, Debug)]
pub struct Env<T> {
    pub previous: Option<Rc<Self>>,
    pub current: RefCell<HashMap<String, T>>,
}

impl<T> Env<T>
where
    T: Clone,
{
    fn declared_keys(&self) -> Vec<String> {
        if let Some(prev) = self.previous.clone() {
            let mut x = self
                .current
                .borrow()
                .keys()
                .map(|s| s.to_string())
                .collect::<Vec<String>>();

            x.append(&mut prev.declared_keys());

            x
        } else {
            self.current
                .borrow()
                .keys()
                .map(|s| s.to_string())
                .collect()
        }
    }

    fn current_keys(&self) -> Vec<String> {
        self.current
            .borrow()
            .keys()
            .map(|s| s.to_string())
            .collect()
    }

    fn new_with_prev(previous: Rc<Self>) -> Self {
        Self {
            previous: Some(previous),
            current: Default::default(),
        }
    }

    fn get(&self, key: &str) -> Option<T> {
        if self.current.borrow().contains_key(key) {
            self.current.borrow().get(key).cloned()
        } else if self.previous.is_some() {
            self.previous.clone().unwrap().get(key)
        } else {
            None
        }
    }
}

#[derive(Clone, Default, Debug, PartialEq)]
pub struct SmntcEnvValue {
    pub t: Type,
    pub defined: bool,
    pub token: Token,
}

impl SmntcEnvValue {
    pub fn new(t: Type, defined: bool, token: Token) -> Self {
        Self { t, defined, token }
    }
}

pub type InterpreterEnvironment = Environment<Value>;
pub type SemanticEnvironment = Environment<SmntcEnvValue>;
