use std::rc::Rc;

use crate::{
    environment::Environment,
    interpreter::{Interpreter, InterpreterResult},
    stmt::Stmt,
    token::Token,
};

use super::Value;
#[derive(Debug, Clone, PartialEq)]
pub struct Callable {
    env: Environment,
    id_token: Token,
    params: Vec<Token>,
    body: Rc<Stmt>,
}

impl Callable {
    pub fn new(env: Environment, id_token: Token, params: Vec<Token>, body: Rc<Stmt>) -> Self {
        Self {
            env,
            id_token,
            params,
            body,
        }
    }

    pub fn call(&self, _interpreter: &mut Interpreter, _args: &[Value]) -> InterpreterResult {
        todo!("Implement Callable::call")
        // interpreter.eval_func(self, args)
    }

    pub fn arity(&self) -> usize {
        self.params.len()
    }
}

impl std::fmt::Display for Callable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<function {}>", self.id_token.lexeme)
    }
}
