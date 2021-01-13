use std::fmt::Debug;

use crate::{
    environment::Environment,
    interpreter::{Interpreter, InterpreterResult},
    stmt::Stmt,
    token::{Token, VarType},
};

use super::Value;
#[derive(Debug, Clone, PartialEq)]
pub struct Callable {
    pub env: Environment,
    pub id_token: Token,
    pub params: Vec<Token>,
    pub body: Vec<Stmt>,
    pub ret_type: VarType,
}

impl Callable {
    pub fn new(
        env: Environment,
        id_token: Token,
        params: Vec<Token>,
        body: Vec<Stmt>,
        ret_type: VarType,
    ) -> Self {
        Self {
            env,
            id_token,
            params,
            body,
            ret_type,
        }
    }

    pub fn call(&self, interpreter: &mut Interpreter, args: &[Value]) -> InterpreterResult {
        interpreter.eval_func(self, args)
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
