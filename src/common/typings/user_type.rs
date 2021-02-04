use std::collections::HashMap;

use crate::common::symbol::token::Token;

use super::{value::Value, var_type::VarType};

#[derive(Debug, PartialEq, Clone, Default)]
pub struct UserType {
    pub name_token: Token,
    pub attrs: Vec<(Token, VarType)>,
}

impl UserType {
    pub fn from_var_type(name_token: &Token) -> Self {
        Self {
            name_token: name_token.clone(),
            attrs: Vec::default(),
        }
    }

    pub fn new(name_token: &Token, attrs: &[(Token, VarType)]) -> Self {
        Self {
            name_token: name_token.clone(),
            attrs: attrs.to_owned(),
        }
    }

    pub fn arity(&self) -> usize {
        self.attrs.len()
    }
}

#[derive(Debug, PartialEq, Clone, Default)]
pub struct UserInstance {
    pub type_name: UserType,
    pub attrs: HashMap<String, Value>,
}

impl UserInstance {
    pub fn new(type_name: &UserType, attrs: &[(Token, Value)]) -> Self {
        let mut hash = HashMap::default();

        for (id, value) in attrs {
            hash.insert(id.lexeme(), value.clone());
        }

        Self {
            type_name: type_name.clone(),
            attrs: hash,
        }
    }
}
