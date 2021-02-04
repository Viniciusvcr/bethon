pub mod callable;
pub mod operations;
pub mod value;

use crate::token::Token;
use std::{
    hash::{Hash, Hasher},
    usize,
};

use operations::*;
use value::Value;

#[derive(Clone, PartialEq, Debug)]
#[allow(clippy::large_enum_variant)]
pub enum Expr {
    BinaryArith(Box<Expr>, OpWithToken<BinaryOp>, Box<Expr>),
    BinaryComp(Box<Expr>, OpWithToken<BinaryCompOp>, Box<Expr>),
    BinaryLogic(Box<Expr>, OpWithToken<BinaryLogicOp>, Box<Expr>),
    LogicNot((Box<Expr>, Token)),
    Unary(OpWithToken<UnaryOp>, Box<Expr>),
    Grouping(Box<Expr>),
    Literal(OpWithToken<Value>),
    Variable(Token),
    Call(Box<Expr>, Vec<Expr>),
    Get(Box<Expr>, Token),
}

impl Hash for &Expr {
    fn hash<H>(&self, hasher: &mut H)
    where
        H: Hasher,
    {
        std::ptr::hash(self, hasher)
    }
}

impl Expr {
    pub fn get_token(&self) -> &Token {
        match self {
            Expr::BinaryArith(_, op_and_token, _) => &op_and_token.token,
            Expr::BinaryComp(_, op_and_token, _) => &op_and_token.token,
            Expr::BinaryLogic(_, op_and_token, _) => &op_and_token.token,
            Expr::LogicNot((_, token)) => &token,
            Expr::Unary(op_and_token, _) => &op_and_token.token,
            Expr::Grouping(expr) => expr.get_token(),
            Expr::Literal(op_and_token) => &op_and_token.token,
            Expr::Variable(token) => &token,
            Expr::Call(callee, _) => callee.get_token(),
            Expr::Get(_, token) => token,
        }
    }

    pub fn get_expr_placement(&self) -> (usize, usize) {
        match self {
            Expr::BinaryArith(l, _, r) => (l.get_expr_placement().0, r.get_expr_placement().1),
            Expr::BinaryComp(l, _, r) => (l.get_expr_placement().0, r.get_expr_placement().1),
            Expr::BinaryLogic(l, _, r) => (l.get_expr_placement().0, r.get_expr_placement().1),
            Expr::LogicNot((_, token)) => (token.placement.starts_at, token.placement.ends_at),
            Expr::Unary(_, r) => {
                let (x, y) = r.get_expr_placement();
                (x - 1, y)
            }
            Expr::Grouping(expr) => {
                let (x, y) = expr.get_expr_placement();
                (x - 1, y + 1)
            }
            Expr::Literal(op_and_token) => (
                op_and_token.token.placement.starts_at,
                op_and_token.token.placement.ends_at,
            ),
            Expr::Variable(token) => (token.placement.starts_at, token.placement.ends_at),
            Expr::Call(callee, params) => {
                let (x, mut y) = callee.get_expr_placement();

                if !params.is_empty() {
                    let (_, new_y) = params.last().unwrap().get_expr_placement();

                    y = new_y;
                } else {
                    y += 1;
                }

                (x, y + 1)
            }
            Expr::Get(_, token) => (token.placement.starts_at - 1, token.placement.ends_at),
        }
    }

    pub fn get_line(&self) -> usize {
        self.get_token().placement.line
    }

    pub fn placement(&self) -> (usize, usize, usize) {
        let (x, y) = self.get_expr_placement();
        (self.get_line(), x, y)
    }
}

impl Eq for &Expr {}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::BinaryArith(left, op_and_token, right) => {
                write!(f, "{} {} {}", left, op_and_token.op, right)
            }
            Expr::BinaryComp(left, op_and_token, right) => {
                write!(f, "{} {} {}", left, op_and_token.op, right)
            }
            Expr::BinaryLogic(left, op_and_token, right) => {
                write!(f, "{} {} {}", left, op_and_token.op, right)
            }
            Expr::LogicNot((expr, _token)) => write!(f, "not {}", expr),
            Expr::Unary(op_and_token, expr) => write!(f, "{} {}", op_and_token.op, expr),
            Expr::Grouping(expr) => write!(f, "({})", expr),
            Expr::Literal(value_and_token) => write!(f, "{}", value_and_token.op),
            Expr::Variable(token) => write!(f, "{}", token.lexeme()),
            Expr::Call(callee, arguments) => {
                write!(f, "{}(", callee)?;
                for arg in arguments {
                    write!(f, "{}", arg)?;
                }
                write!(f, ")")
            }
            Expr::Get(_, token) => write!(f, ".{}", token.lexeme),
        }
    }
}
