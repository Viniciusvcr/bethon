use crate::common::{
    grammar::{expr::Expr, operations::BinaryCompOp},
    typings::value::Value,
};

use super::{blue_pipe, print_marker, Color};

#[derive(Debug, Clone)]
#[allow(clippy::large_enum_variant)]
pub enum RuntimeError {
    AssertionFailed(usize),
    CompAssertionFailed(usize, Expr, BinaryCompOp, Value, Value),
    DivisionByZero(usize, usize, usize),
    NotCallable,
    Return(Value),
}

impl RuntimeError {
    pub fn format(&self, source_vec: &[String]) -> String {
        match self {
            RuntimeError::AssertionFailed(line) => format!("Assertion error on line {}:\n{}\n{} '{}'", line, blue_pipe(), blue_pipe(), source_vec.get(line - 1).unwrap()),
            RuntimeError::CompAssertionFailed(line, expr, op, left, right) => format!("Assertion error on line {}:\n{}\n{} '{}'\n{}\n{} Expected {}'{}'{} to be {} {}'{}'{}, but evaluation resulted {}'{}'{}", line, blue_pipe(), blue_pipe(), source_vec.get(line - 1).unwrap(), blue_pipe(), blue_pipe(), Color::Yellow, expr, Color::Reset, op.to_word(), Color::Yellow, right, Color::Reset, Color::Yellow, left, Color::Reset),
            RuntimeError::DivisionByZero(line, token_starts, token_ends) => format!("{}Runtime error caused by line {}:\n{}\n{} '{}'\n{} {}\n{} {}Reason: Attempting to divide by zero!{}", Color::White, line, blue_pipe(), blue_pipe(), source_vec.get(*line -1).unwrap(), blue_pipe(), print_marker(*token_starts, *token_ends, None), blue_pipe(), Color::Yellow, Color::Reset),
            RuntimeError::NotCallable => "Not callable error".to_string(),
            RuntimeError::Return(_) => "".to_string()
        }
    }
}
