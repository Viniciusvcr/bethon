use crate::common::{grammar::operations::BinaryCompOp, typings::value::Value};

use super::{blue_pipe, print_marker, Color};

#[derive(Debug, Clone)]
#[allow(clippy::large_enum_variant)]
pub enum RuntimeError {
    AssertionFailed(usize),
    CompAssertionFailed(usize, String, String, BinaryCompOp, Value),
    DivisionByZero(usize, usize, usize),
    NotCallable,
    Return(Value),
}

impl RuntimeError {
    pub fn format(&self, source_vec: &[String]) -> String {
        use RuntimeError::*;

        match self {
            AssertionFailed(line) => format!("Assertion error on line {}:\n{}\n{} '{}'", line, blue_pipe(), blue_pipe(), source_vec.get(line - 1).unwrap()),
            CompAssertionFailed(line, left, right, op,  val) => format!("Assertion error on line {}:\n{}\n{} '{}'\n{}\n{} Assertion of the comparison {}'{} {} {}'{}\n{} was expected to be {}True{}, but evaluation resulted {}{}{}", line, blue_pipe(), blue_pipe(), source_vec.get(line - 1).unwrap(), blue_pipe(), blue_pipe(), Color::Yellow, left, op, right, Color::Reset, blue_pipe(), Color::Yellow, Color::Reset, Color::Yellow, val, Color::Reset),
            DivisionByZero(line, token_starts, token_ends) => format!("{}Runtime error caused by line {}:\n{}\n{} '{}'\n{} {}\n{} {}Reason: Attempting to divide by zero!{}", Color::White, line, blue_pipe(), blue_pipe(), source_vec.get(*line -1).unwrap(), blue_pipe(), print_marker(*token_starts, *token_ends, None), blue_pipe(), Color::Yellow, Color::Reset),
            RuntimeError::NotCallable => "Not callable error".to_string(),
            RuntimeError::Return(_) => "".to_string()
        }
    }
}
