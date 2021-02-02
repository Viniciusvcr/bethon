use crate::token::{token_type::TokenType, VarType};

use super::{print_marker, static_error_template, Color};

#[derive(Debug, Clone)]
pub enum ParserError {
    // Line
    MissingRightParen(usize),
    // Note
    MissingExpression(usize),
    // line, missing_equal_column
    AssignmentExpected(usize, usize),
    // line, token_start, token_end
    TypeNotDefined(usize, usize, usize),
    // line, missing_colon
    ExpectedColon(usize, usize),
    Expected(TokenType, usize),
    UnexpectedIdent(usize, usize, usize),
    UnexpectedDeident(usize),
    IndentedElse(usize, usize, usize),
    DanglingElse(usize, usize, usize),
    MaxFuntionArgsReached(usize),
    MissingFunctionReturnType(usize, usize, usize),
    MissingParameterType(usize, usize, usize),
    RegularClass(usize, usize, usize),
}

impl ParserError {
    pub fn format(&self, source_vec: &[String]) -> String {
        let error_type = "Syntax";

        match self {
            ParserError::MissingRightParen(line) => static_error_template(
                error_type,
                source_vec,
                *line,
                None,
                None,
                "Expected a ')' after this expression".to_string(),
                None,
            ),
            ParserError::MissingExpression(line) => static_error_template(
                error_type,
                source_vec,
                *line,
                None,
                None,
                "Missing an expression".to_string(),
                None,
            ),
            ParserError::AssignmentExpected(line, equal_plcmnt) => static_error_template(
                error_type,
                source_vec,
                *line,
                None,
                None,
                "Variable declaration expects an '=' and an expression after type declaration"
                    .to_string(),
                Some(print_marker(
                    *equal_plcmnt,
                    *equal_plcmnt,
                    Some("Missing an assignment"),
                )),
            ),
            ParserError::TypeNotDefined(line, starts_at, ends_at) => static_error_template(
                error_type,
                source_vec,
                *line,
                Some(*starts_at),
                Some(*ends_at),
                "Declared type is not a known type".to_string(),
                Some(print_marker(*starts_at, *ends_at, Some("here"))),
            ),
            ParserError::ExpectedColon(line, colon_plcmnt) => static_error_template(
                error_type,
                source_vec,
                *line,
                None,
                None,
                "':' expected after identifier".to_string(),
                Some(print_marker(*colon_plcmnt, *colon_plcmnt + 1, Some("here"))),
            ),
            ParserError::Expected(tt, line) => static_error_template(
                error_type,
                source_vec,
                *line,
                None,
                None,
                format!("'{:#?}' expected", tt),
                None,
            ),
            ParserError::UnexpectedIdent(line, starts_at, ends_at) => static_error_template(
                error_type,
                source_vec,
                *line,
                Some(*starts_at),
                Some(*ends_at),
                "Indentation not expected here".to_string(),
                Some(print_marker(*starts_at, *ends_at, Some("here"))),
            ),
            ParserError::UnexpectedDeident(line) => static_error_template(
                error_type,
                source_vec,
                *line,
                None,
                None,
                "Unexpected deindentation".to_string(),
                None,
            ),
            ParserError::IndentedElse(line, starts_at, ends_at) => static_error_template(
                error_type,
                source_vec,
                *line,
                Some(*starts_at),
                Some(*ends_at),
                "'else' needs to be unindented from 'if.".to_string(),
                Some(print_marker(
                    *starts_at,
                    *ends_at,
                    Some("try removing the indentation at the start"),
                )),
            ),
            ParserError::DanglingElse(line, starts_at, ends_at) => static_error_template(
                error_type,
                source_vec,
                *line,
                Some(*starts_at),
                Some(*ends_at),
                "'else' needs to be binded to a 'if. Maybe the indentation is wrong?".to_string(),
                Some(print_marker(
                    *starts_at,
                    *ends_at,
                    Some("this 'else' is not binded to a 'if."),
                )),
            ),
            ParserError::MaxFuntionArgsReached(line) => static_error_template(
                error_type,
                source_vec,
                *line,
                None,
                None,
                "A function can have 255 arguments maximum".to_string(),
                None,
            ),
            ParserError::MissingFunctionReturnType(line, starts_at, ends_at) => {
                static_error_template(
                    error_type,
                    source_vec,
                    *line,
                    Some(*starts_at),
                    Some(*ends_at),
                    format!(
                        "Expected one of {}'{}', '{}', '{}', '{}', '{}'.",
                        Color::White,
                        VarType::Boolean,
                        VarType::Integer,
                        VarType::Float,
                        VarType::Str,
                        VarType::PythonNone,
                    ),
                    Some(print_marker(*starts_at, *ends_at, Some("here"))),
                )
            }
            ParserError::MissingParameterType(line, starts_at, ends_at) => static_error_template(
                error_type,
                source_vec,
                *line,
                Some(*starts_at),
                Some(*ends_at),
                format!(
                    "Expected one of {}'{}', '{}', '{}', '{}'.",
                    Color::White,
                    VarType::Boolean,
                    VarType::Integer,
                    VarType::Float,
                    VarType::Str
                ),
                Some(print_marker(*starts_at, *ends_at, Some("after this"))),
            ),
            ParserError::RegularClass(line, starts_at, ends_at) => static_error_template(
                error_type,
                source_vec,
                *line,
                Some(*starts_at),
                Some(*ends_at),
                "Regular Python classes are not suported, only Dataclasses".to_string(),
                Some(print_marker(
                    *starts_at,
                    *ends_at,
                    Some("try marking with '@dataclass' before 'class'"),
                )),
            ),
        }
    }
}
