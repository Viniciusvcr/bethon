#![allow(clippy::too_many_arguments)]

use std::usize;

use crate::{
    expr::operations::*,
    expr::value::Value,
    smntc_analyzer::Type,
    token::{token_type::TokenType, VarType},
};
#[derive(Debug, Clone)]
pub enum ScannerError {
    // Line, line_start, line_char, reason
    InvalidToken(usize, usize, usize, String),
    // Line, line, line
    InvalidCharacter(usize, usize, usize),
    // Line, line, line
    LonelyBangSign(usize, usize, usize),
    // Line
    UnterminatedString(usize),
    MismatchedIdent(usize, usize, usize),
}
#[derive(Debug, Clone)]
#[allow(clippy::large_enum_variant)]
pub enum RuntimeError {
    AssertionFailed(usize),
    CompAssertionFailed(usize, String, String, BinaryCompOp, Value),
    DivisionByZero(usize, usize, usize),
    NotCallable,
    Return(Value),
}

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
}
#[derive(Debug, Clone)]
pub enum SmntcError {
    MismatchedTypes(usize, usize, usize, Type, Type), // Expected, Found, Note
    IncompatibleBinArith(usize, usize, usize, BinaryOp, Type, Type), // Operation, Left, Right
    IncompatibleComparation(usize, usize, usize, BinaryCompOp, Type, Type),
    IncompatibleLogicOp(usize, usize, usize, BinaryLogicOp, Type, Type),
    IncompatibleLogicNot(usize, usize, usize, Type),
    IncompatibleUnaryOp(usize, usize, usize, UnaryOp, Type),
    VariableNotDeclared(usize, usize, usize, String),
    VariableAlreadyDeclared(usize, usize, usize, String),
    IncompatibleDeclaration(usize, usize, usize, VarType, Type),
    MissingReturns(usize, usize, usize, VarType),
    IfNotLogicalCondition(usize, usize, usize, Type),
    NotCallable(usize, usize, usize, Type),
    WrongArity(usize, usize, usize, usize, usize),
}

#[allow(dead_code)]
enum Color {
    Reset,
    White,
    Red,
    Green,
    Blue,
    Yellow,
}

impl std::fmt::Display for Color {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Color::*;
        match self {
            Reset => write!(f, "\x1b[0m"),
            White => write!(f, "\x1b[1;37m"),
            Red => write!(f, "\x1b[1;31m"),
            Green => write!(f, "\x1b[1;32m"),
            Blue => write!(f, "\x1b[1;34m"),
            Yellow => write!(f, "\x1b[1;33m"),
        }
    }
}
#[derive(Debug, Clone)]
pub enum Error {
    UnexpectedFail,
    Input(String, String),
    Scanner(ScannerError),
    Parser(ParserError),
    Smntc(SmntcError),
    Runtime(RuntimeError),
}

impl Error {
    fn blue_pipe(&self) -> String {
        format!("{}|{}", Color::Blue, Color::Reset)
    }

    fn print_marker(&self, start: usize, end: usize, message: Option<&str>) -> String {
        let mut arrow: String = "  ".to_string();
        for i in 0..end {
            if i >= start {
                arrow.push('^');
            } else {
                arrow.push(' ');
            }
        }

        if message.is_some() {
            arrow.push_str(&format!("--- {}", message.unwrap()));
        }

        arrow
    }

    fn static_error_template(
        &self,
        error_type: &str,
        source_vec: &[String],
        line: usize,
        starts_at: Option<usize>,
        ends_at: Option<usize>,
        reason: String,
        marker: Option<String>,
    ) -> String {
        fn create_title(
            error_type: &str,
            line: usize,
            starts_at: Option<usize>,
            ends_at: Option<usize>,
        ) -> String {
            match starts_at {
                Some(start_number) => match ends_at {
                    Some(end_number) => format!(
                        "{} error in line {} from character {} to {}:",
                        error_type, line, start_number, end_number
                    ),
                    None => format!(
                        "{} error in line {} from character {} to the end of file:",
                        error_type, line, start_number
                    ),
                },
                None => format!("{} error in line {}:", error_type, line),
            }
        }

        if marker.is_some() {
            format!(
                "{}{} \n{}\n{} '{}'\n{}{}\n{}\n{}{} Reason: {}{}",
                Color::White,
                create_title(error_type, line, starts_at, ends_at),
                self.blue_pipe(),
                self.blue_pipe(),
                source_vec.get(line - 1).unwrap(),
                self.blue_pipe(),
                marker.unwrap(),
                self.blue_pipe(),
                self.blue_pipe(),
                Color::Yellow,
                reason,
                Color::Reset
            )
        } else {
            format!(
                "{}{} \n{}\n{} '{}'\n{}\n{}{} Reason: {}{}",
                Color::White,
                create_title(error_type, line, starts_at, ends_at),
                self.blue_pipe(),
                self.blue_pipe(),
                source_vec.get(line - 1).unwrap(),
                self.blue_pipe(),
                self.blue_pipe(),
                Color::Yellow,
                reason,
                Color::Reset
            )
        }
    }

    pub fn show_error(&self, file: Option<&str>, source_vec: Option<&[String]>) {
        if let Some(filename) = file {
            eprintln!(
                "{}Bethon Error in file {}'{}':",
                Color::Red,
                Color::Reset,
                filename
            )
        } else {
            eprintln!("{}Bethon Error{}:", Color::Red, Color::Reset)
        }
        eprintln!("{} {}", self.blue_pipe(), self.format_error(source_vec));
    }

    fn format_error(&self, source_vec: Option<&[String]>) -> String {
        use Error::*;

        match self {
            Input(reason, note) => format!(
                "{}Input error: {}\n{}\n{} {}",
                Color::White,
                reason,
                self.blue_pipe(),
                self.blue_pipe(),
                note
            ),
            UnexpectedFail => format!(
                "{}Unexpected fail: Something went wrong while parsing the file{}",
                Color::White,
                Color::Reset
            ),
            Scanner(scanner_error) => self.format_scanner_error(scanner_error, source_vec.unwrap()),
            Parser(parser_error) => self.format_parser_error(parser_error, source_vec.unwrap()),
            Smntc(smntc_error) => self.format_smntc_error(smntc_error, source_vec.unwrap()),
            Runtime(runtime_error) => self.format_runtime_error(runtime_error, source_vec.unwrap()),
        }
    }

    fn format_smntc_error(&self, error: &SmntcError, source_vec: &[String]) -> String {
        let error_type = "Semantic";

        match error {
            SmntcError::MismatchedTypes(line, starts_at, ends_at, expected, found) => self.static_error_template(
                error_type,
                source_vec,
                *line,
                Some(*starts_at),
                Some(*ends_at),
                format!("Expected {}{}{}, found {}{}{}", Color::White,
                expected,
                Color::Yellow,
                Color::White,
                found,
                Color::Reset),
                Some(self.print_marker(*starts_at, *ends_at, None))
            ),
            SmntcError::MissingReturns(line, starts_at, ends_at, expected) => self.static_error_template(
                error_type,
                source_vec,
                *line,
                Some(*starts_at),
                Some(*ends_at),
                format!("Defined return type is {}{}{}, but not 'return' found in the function", Color::White,
                expected,
                Color::Yellow),
                Some(self.print_marker(*starts_at, *ends_at, None))
            ),
            SmntcError::IncompatibleBinArith(line, starts_at, ends_at, op, left, right) => self.static_error_template(
                error_type,
                source_vec,
                *line,
                Some(*starts_at),
                Some(*ends_at),
                format!("Incompatible operation error: Cannot use the {}'{}'{} binary operator with {}{}{} and {}{}{}",
                        Color::White,
                        op,
                        Color::Yellow,
                        Color::White,
                        left,
                        Color::Yellow,
                        Color::White,
                        right,
                        Color::Reset),
                Some(self.print_marker(*starts_at, *ends_at, None))
            ),
            SmntcError::IncompatibleLogicNot(line, starts_at, ends_at, t) => self.static_error_template(
                error_type,
                source_vec,
                *line,
                Some(*starts_at),
                Some(*ends_at),
                format!("The 'not' operator expects an expression of type {}{}{}, but the expression evaluates to {}{}{}.",
                        Color::White,
                        Type::Boolean(true),
                        Color::Yellow,
                        Color::White,
                        t,
                        Color::Reset),
                Some(self.print_marker(*starts_at, *ends_at, None))
            ),
            SmntcError::IncompatibleUnaryOp(line, starts_at, ends_at, op, t) => self.static_error_template(
                error_type,
                source_vec,
                *line,
                Some(*starts_at),
                Some(*ends_at),
                format!("The unary {}'{}'{} operator expects {}'int'{} or {}'float'{}, but the expression evaluates to {}{}{}.",
                        Color::White,
                        op,
                        Color::Yellow,
                        Color::White,
                        Color::Yellow,
                        Color::White,
                        Color::Yellow,
                        Color::White,
                        t,
                        Color::Reset),
                Some(self.print_marker(*starts_at, *ends_at, None))
            ),
            SmntcError::IncompatibleComparation(line, starts_at, ends_at, op, l, r) => self.static_error_template(
                error_type,
                source_vec,
                *line,
                Some(*starts_at),
                Some(*ends_at),
                format!("Incompatible comparation: Cannot compare using the {}'{}'{} operator with {}{}{} and {}{}{}",
                        Color::White,
                        op,
                        Color::Yellow,
                        Color::White,
                        l,
                        Color::Yellow,
                        Color::White,
                        r,
                        Color::Reset),
                Some(self.print_marker(*starts_at, *ends_at, None))
            ),
            SmntcError::IncompatibleLogicOp(line, starts_at, ends_at,op, l, r) => self.static_error_template(
                error_type,
                source_vec,
                *line,
                Some(*starts_at),
                Some(*ends_at),
                format!("The {}'{}'{} operator expects {}{}{} or {}{}{}, but the expressions evaluates to {}{}{} and {}{}{} respectively.",
                        Color::White,
                        op,
                        Color::Yellow,
                        Color::White,
                        Type::Boolean(true),
                        Color::Yellow,
                        Color::White,
                        Type::Null,
                        Color::Yellow,
                        Color::White,
                        l,
                        Color::Yellow,
                        Color::White,
                        r,
                        Color::Yellow),
                Some(self.print_marker(*starts_at, *ends_at, None))
            ),
            SmntcError::IncompatibleDeclaration(line, starts_at, ends_at, expected, found) => self.static_error_template(
                error_type,
                source_vec,
                *line,
                Some(*starts_at),
                Some(*ends_at),
                format!("Declared type is {}'{}'{}, but the assigned expression evaluates to {}'{}'{}",
                        Color::White,
                        expected,
                        Color::Yellow,
                        Color::White,
                        found,
                        Color::Reset),
                Some(self.print_marker(*starts_at, *ends_at, Some(&format!("evaluates to {}", found))))
            ),
            SmntcError::VariableNotDeclared(line, starts_at, ends_at, var_name) => self.static_error_template(
                error_type,
                source_vec,
                *line,
                Some(*starts_at),
                Some(*ends_at),
                format!("Attempting to read an undeclared variable '{}'", var_name),
                Some(self.print_marker(*starts_at, *ends_at, Some("not found in this scope")))
            ),
            SmntcError::VariableAlreadyDeclared(line, starts_at, ends_at, var_name) => self.static_error_template(
                error_type,
                source_vec,
                *line,
                Some(*starts_at),
                Some(*ends_at),
                format!("Attempting to reassign the variable '{}'", var_name),
                Some(self.print_marker(*starts_at, *ends_at, Some("reassign is not allowed")))
            ),
            SmntcError::IfNotLogicalCondition(line, starts_at, ends_at, t) => self.static_error_template(
                error_type,
                source_vec,
                *line,
                Some(*starts_at),
                Some(*ends_at),
                format!("The 'if' condition must resolve to {}'{}'{}, but {}'{}'{} was found",
                        Color::White,
                        VarType::Boolean,
                        Color::Yellow,
                        Color::White,
                        t,
                        Color::Yellow),
                Some(self.print_marker(*starts_at, *ends_at, Some("here")))
            ),
            SmntcError::NotCallable(line, starts_at, ends_at, t) => self.static_error_template(
                error_type,
                source_vec,
                *line,
                Some(*starts_at),
                Some(*ends_at),
                format!("Cannot call {}'{}'{}, only {}'{}'{}",
                        Color::White,
                        t,
                        Color::Yellow,
                        Color::White,
                        VarType::Function,
                        Color::Yellow),
                Some(self.print_marker(*starts_at, *ends_at, Some("call happens here")))
            ),
            SmntcError::WrongArity(line, starts_at, ends_at, expected, found) => self.static_error_template(
                error_type,
                source_vec,
                *line,
                Some(*starts_at),
                Some(*ends_at),
                format!("Expected {}{}{} arguments, found {}{}{}",
                        Color::White,
                        expected,
                        Color::Yellow,
                        Color::White,
                        found,
                        Color::Yellow),
                Some(self.print_marker(*starts_at, *ends_at, Some("here")))
            ),
        }
    }

    fn format_runtime_error(&self, error: &RuntimeError, source_vec: &[String]) -> String {
        use RuntimeError::*;

        match error {
            AssertionFailed(line) => format!("Assertion error on line {}:\n{}\n{} '{}'", line, self.blue_pipe(), self.blue_pipe(), source_vec.get(line - 1).unwrap()),
            CompAssertionFailed(line, left, right, op,  val) => format!("Assertion error on line {}:\n{}\n{} '{}'\n{}\n{} Assertion of the comparison {}'{} {} {}'{}\n{} was expected to be {}True{}, but evaluation resulted {}{}{}", line, self.blue_pipe(), self.blue_pipe(), source_vec.get(line - 1).unwrap(), self.blue_pipe(), self.blue_pipe(), Color::Yellow, left, op, right, Color::Reset, self.blue_pipe(), Color::Yellow, Color::Reset, Color::Yellow, val, Color::Reset),
            DivisionByZero(line, token_starts, token_ends) => format!("{}Runtime error caused by line {}:\n{}\n{} '{}'\n{} {}\n{} {}Reason: Attempting to divide by zero!{}", Color::White, line, self.blue_pipe(), self.blue_pipe(), source_vec.get(*line -1).unwrap(), self.blue_pipe(), self.print_marker(*token_starts, *token_ends, None), self.blue_pipe(), Color::Yellow, Color::Reset),
            RuntimeError::NotCallable => "Not callable error".to_string(),
            RuntimeError::Return(_) => "".to_string()
        }
    }

    fn format_scanner_error(&self, error: &ScannerError, source_vec: &[String]) -> String {
        let error_type = "Syntax";

        match error {
            ScannerError::InvalidCharacter(line, starts_at, ends_at) => self.static_error_template(
                error_type,
                source_vec,
                *line,
                Some(*starts_at),
                Some(*ends_at),
                "Invalid character".to_string(),
                Some(self.print_marker(*starts_at, *ends_at, Some("here"))),
            ),
            ScannerError::LonelyBangSign(line, starts_at, ends_at) => self.static_error_template(
                error_type,
                source_vec,
                *line,
                Some(*starts_at),
                Some(*ends_at),
                "Invalid syntax. Did you mean 'not'?".to_string(),
                Some(self.print_marker(*starts_at, *ends_at, Some("here"))),
            ),
            ScannerError::InvalidToken(line, starts_at, ends_at, reason) => self
                .static_error_template(
                    error_type,
                    source_vec,
                    *line,
                    Some(*starts_at),
                    Some(*ends_at),
                    reason.to_string(),
                    Some(self.print_marker(*starts_at, *ends_at, Some("here"))),
                ),
            ScannerError::MismatchedIdent(line, starts_at, ends_at) => self.static_error_template(
                error_type,
                source_vec,
                *line,
                Some(*starts_at),
                Some(*ends_at),
                "There is a conflicted usage of spaces and tabs indentation in this file"
                    .to_string(),
                Some(self.print_marker(*starts_at, *ends_at, Some("first conflict happens here"))),
            ),
            ScannerError::UnterminatedString(line) => self.static_error_template(
                error_type,
                source_vec,
                *line,
                None,
                None,
                "Unterminated String. Maybe you forgot a '\"'?".to_string(),
                None,
            ),
        }
    }

    fn format_parser_error(&self, error: &ParserError, source_vec: &[String]) -> String {
        let error_type = "Syntax";
        match error {
            ParserError::MissingRightParen(line) => self.static_error_template(
                error_type,
                source_vec,
                *line,
                None,
                None,
                "Expected a ')' after this expression".to_string(),
                None,
            ),
            ParserError::MissingExpression(line) => self.static_error_template(
                error_type,
                source_vec,
                *line,
                None,
                None,
                "Missing an expression".to_string(),
                None,
            ),
            ParserError::AssignmentExpected(line, equal_plcmnt) => self.static_error_template(
                error_type,
                source_vec,
                *line,
                None,
                None,
                "Variable declaration expects an '=' and an expression after type declaration"
                    .to_string(),
                Some(self.print_marker(
                    *equal_plcmnt,
                    *equal_plcmnt,
                    Some("Missing an assignment"),
                )),
            ),
            ParserError::TypeNotDefined(line, starts_at, ends_at) => self.static_error_template(
                error_type,
                source_vec,
                *line,
                Some(*starts_at),
                Some(*ends_at),
                "Declared type is not a known type".to_string(),
                Some(self.print_marker(*starts_at, *ends_at, Some("here"))),
            ),
            ParserError::ExpectedColon(line, colon_plcmnt) => self.static_error_template(
                error_type,
                source_vec,
                *line,
                None,
                None,
                "':' expected after identifier".to_string(),
                Some(self.print_marker(*colon_plcmnt, *colon_plcmnt + 1, Some("here"))),
            ),
            ParserError::Expected(tt, line) => self.static_error_template(
                error_type,
                source_vec,
                *line,
                None,
                None,
                format!("'{:#?}' expected", tt),
                None,
            ),
            ParserError::UnexpectedIdent(line, starts_at, ends_at) => self.static_error_template(
                error_type,
                source_vec,
                *line,
                Some(*starts_at),
                Some(*ends_at),
                "Indentation not expected here".to_string(),
                Some(self.print_marker(*starts_at, *ends_at, Some("here"))),
            ),
            ParserError::UnexpectedDeident(line) => self.static_error_template(
                error_type,
                source_vec,
                *line,
                None,
                None,
                "Unexpected deindentation".to_string(),
                None,
            ),
            ParserError::IndentedElse(line, starts_at, ends_at) => self.static_error_template(
                error_type,
                source_vec,
                *line,
                Some(*starts_at),
                Some(*ends_at),
                "'else' needs to be unindented from 'if.".to_string(),
                Some(self.print_marker(
                    *starts_at,
                    *ends_at,
                    Some("try removing the indentation at the start"),
                )),
            ),
            ParserError::DanglingElse(line, starts_at, ends_at) => self.static_error_template(
                error_type,
                source_vec,
                *line,
                Some(*starts_at),
                Some(*ends_at),
                "'else' needs to be binded to a 'if. Maybe the indentation is wrong?".to_string(),
                Some(self.print_marker(
                    *starts_at,
                    *ends_at,
                    Some("this 'else' is not binded to a 'if."),
                )),
            ),
            ParserError::MaxFuntionArgsReached(line) => self.static_error_template(
                error_type,
                source_vec,
                *line,
                None,
                None,
                "A function can have 255 arguments maximum".to_string(),
                None,
            ),
            // todo write error
            ParserError::MissingFunctionReturnType(line, starts_at, ends_at) => self
                .static_error_template(
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
                    Some(self.print_marker(*starts_at, *ends_at, Some("here"))),
                ),
            ParserError::MissingParameterType(line, starts_at, ends_at) => self
                .static_error_template(
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
                    Some(self.print_marker(*starts_at, *ends_at, Some("after this"))),
                ),
        }
    }
}
