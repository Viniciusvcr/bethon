use std::usize;

use crate::{
    expr::Value,
    expr::{BinaryCompOp, BinaryLogicOp, BinaryOp, UnaryOp},
    smntc_analyzer::Type,
    token::{TokenType, VarType},
};
#[derive(Debug)]
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
#[derive(Debug)]
pub enum RuntimeError {
    AssertionFailed(usize),
    CompAssertionFailed(usize, String, String, BinaryCompOp, Value),
    DivisionByZero(usize, usize, usize),
}

#[derive(Debug)]
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
}
#[derive(Debug)]
pub enum SmntcError {
    MismatchedTypes(Type, Type, Option<String>), // Expected, Found, Note
    IncompatibleBinArith(BinaryOp, Type, Type),  // Operation, Left, Right
    IncompatibleComparation(BinaryCompOp, Type, Type, Option<String>),
    IncompatibleLogicOp(BinaryLogicOp, Type, Type),
    IncompatibleLogicNot(Type),
    IncompatibleUnaryOp(UnaryOp, Type),
    VariableNotDeclared(usize, String),
    VariableAlreadyDeclared(usize, String),
    IncompatibleDeclaration(usize, VarType, Type),
    IfNotLogicalCondition,
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
#[derive(Debug)]
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

    fn syntax_error_template(
        &self,
        source_vec: &[String],
        line: usize,
        starts_at: Option<usize>,
        ends_at: Option<usize>,
        reason: String,
        marker: Option<String>,
    ) -> String {
        fn create_title(line: usize, starts_at: Option<usize>, ends_at: Option<usize>) -> String {
            match starts_at {
                Some(start_number) => match ends_at {
                    Some(end_number) => format!(
                        "Syntax error in line {} from character {} to {}:",
                        line, start_number, end_number
                    ),
                    None => format!(
                        "Syntax error in line {} from character {} to the end of file:",
                        line, start_number
                    ),
                },
                None => format!("Syntax error in line {}:", line),
            }
        }

        if marker.is_some() {
            format!(
                "{}{} \n{}\n{} '{}'\n{}{}\n{}\n{}{} Reason: {}{}",
                Color::White,
                create_title(line, starts_at, ends_at),
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
                create_title(line, starts_at, ends_at),
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
        match error {
            SmntcError::MismatchedTypes(expected, found, note) => {
                if note.is_some() {
                    format!(
                        "{}Mismatched types error: Expected {}{}{}, found {}{}{}\n{}\n{} {}",
                        Color::White,
                        Color::Yellow,
                        expected,
                        Color::White,
                        Color::Yellow,
                        found,
                        Color::White,
                        self.blue_pipe(),
                        self.blue_pipe(),
                        note.as_ref().unwrap()
                    )
                } else {
                    format!(
                        "{}Mismatched types error: Expected {}{}{}, found {}{}{}",
                        Color::White,
                        Color::Yellow,
                        expected,
                        Color::White,
                        Color::Yellow,
                        found,
                        Color::White,
                    )
                }
            }
            SmntcError::IncompatibleBinArith(op, left, right) => {
                format!(
                    "{}Incompatible operation error: Cannot use the {}'{}'{} binary operator with {}{}{} and {}{}{}.",
                    Color::White,
                    Color::Yellow,
                    op,
                    Color::White,
                    Color::Yellow,
                    left,
                    Color::White,
                    Color::Yellow,
                    right,
                    Color::Reset
                )
            },
            SmntcError::IncompatibleLogicNot(t) => format!("{} The 'not' operator expects the following expression to be of type {}{}{}, but the expression evaluates to {}{}{}.", Color::White, Color::Yellow, Type::Boolean(true), Color::White, Color::Yellow, t, Color::Reset),
            SmntcError::IncompatibleUnaryOp(op, t) => format!("{} The unary '{}' operator expects the following expression to be of type {}'int'{} or {}'float'{}, but the expression evaluates to {}{}{}.", Color::White, op, Color::Yellow, Color::White,Color::Yellow, Color::White, Color::Yellow, t, Color::Reset),
            SmntcError::IncompatibleComparation(op, l, r, note) => {
                if note.is_some() {
                    format!("{}Incompatible comparation: Cannot compare using the {}'{}'{} operator with {}{}{} and {}{}{}\n{}\n{} {}", Color::White, Color::Yellow, op, Color::White, Color::Yellow, l, Color::White, Color::Yellow, r, Color::Reset, self.blue_pipe(), self.blue_pipe(), note.as_ref().unwrap())
                } else {
                    format!("{}Incompatible comparation: Cannot compare using the {}'{}'{} operator with {}{}{} and {}{}{}", Color::White, Color::Yellow, op, Color::White, Color::Yellow, l, Color::White, Color::Yellow, r, Color::Reset)

                }
            }
            SmntcError::IncompatibleLogicOp(op, l, r) => format!("{}The {}'{}'{} operator expects the left and right expressions to be both of type {}{}{} or {}{}{}, but the expressions evaluates to {}{}{} and {}{}{} respectively.", Color::White, Color::Yellow, op, Color::White, Color::Yellow, Type::Boolean(true), Color::White, Color::Yellow, Type::Null, Color::White, Color::Yellow, l, Color::White, Color::Yellow, r, Color::White),
            SmntcError::IncompatibleDeclaration(line, expected, found) => format!("{}Incompatible assignment error on line {}\n{}\n{} '{}' \n{}\n{}{}Note: Declared type is {}'{}'{}, but the assigned expression evaluates to {}'{}'{}", Color::White, line, self.blue_pipe(), self.blue_pipe(), source_vec.get(*line - 1).unwrap(), self.blue_pipe(), self.blue_pipe(), Color::White, Color::Yellow, expected, Color::White, Color::Yellow, found, Color::Reset),
            SmntcError::VariableNotDeclared(line, var_name) => format!("{}Variable {}'{}'{} not found in this scope:\n{}\n{} '{}'\n{}\n{}{} Note: The attempt to read the undeclared variable is on line {}{}", Color::White, Color::Yellow, var_name, Color::White, self.blue_pipe(), self.blue_pipe(), source_vec.get(*line - 1).unwrap(), self.blue_pipe(), self.blue_pipe(), Color::Yellow, line, Color::Reset),
            SmntcError::VariableAlreadyDeclared(line, var_name) => format!("{}Redeclaration of variable {}'{}'{} on line {}:\n{}\n{} '{}'\n{}\n{}{} Note: It is not allowed to assign the same variable more than once.{}", Color::White, Color::Yellow, var_name, Color::White, line, self.blue_pipe(), self.blue_pipe(), source_vec.get(*line - 1).unwrap(), self.blue_pipe(), self.blue_pipe(), Color::Yellow, Color::Reset),
            SmntcError::IfNotLogicalCondition => "IfNotLogicalCondition".to_string()
        }
    }

    fn format_runtime_error(&self, error: &RuntimeError, source_vec: &[String]) -> String {
        use RuntimeError::*;

        match error {
            AssertionFailed(line) => format!("Assertion error on line {}:\n{}\n{} '{}'", line, self.blue_pipe(), self.blue_pipe(), source_vec.get(line - 1).unwrap()),
            CompAssertionFailed(line, left, right, op,  val) => format!("Assertion error on line {}:\n{}\n{} '{}'\n{}\n{} Assertion of the comparison {}'{} {} {}'{}\n{} was expected to be {}True{}, but evaluation resulted {}{}{}", line, self.blue_pipe(), self.blue_pipe(), source_vec.get(line - 1).unwrap(), self.blue_pipe(), self.blue_pipe(), Color::Yellow, left, op, right, Color::Reset, self.blue_pipe(), Color::Yellow, Color::Reset, Color::Yellow, val, Color::Reset),
            DivisionByZero(line, token_starts, token_ends) => format!("{}Runtime error caused by line {}:\n{}\n{} '{}'\n{} {}\n{} {}Reason: Attempting to divide by zero!{}", Color::White, line, self.blue_pipe(), self.blue_pipe(), source_vec.get(*line -1).unwrap(), self.blue_pipe(), self.print_marker(*token_starts, *token_ends, None), self.blue_pipe(), Color::Yellow, Color::Reset),
        }
    }

    fn format_scanner_error(&self, error: &ScannerError, source_vec: &[String]) -> String {
        match error {
            ScannerError::InvalidCharacter(line, starts_at, ends_at) => self.syntax_error_template(
                source_vec,
                *line,
                Some(*starts_at),
                Some(*ends_at),
                "Invalid character".to_string(),
                Some(self.print_marker(*starts_at, *ends_at, Some("here"))),
            ),
            ScannerError::LonelyBangSign(line, starts_at, ends_at) => self.syntax_error_template(
                source_vec,
                *line,
                Some(*starts_at),
                Some(*ends_at),
                "Invalid syntax. Did you mean 'not'?".to_string(),
                Some(self.print_marker(*starts_at, *ends_at, Some("here"))),
            ),
            ScannerError::InvalidToken(line, starts_at, ends_at, reason) => self
                .syntax_error_template(
                    source_vec,
                    *line,
                    Some(*starts_at),
                    Some(*ends_at),
                    reason.to_string(),
                    Some(self.print_marker(*starts_at, *ends_at, Some("here"))),
                ),
            ScannerError::MismatchedIdent(line, starts_at, ends_at) => self.syntax_error_template(
                source_vec,
                *line,
                Some(*starts_at),
                Some(*ends_at),
                "There is a conflicted usage of spaces and tabs indentation in this file"
                    .to_string(),
                Some(self.print_marker(*starts_at, *ends_at, Some("first conflict happens here"))),
            ),
            ScannerError::UnterminatedString(line) => self.syntax_error_template(
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
        match error {
            ParserError::MissingRightParen(line) => self.syntax_error_template(
                source_vec,
                *line,
                None,
                None,
                "Expected a ')' after this expression".to_string(),
                None,
            ),
            ParserError::MissingExpression(line) => self.syntax_error_template(
                source_vec,
                *line,
                None,
                None,
                "Missing an expression".to_string(),
                None,
            ),
            ParserError::AssignmentExpected(line, equal_plcmnt) => self.syntax_error_template(
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
            ParserError::TypeNotDefined(line, starts_at, ends_at) => self.syntax_error_template(
                source_vec,
                *line,
                Some(*starts_at),
                Some(*ends_at),
                "Declared type is not a known type".to_string(),
                Some(self.print_marker(*starts_at, *ends_at, Some("here"))),
            ),
            ParserError::ExpectedColon(line, colon_plcmnt) => self.syntax_error_template(
                source_vec,
                *line,
                None,
                None,
                "':' expected after identifier".to_string(),
                Some(self.print_marker(*colon_plcmnt, *colon_plcmnt + 1, Some("here"))),
            ),
            ParserError::Expected(tt, line) => self.syntax_error_template(
                source_vec,
                *line,
                None,
                None,
                format!("'{:#?}' expected", tt),
                None,
            ),
            ParserError::UnexpectedIdent(line, starts_at, ends_at) => self.syntax_error_template(
                source_vec,
                *line,
                Some(*starts_at),
                Some(*ends_at),
                "Indentation not expected here".to_string(),
                Some(self.print_marker(*starts_at, *ends_at, Some("here"))),
            ),
            ParserError::UnexpectedDeident(line) => self.syntax_error_template(
                source_vec,
                *line,
                None,
                None,
                "Unexpected deindentation".to_string(),
                None,
            ),
            ParserError::IndentedElse(line, starts_at, ends_at) => self.syntax_error_template(
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
            ParserError::DanglingElse(line, starts_at, ends_at) => self.syntax_error_template(
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
        }
    }
}
