use crate::{
    expr::{BinaryCompOp, BinaryLogicOp, BinaryOp, UnaryOp},
    smntc_analyzer::Type,
};

pub enum ScannerError {
    // Line, line_start, line_char, reason
    InvalidToken(usize, usize, usize, String),
    // Line, line, line
    InvalidCharacter(usize, usize, usize),
    // Line, line, line
    LonelyBangSign(usize, usize, usize),
    // Line
    UnterminatedString(usize),
}

pub enum RuntimeError {
    DivisionByZero(usize, usize, usize),
    NotAllowed, // REFACTOR
}

pub enum ParserError {
    // Line
    MissingRightParen(usize),
    // Note
    MissingExpression(Option<usize>),
}

pub enum SmntcError {
    MismatchedTypes(Type, Type, Option<String>), // Expected, Found, Note
    IncompatibleBinArith(BinaryOp, Type, Type),  // Operation, Left, Right
    IncompatibleComparation(BinaryCompOp, Type, Type, Option<String>),
    IncompatibleLogicOp(BinaryLogicOp, Type, Type),
    IncompatibleLogicNot(Type),
    IncompatibleUnaryOp(UnaryOp, Type),
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

    fn print_marker(&self, start: usize, end: usize) -> String {
        let mut arrow: String = "  ".to_string();
        for i in 0..end {
            if i >= start {
                arrow.push('^');
            } else {
                arrow.push(' ');
            }
        }
        arrow
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

    fn format_smntc_error(&self, error: &SmntcError, _source_vec: &[String]) -> String {
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
            SmntcError::IncompatibleLogicNot(t) => format!("{} The 'not' operator expects the following expression to be of type {}{}{}, but the expression evaluates to {}{}{}.", Color::White, Color::Yellow, Type::Bool, Color::White, Color::Yellow, t, Color::Reset),
            SmntcError::IncompatibleUnaryOp(op, t) => format!("{} The unary '{}' operator expects the following expression to be of type {}{}{}, but the expression evaluates to {}{}{}.", Color::White, op, Color::Yellow, Type::Num, Color::White, Color::Yellow, t, Color::Reset),
            SmntcError::IncompatibleComparation(op, l, r, note) => {
                if note.is_some() {
                    format!("{}Incompatible comparation: Cannot compare using the {}'{}'{} operator with {}{}{} and {}{}{}\n{}\n{} {}", Color::White, Color::Yellow, op, Color::White, Color::Yellow, l, Color::White, Color::Yellow, r, Color::Reset, self.blue_pipe(), self.blue_pipe(), note.as_ref().unwrap())
                } else {
                    format!("{}Incompatible comparation: Cannot compare using the {}'{}'{} operator with {}{}{} and {}{}{}", Color::White, Color::Yellow, op, Color::White, Color::Yellow, l, Color::White, Color::Yellow, r, Color::Reset)

                }
            }
            SmntcError::IncompatibleLogicOp(op, l, r) => format!("{}The {}'{}'{} operator expects the left and right expressions to be both of type {}{}{} or {}{}{}, but the expressions evaluates to {}{}{} and {}{}{} respectively.", Color::White, Color::Yellow, op, Color::White, Color::Yellow, Type::Bool, Color::White, Color::Yellow, Type::Null, Color::White, Color::Yellow, l, Color::White, Color::Yellow, r, Color::White)
        }
    }

    fn format_runtime_error(&self, error: &RuntimeError, source_vec: &[String]) -> String {
        use RuntimeError::*;

        match error {
            DivisionByZero(line, token_starts, token_ends) => format!("{}Runtime error caused by line {}:\n{}\n{} '{}'\n{} {}\n{} {}Reason: Attempting to divide by zero!{}", Color::White, line, self.blue_pipe(), self.blue_pipe(), source_vec.get(*line -1).unwrap(), self.blue_pipe(), self.print_marker(*token_starts, *token_ends), self.blue_pipe(), Color::Yellow, Color::Reset),
            NotAllowed => format!("{}Runtime error: Operation not allowed.", Color::White)
        }
    }

    fn format_scanner_error(&self, error: &ScannerError, source_vec: &[String]) -> String {
        use ScannerError::*;
        match error {
            InvalidCharacter(line, token_start, token_end) => format!("{}Syntax error in line {} from character {} to {}: \n{}\n{} '{}'\n{}{}\n{} {}Reason: Invalid character{}", 
                Color::White,
                line,
                token_start,
                token_end,
                self.blue_pipe(),
                self.blue_pipe(),
                source_vec.get(*line - 1).unwrap(),
                self.blue_pipe(),
                self.print_marker(*token_start, *token_end),
                self.blue_pipe(),
                Color::Yellow,
                Color::Reset
            ),
            LonelyBangSign(line, token_start, token_end) => format!("{}Syntax error in line {} from character {} to {}: \n{}\n{} '{}'\n{}{}\n{} {}Reason: Invalid syntax. Did you mean '!='?{}", 
                Color::White,
                line,
                token_start,
                token_end,
                self.blue_pipe(),
                self.blue_pipe(),
                source_vec.get(*line - 1).unwrap(),
                self.blue_pipe(),
                self.print_marker(*token_start, *token_end),
                self.blue_pipe(),
                Color::Yellow,
                Color::Reset
            ),
            InvalidToken(line, token_start, token_end, note) => format!(
                "{}Syntax error in line {} from character {} to {}: \n{}\n{} '{}'\n{}{}\n{} {}Reason: {}{}",
                Color::White,
                line,
                token_start,
                token_end,
                self.blue_pipe(),
                self.blue_pipe(),
                source_vec.get(*line - 1).unwrap(),
                self.blue_pipe(),
                self.print_marker(*token_start, *token_end),
                self.blue_pipe(),
                Color::Yellow,
                note,
                Color::Reset
            ),
            UnterminatedString(line) => format!(
                "{}Unterminated String from line {} to the end of file:\n{} \n{}'{}'\n{}\n{} {}Note: Every string must start and finish with a quotation mark, (e.g \"Lorem ipsum\"). Maybe you forgot a '\"'?{}",
                Color::White,
                line,
                self.blue_pipe(),
                self.blue_pipe(),
                source_vec.get(*line - 1).unwrap(),
                self.blue_pipe(),
                self.blue_pipe(),
                Color::Yellow,
                Color::Reset
            ),
        }
    }

    fn format_parser_error(&self, error: &ParserError, source_vec: &[String]) -> String {
        use ParserError::*;
        match error {
            MissingRightParen(line) => format!("{}Syntax error in line {}: \n{}\n{} '{}'\n{}\n{} {}Expected a ')' after this expression{}",
                Color::White,
                line,
                self.blue_pipe(),
                self.blue_pipe(),
                source_vec.get(*line - 1).unwrap(),
                self.blue_pipe(),
                self.blue_pipe(),
                Color::Yellow,
                Color::Reset,
            ),
            MissingExpression(line) => format!(
                "{}Syntax error in line {}:\n{}\n{} '{}'\n{}\n{}{} Reason: Missing an expression{}",
                Color::White,
                line.as_ref().unwrap(),
                self.blue_pipe(),
                self.blue_pipe(),
                source_vec.get(*line.as_ref().unwrap() - 1).unwrap(),
                self.blue_pipe(),
                self.blue_pipe(),
                Color::Yellow,
                Color::Reset
            ),
        }
    }
}
