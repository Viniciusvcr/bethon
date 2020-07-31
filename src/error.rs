use crate::token::TokenType;

pub enum ScannerError {
    // Line, line_start, line_char, reason
    InvalidToken(usize, usize, usize, String),
    // Line, token_start
    UnterminatedString(usize),
}

#[allow(dead_code)]
pub enum ParserError {
    // MissingToken, Note
    Missing(TokenType, Option<String>),
    // Note
    MissingExpression(Option<String>),
    // Expected, Found, Note
    MismatchedTypes(String, String, Option<String>), // REFACTOR change Expected and Found to enums?
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
        }
    }

    fn format_scanner_error(&self, error: &ScannerError, source_vec: &[String]) -> String {
        use ScannerError::*;
        match error {
            InvalidToken(line, token_start, token_end, note) => format!(
                "{}Syntax error in line {} from column {} to {}: \n{}\n{} '{}'\n{}{}\n{} {}Reason: {}{}",
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
}
