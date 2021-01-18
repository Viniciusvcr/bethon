pub mod parser;
pub mod runtime;
pub mod scanner;
pub mod semantic;

use parser::ParserError;
use runtime::RuntimeError;
use scanner::ScannerError;
use semantic::SmntcError;

use std::usize;

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

fn blue_pipe() -> String {
    format!("{}|{}", Color::Blue, Color::Reset)
}

fn print_marker(start: usize, end: usize, message: Option<&str>) -> String {
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
            blue_pipe(),
            blue_pipe(),
            source_vec.get(line - 1).unwrap(),
            blue_pipe(),
            marker.unwrap(),
            blue_pipe(),
            blue_pipe(),
            Color::Yellow,
            reason,
            Color::Reset
        )
    } else {
        format!(
            "{}{} \n{}\n{} '{}'\n{}\n{}{} Reason: {}{}",
            Color::White,
            create_title(error_type, line, starts_at, ends_at),
            blue_pipe(),
            blue_pipe(),
            source_vec.get(line - 1).unwrap(),
            blue_pipe(),
            blue_pipe(),
            Color::Yellow,
            reason,
            Color::Reset
        )
    }
}

impl Error {
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
        eprintln!("{} {}", blue_pipe(), self.format_error(source_vec));
    }

    fn format_error(&self, source_vec: Option<&[String]>) -> String {
        match self {
            Error::Input(reason, note) => format!(
                "{}Input error: {}\n{}\n{} {}",
                Color::White,
                reason,
                blue_pipe(),
                blue_pipe(),
                note
            ),
            Error::UnexpectedFail => format!(
                "{}Unexpected fail: Something went wrong while parsing the file{}",
                Color::White,
                Color::Reset
            ),
            Error::Scanner(scanner_error) => scanner_error.format(source_vec.unwrap()),
            Error::Parser(parser_error) => parser_error.format(source_vec.unwrap()),
            Error::Smntc(smntc_error) => smntc_error.format(source_vec.unwrap()),
            Error::Runtime(runtime_error) => runtime_error.format(source_vec.unwrap()),
        }
    }
}
