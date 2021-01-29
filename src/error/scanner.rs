use super::{print_marker, static_error_template};

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

impl ScannerError {
    pub fn format(&self, source_vec: &[String]) -> String {
        let error_type = "Syntax";

        match self {
            ScannerError::InvalidCharacter(line, starts_at, ends_at) => static_error_template(
                error_type,
                source_vec,
                *line,
                Some(*starts_at),
                Some(*ends_at),
                "Invalid character".to_string(),
                Some(print_marker(*starts_at, *ends_at, Some("here"))),
            ),
            ScannerError::LonelyBangSign(line, starts_at, ends_at) => static_error_template(
                error_type,
                source_vec,
                *line,
                Some(*starts_at),
                Some(*ends_at),
                "Invalid syntax. Did you mean 'not'?".to_string(),
                Some(print_marker(*starts_at, *ends_at, Some("here"))),
            ),
            ScannerError::InvalidToken(line, starts_at, ends_at, reason) => static_error_template(
                error_type,
                source_vec,
                *line,
                Some(*starts_at),
                Some(*ends_at),
                reason.to_string(),
                Some(print_marker(*starts_at, *ends_at, Some("here"))),
            ),
            ScannerError::MismatchedIdent(line, starts_at, ends_at) => static_error_template(
                error_type,
                source_vec,
                *line,
                Some(*starts_at),
                Some(*ends_at),
                "There is a conflicted usage of spaces and tabs indentation in this file"
                    .to_string(),
                Some(print_marker(
                    *starts_at,
                    *ends_at,
                    Some("first conflict happens here"),
                )),
            ),
            ScannerError::UnterminatedString(line) => static_error_template(
                error_type,
                source_vec,
                *line,
                None,
                None,
                "Unterminated String or Block Comment. Maybe you forgot a '\"'?".to_string(),
                None,
            ),
        }
    }
}
