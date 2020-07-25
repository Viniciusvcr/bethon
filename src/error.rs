pub enum ScannerError {
    // Line, line_start, line_char, reason
    InvalidToken(usize, usize, usize, String),
    UnterminatedString(usize),
}

pub enum Error {
    UnexpectedFail,
    Input(String, String),
    Scanner(ScannerError),
}

impl Error {
    // TODO Colors in errors
    pub fn show_error(&self, file: Option<&str>, source_vec: Option<&[&str]>) {
        if let Some(filename) = file {
            eprintln!("Bethon Error in file --> {}:", filename)
        } else {
            eprintln!("Bethon Error:")
        }
        eprintln!("| {}", self.format_error(source_vec));
    }

    fn format_error(&self, source_vec: Option<&[&str]>) -> String {
        use Error::*;

        match self {
            Input(reason, note) => format!("Input error: {}\n| Note: {}", reason, note),
            UnexpectedFail => {
                format!("Unexpected fail: Something went wrong while parsing the file")
            }
            Scanner(scanner_error) => self.format_scanner_error(scanner_error, source_vec.unwrap()),
        }
    }

    fn format_scanner_error(&self, error: &ScannerError, source_vec: &[&str]) -> String {
        use ScannerError::*;
        match error {
            InvalidToken(line, line_start, line_end, note) => format!(
                "Syntax error in line {} from column {} to {}: \n| '{}'\n| Reason: {}",
                line,
                line_start,
                line_end,
                source_vec.get(*line - 1).unwrap(),
                note,
            ),
            UnterminatedString(line) => format!(
                "Unterminated String error from line {} to the end of file:\n| '{}'\n| Note: Maybe you forgot a '\"'?",
                line,
                source_vec.get(*line - 1).unwrap()
            ),
        }
    }
}
