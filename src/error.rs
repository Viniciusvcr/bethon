pub enum Error {
    Input(String, String),
}

impl Error {
    // TODO Colors in errors
    pub fn show_error(&self, file: Option<&str>) {
        if let Some(filename) = file {
            eprintln!("Bethon Error in file --> {}:", filename)
        } else {
            eprintln!("Bethon Error:")
        }
        eprintln!("| {}", self.format_error());
    }

    fn format_error(&self) -> String {
        use Error::*;

        match self {
            Input(reason, note) => format!("Input error: {}\n| Note: {}", reason, note),
        }
    }
}
