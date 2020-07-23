mod error;
mod token;

use error::Error;

fn run(source_code: String) {
    println!("{}", source_code);
}

fn main() {
    use std::process::exit;
    let mut argv = std::env::args();
    let argv_len = argv.len();

    if argv_len == 2 {
        if let Some(path) = argv.nth(1) {
            if let Ok(source_code) = std::fs::read_to_string(path) {
                run(source_code);
            } else {
                let error = Error::Input(
                    "Something went wrong while reading the file".to_string(),
                    "Verify the path provided and try again".to_string(),
                );
                error.show_error(None);
                exit(64);
            }
        } else {
            let error = Error::Input(
                "Internal error reading the file".to_string(),
                "did you use 'rlox [filename]'?".to_string(),
            );
            error.show_error(None);
            exit(64);
        }
    } else if argv_len > 2 {
        let error = Error::Input(
            "Too many arguments!".to_string(),
            "Usage: rlox [path to script]".to_string(),
        );
        error.show_error(None);
        exit(65);
    } else if argv_len < 2 {
        let error = Error::Input(
            "Too few arguments!".to_string(),
            "Usage: rlox [path to script]".to_string(),
        );
        error.show_error(None);
        exit(65);
    }
}
