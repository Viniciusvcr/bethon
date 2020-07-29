use bethon::error::Error;
use bethon::scanner::{create_code_vec, Scanner};

#[allow(unused_variables)]
fn run(filename: &str, source_code: &str) {
    let code_vec = create_code_vec(source_code);
    let mut lexer = Scanner::new(source_code);

    match lexer.scan_tokens() {
        Ok(vec) => println!("{:?}", vec),
        Err(err) => err.show_error(Some(filename), Some(&code_vec)),
    }
}

fn main() {
    use std::process::exit;
    let mut argv = std::env::args();
    let argv_len = argv.len();

    if argv_len == 2 {
        if let Some(path) = argv.nth(1) {
            if let Ok(source_code) = std::fs::read_to_string(&path) {
                run(&path, &source_code);
            }
        } else {
            let error = Error::Input(
                "Internal error reading the file".to_string(),
                "did you use 'bethon [filename]'?".to_string(),
            );
            error.show_error(None, None);
            exit(64);
        }
    } else if argv_len > 2 {
        let error = Error::Input(
            "Too many arguments!".to_string(),
            "Usage: bethon [path to script]".to_string(),
        );
        error.show_error(None, None);
        exit(65);
    } else if argv_len < 2 {
        let error = Error::Input(
            "Too few arguments!".to_string(),
            "Usage: bethon [path to script]".to_string(),
        );
        error.show_error(None, None);
        exit(65);
    }
}

#[test]
fn create_code_vec_hello_world() {
    let hello_word = "print(\"Hello, world!\")";

    assert_eq!(create_code_vec(hello_word), ["print(\"Hello, world!\")"]);
}
