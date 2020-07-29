mod error;
mod scanner;
mod token;

use error::Error;
use scanner::Scanner;

fn create_code_vec(source_code: &str) -> std::vec::Vec<String> {
    let source_code_len = source_code.chars().count();

    let mut line: String = "".to_string();
    if source_code_len > 0 {
        let mut vec_lines: std::vec::Vec<String> = vec![];
        for c in source_code.chars() {
            if c == '\n' {
                vec_lines.push(line.clone());
                line = "".to_string();
            } else {
                line.push(c);
            }
        }
        if line != "" {
            vec_lines.push(line.clone());
        }
        vec_lines
    } else {
        vec![]
    }
}

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

#[test]
fn create_code_vec_three_lines() {
    let three_lines = "print(\"Hello, world!\")\n2 + 2\n2";

    assert_eq!(
        create_code_vec(three_lines),
        ["print(\"Hello, world!\")", "2 + 2", "2"]
    );
}

#[test]
fn create_code_vec_three_lines_with_empty_line() {
    let three_lines = "print(\"Hello, world!\")\n2 + 2\n2\n\n";

    assert_eq!(
        create_code_vec(three_lines),
        ["print(\"Hello, world!\")", "2 + 2", "2", ""]
    );
}

#[test]
fn create_code_vec_three_lines_with_endline() {
    let three_lines = "print(\"Hello, world!\")\n2 + 2\n2\n";

    assert_eq!(
        create_code_vec(three_lines),
        ["print(\"Hello, world!\")", "2 + 2", "2"]
    );
}

#[test]
fn create_code_vec_empty() {
    let empty_vec: std::vec::Vec<&str> = vec![];
    assert_eq!(create_code_vec(""), empty_vec);
}
