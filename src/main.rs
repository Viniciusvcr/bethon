use bethon::error::Error;
use bethon::interpreter::Interpreter;
use bethon::parser::Parser;
use bethon::scanner::{create_code_vec, Scanner};
use bethon::smntc_analyzer::SemanticAnalyzer;

use std::{cmp::Ordering, env, process::exit};

#[allow(unused_variables)]
fn run(filename: &str, source_code: &str) {
    let mut lexer = Scanner::new(source_code);

    match lexer.scan_tokens() {
        Ok(vec) => {
            println!("{:#?}", vec);
            let mut parser = Parser::new(vec);

            match parser.parse() {
                Ok(stmts) => {
                    let mut pass = SemanticAnalyzer::new();

                    if let Err(errors) = pass.analyze(&stmts) {
                        let code_vec = create_code_vec(source_code);
                        for error in errors {
                            error.show_error(Some(filename), Some(&code_vec));
                        }
                        exit(1);
                    } else {
                        let mut interpreter = Interpreter::default();

                        if let Some(error) = interpreter.interpret(&stmts) {
                            error.show_error(Some(filename), Some(&create_code_vec(source_code)));
                            exit(1);
                        }
                    }
                }
                Err(errors) => {
                    let code_vec = create_code_vec(source_code);
                    for error in errors {
                        error.show_error(Some(filename), Some(&code_vec));
                    }
                    exit(1);
                }
            }
        }
        Err(err) => {
            let code_vec = create_code_vec(source_code);
            err.show_error(Some(filename), Some(&code_vec));
            exit(1);
        }
    }
}

fn main() {
    let mut argv = env::args();
    let argv_len = argv.len();

    match argv_len.cmp(&2) {
        Ordering::Equal => {
            if let Some(path) = argv.nth(1) {
                match std::fs::read_to_string(&path) {
                    Ok(source_code) => {
                        run(&path, &source_code);
                    }
                    Err(_) => {
                        let error = Error::Input(
                            format!("Cannot find file '{}'", path),
                            "Make sure you provided the right path to the program.".to_string(),
                        );

                        error.show_error(None, None);
                        exit(64);
                    }
                }
            } else {
                let error = Error::Input(
                    "Internal error reading the file".to_string(),
                    "did you use 'bethon [filename]'?".to_string(),
                );
                error.show_error(None, None);
                exit(64);
            }
        }
        Ordering::Greater => {
            let error = Error::Input(
                "Too many arguments!".to_string(),
                "Usage: bethon [path to script]".to_string(),
            );
            error.show_error(None, None);
            exit(65);
        }
        Ordering::Less => {
            let error = Error::Input(
                "Too few arguments!".to_string(),
                "Usage: bethon [path to script]".to_string(),
            );
            error.show_error(None, None);
            exit(65);
        }
    }
}
