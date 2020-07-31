use bethon::create_code_vec;
use bethon::scanner::Scanner;

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

#[test]
fn empty_source_code() {
    let empty = "";
    let mut scanner = Scanner::new(empty);

    match scanner.scan_tokens() {
        Ok(vec) => assert_eq!(vec.len(), 1), // EOF
        Err(_) => assert!(false),
    }
}

#[test]
fn hello_world() {
    let string = "print(\"Hello, world!\")";
    let mut scanner = Scanner::new(string);

    match scanner.scan_tokens() {
        Ok(vec) => assert_eq!(vec.len(), 5), // print, (, "Hello, World", ), EOF
        Err(_) => assert!(false),
    }
}

#[test]
fn unterminated_string() {
    let string = "\"Hello, world!";
    let mut scanner = Scanner::new(string);

    assert_eq!(scanner.scan_tokens().is_err(), true)
}

#[test]
fn accentuation() {
    let string = "print(\"Olá, mundo!\")";
    let mut scanner = Scanner::new(string);

    assert_eq!(scanner.scan_tokens().is_ok(), true)
}

#[test]
fn number_finishing_with_dot() {
    let string = "123.";

    let mut scanner = Scanner::new(string);
    assert_eq!(scanner.scan_tokens().is_err(), true)
}

#[test]
fn number_with_two_dots() {
    let string = "123.2.2";

    let mut scanner = Scanner::new(string);
    assert_eq!(scanner.scan_tokens().is_err(), true)
}

#[test]
fn float_ok() {
    let string = "123.3";

    let mut scanner = Scanner::new(string);
    assert_eq!(scanner.scan_tokens().is_ok(), true)
}

#[test]
fn integer_ok() {
    let string = "123";

    let mut scanner = Scanner::new(string);
    assert_eq!(scanner.scan_tokens().is_ok(), true)
}

#[test]
fn create_code_vec_hello_world() {
    let hello_word = "print(\"Hello, world!\")";

    assert_eq!(create_code_vec(hello_word), ["print(\"Hello, world!\")"]);
}
