use std::process::{Command, Output};

const BETHON: &str = env!("CARGO_BIN_EXE_bethon");

// Contains a test that calls pass(f) for each python file f in tests/pass/
include!(concat!(env!("OUT_DIR"), "/pass.rs"));

// Contains a test that calls fail(f) for each python file f in tests/fail/
include!(concat!(env!("OUT_DIR"), "/fail.rs"));

// The tests must succeed
fn pass(file: &str) {
    let output = run(file);
    if !output.status.success() {
        panic!(
            "Expect success but get failure!\n{}",
            merge_out_err(&output)
        );
    }
}

// The tests must fail
fn fail(file: &str) {
    let output = run(file);
    if output.status.success() {
        panic!(
            "Expect failure but get success!\n{}",
            merge_out_err(&output)
        );
    }
}

fn run(file: &str) -> Output {
    Command::new(BETHON).arg(file).output().unwrap()
}

fn merge_out_err(output: &Output) -> String {
    let mut out = String::new();
    out += "=== Stdout ===\n";
    out += &String::from_utf8_lossy(&output.stdout);
    out += "=== Stderr ===\n";
    out += &String::from_utf8_lossy(&output.stderr);
    out
}
