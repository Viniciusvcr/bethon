pub mod environment;
pub mod error;
pub mod expr;
pub mod interpreter;
pub mod parser;
pub mod scanner;
pub mod smntc_analyzer;
pub mod stmt;
pub mod token;

pub use scanner::create_code_vec;
