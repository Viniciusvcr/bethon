use bethon::parser::Parser;
use bethon::token::{Token, TokenType};

#[test]
fn empty_code() {
    let tokens = [Token::new(TokenType::Eof, 0, 0, 0)];
    let mut parser = Parser::new(&tokens);

    assert_eq!(parser.parse().is_ok(), true)
}
