use bethon::{
    common::symbol::{token::Token, token_type::TokenType},
    frontend::parser::Parser,
};

#[test]
fn empty_code() {
    let tokens = [Token::new(TokenType::Eof, "EOF".to_string(), 0, 0, 0)];
    let mut parser = Parser::new(&tokens);

    assert_eq!(parser.parse().is_ok(), true)
}
