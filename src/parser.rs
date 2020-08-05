use crate::error::{Error, ParserError};
use crate::expr::*;
use crate::stmt::Stmt;
use crate::token::{Token, TokenType};

type ParserResult = std::result::Result<Expr, ParserError>;

pub struct Parser<'a> {
    tokens: &'a [Token],
    current_line: usize,
}

use TokenType::*;
impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Token]) -> Self {
        Self {
            tokens,
            current_line: 1,
        }
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.first()
    }

    fn is_at_end(&self) -> bool {
        if let Some(token) = self.peek() {
            *token.tt() == TokenType::Eof
        } else {
            true
        }
    }

    fn advance(&mut self) -> Option<&Token> {
        if let Some((first, rest)) = self.tokens.split_first() {
            self.tokens = rest;
            self.current_line = first.placement().line;
            Some(first)
        } else {
            None
        }
    }

    fn next_is<T>(&mut self, fun: impl Fn(&TokenType) -> Option<T>) -> Option<(T, Token)> {
        if let Some(token) = self.tokens.first() {
            if let Some(t) = fun(token.tt()) {
                self.advance();
                return Some((t, token.clone()));
            }
        }
        None
    }

    fn check_type(&mut self, tt: TokenType) -> bool {
        if let Some(token) = self.peek() {
            *token.tt() == tt
        } else {
            false
        }
    }

    fn consume(&mut self, tt: TokenType) -> Option<&Token> {
        if self.check_type(tt) {
            self.advance()
        } else {
            None
        }
    }

    fn ignore_spaces(&mut self) {
        while let Some(_) = self.next_is(|tt| match tt {
            TokenType::Space => Some(()),
            _ => None,
        }) {}
    }

    fn primary(&mut self) -> ParserResult {
        if let Some(value_and_token) = self.next_is(|tt| match tt {
            False => Some(Value::Bool(false)),
            True => Some(Value::Bool(true)),
            PythonNone => Some(Value::PythonNone),
            Number(number) => Some(Value::Number(*number)),
            String(string) => Some(Value::Str(string.to_string())),
            _ => None,
        }) {
            Ok(Expr::Literal(value_and_token))
        } else if let Some((_, token)) = self.next_is(|tt| match tt {
            LeftParen => Some(LeftParen),
            _ => None,
        }) {
            let expr = self.expression()?;

            match self.consume(RightParen) {
                Some(_) => Ok(Expr::Grouping(expr.into())),
                None => Err(ParserError::Missing(
                    token.placement().line,
                    Some("Expected a ')' after this expression.".to_string()),
                )),
            }
        } else {
            Err(ParserError::MissingExpression(Some(self.current_line)))
        }
    }

    fn unary(&mut self) -> ParserResult {
        self.ignore_spaces();

        if let Some(op_and_token) = self.next_is(|tt| match tt {
            Minus => Some(UnaryOp::Minus),
            Plus => Some(UnaryOp::Plus),
            _ => None,
        }) {
            Ok(Expr::Unary(op_and_token, self.unary()?.into()))
        } else {
            self.primary()
        }
    }

    fn multiplication(&mut self) -> ParserResult {
        let mut expr = self.unary()?;

        self.ignore_spaces();

        while let Some(op_and_token) = self.next_is(|tt| match tt {
            Slash => Some(BinaryOp::Div),
            Star => Some(BinaryOp::Mul),
            Mod => Some(BinaryOp::Mod),
            _ => None,
        }) {
            let right = self.unary()?;

            expr = Expr::Binary(expr.into(), op_and_token, right.into());
        }

        Ok(expr)
    }

    fn addition(&mut self) -> ParserResult {
        let mut expr = self.multiplication()?;

        self.ignore_spaces();

        while let Some(op_and_token) = self.next_is(|tt| match tt {
            Minus => Some(BinaryOp::Sub),
            Plus => Some(BinaryOp::Add),
            _ => None,
        }) {
            let right = self.multiplication()?;

            expr = Expr::Binary(expr.into(), op_and_token, right.into());
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> ParserResult {
        let mut expr = self.addition()?;

        self.ignore_spaces();

        while let Some(op_and_token) = self.next_is(|tt| match tt {
            Greater => Some(BinaryOp::GreaterThan),
            GreaterEqual => Some(BinaryOp::GreaterEqual),
            Less => Some(BinaryOp::LessThan),
            LessEqual => Some(BinaryOp::LessEqual),
            _ => None,
        }) {
            let right = self.addition()?;
            expr = Expr::Binary(expr.into(), op_and_token, right.into());
        }

        Ok(expr)
    }

    fn equality(&mut self) -> ParserResult {
        let mut expr = self.comparison()?;

        self.ignore_spaces();

        while let Some(op_and_token) = self.next_is(|tt| match tt {
            EqualEqual => Some(BinaryOp::Equal),
            _ => None,
        }) {
            let right = self.comparison()?;
            expr = Expr::Binary(Box::new(expr), op_and_token, Box::new(right));
        }

        Ok(expr)
    }

    fn expression(&mut self) -> ParserResult {
        self.equality()
    }

    fn expression_statement(&mut self) -> Result<Stmt, ParserError> {
        match self.expression() {
            Ok(value) => Ok(Stmt::ExprStmt(value)),
            Err(error) => Err(error),
        }
    }

    fn statement(&mut self) -> Result<Stmt, ParserError> {
        self.expression_statement()
    }

    pub fn parse(&mut self) -> Result<Vec<Stmt>, Vec<Error>> {
        let mut statements: Vec<Stmt> = vec![];
        let mut errors: Vec<Error> = vec![];

        while !self.is_at_end() {
            match self.statement() {
                Ok(statement) => statements.push(statement),
                Err(error) => errors.push(Error::Parser(error)),
            }
        }

        if errors.is_empty() {
            Ok(statements)
        } else {
            Err(errors)
        }
    }
}
