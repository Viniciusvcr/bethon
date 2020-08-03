use crate::error::{Error, ParserError};
use crate::expr::*;
use crate::stmt::Stmt;
use crate::token::{Token, TokenType};

type ParserResult = std::result::Result<Expr, ParserError>;

pub struct Parser<'a> {
    tokens: &'a [Token],
    current: usize,
}

use TokenType::*;
impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Token]) -> Self {
        Self { tokens, current: 0 }
    }

    fn peek(&self) -> &Token {
        &self.tokens[0]
    }

    fn is_at_end(&self) -> bool {
        *self.peek().tt() == TokenType::Eof
    }

    fn advance(&mut self) -> &Token {
        self.tokens = &self.tokens[1..];

        &self.tokens[0]
    }

    fn next_is<T>(&mut self, fun: impl Fn(&TokenType) -> Option<T>) -> Option<(T, Token)> {
        if let Some(token) = self.tokens.get(self.current) {
            if let Some(t) = fun(token.tt()) {
                self.advance();
                return Some((t, token.clone()));
            }
        }
        None
    }

    fn check_type(&self, tt: TokenType) -> bool {
        if self.is_at_end() {
            false
        } else {
            *self.peek().tt() == tt
        }
    }

    fn consume(
        &mut self,
        tt: TokenType,
        error_message: std::string::String,
    ) -> Result<&Token, ParserError> {
        if self.check_type(tt) {
            Ok(self.advance())
        } else {
            let token = self.peek();
            Err(ParserError::Missing(
                token.placement().line,
                Some(error_message.to_string()),
            ))
        }
    }

    fn ignore_spaces(&mut self) {
        while *self.peek().tt() == Space {
            self.advance();
        }
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
        } else if let Some(_) = self.next_is(|tt| match tt {
            LeftParen => Some(LeftParen),
            _ => None,
        }) {
            let expr = self.expression()?;

            match self.consume(RightParen, "Expect ')' after this expression".to_string()) {
                Ok(_) => Ok(Expr::Grouping(expr.into())),
                Err(error) => Err(error),
            }
        } else {
            Err(ParserError::MissingExpression(Some(
                self.peek().placement().line,
            )))
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
