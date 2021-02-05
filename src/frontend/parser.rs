use TokenType::*;

use crate::{
    common::{
        grammar::{expr::Expr, operations::*, stmt::Stmt},
        symbol::{token::Token, token_type::TokenType},
        typings::{value::Value, var_type::VarType},
    },
    error::{parser::ParserError, Error},
};

type ParserResult = std::result::Result<Expr, ParserError>;

pub struct Parser<'a> {
    tokens: &'a [Token],
    current_line: usize,
}

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
            self.current_line = first.placement.line;
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

    fn next_is_type(&mut self) -> Option<(VarType, Token)> {
        if let Some(token) = self.tokens.first() {
            if let Some(t) = match token.tt {
                TokenType::PythonNone => Some(VarType::PythonNone),
                TokenType::Int => Some(VarType::Integer),
                TokenType::Float => Some(VarType::Float),
                TokenType::Str => Some(VarType::Str),
                TokenType::Bool => Some(VarType::Boolean),
                TokenType::Identifier => Some(VarType::Class(token.clone())),
                _ => None,
            } {
                self.advance();
                return Some((t, token.clone()));
            }
        }

        None
    }

    fn consume(&mut self, tt: TokenType) -> Result<Token, ParserError> {
        let current_line = self.current_line;

        if let Some((_, token)) = self.next_is(single(tt.clone())) {
            Ok(token)
        } else {
            Err(ParserError::Expected(tt, current_line))
        }
    }

    fn consume_type(&mut self) -> Result<VarType, ParserError> {
        let current_line = self.current_line;

        if let Some((var_type, _)) = self.next_is_type() {
            Ok(var_type)
        } else {
            Err(ParserError::Expected(TokenType::Identifier, current_line))
        }
    }

    fn sync(&mut self, current_line: usize) {
        while let Some(token) = self.peek() {
            if token.placement.line == current_line + 1 || self.is_at_end() {
                return;
            } else {
                self.advance();
            }
        }
    }

    fn find_deindent(&mut self) {
        while let Some(token) = self.advance() {
            if *token.tt() == TokenType::Deindent {
                return;
            }
        }
    }

    fn primary(&mut self) -> ParserResult {
        if let Some((value, token)) = self.next_is(|tt| match tt {
            False => Some(Value::Bool(false)),
            True => Some(Value::Bool(true)),
            PythonNone => Some(Value::PythonNone),
            Number(number) => Some(Value::Number(number.clone())),
            String(string) => Some(Value::Str(string.to_string())),
            _ => None,
        }) {
            let new_literal = OpWithToken::new(value, token);

            Ok(Expr::Literal(new_literal))
        } else if self.next_is(single(LeftParen)).is_some() {
            let expr = self.expression()?;
            self.consume(RightParen)?;

            Ok(Expr::Grouping(expr.into()))
        } else if let Some((_, token)) = self.next_is(single(Not)) {
            let expr = self.expression()?;

            Ok(Expr::LogicNot((expr.into(), token)))
        } else if let Some((_, token)) = self.next_is(single(Identifier)) {
            Ok(Expr::Variable(token))
        } else if let Some((_, indent_token)) = self.next_is(single(Indent)) {
            self.find_deindent();
            Err(ParserError::UnexpectedIdent(
                indent_token.placement.line,
                indent_token.placement.starts_at,
                indent_token.placement.ends_at,
            ))
        } else if let Some((_, token)) = self.next_is(single(Else)) {
            Err(ParserError::DanglingElse(
                token.placement.line,
                token.placement.starts_at,
                token.placement.ends_at,
            ))
        } else if let Some((_, deindent_token)) = self.next_is(single(Deindent)) {
            Err(ParserError::UnexpectedDeident(
                deindent_token.placement.line,
            ))
        } else {
            Err(ParserError::MissingExpression(self.current_line))
        }
    }

    fn finish_call(&mut self, callee: Expr) -> ParserResult {
        let max_function_args = 255;
        let mut arguments: Vec<Expr> = vec![];

        if self.next_is(single(RightParen)).is_none() {
            arguments.push(self.expression()?); // First argument
            while self.next_is(single(Comma)).is_some() {
                if arguments.len() >= max_function_args {
                    return Err(ParserError::MaxFuntionArgsReached(callee.get_line()));
                }

                arguments.push(self.expression()?);
            }

            self.consume(RightParen)?;
        }

        Ok(Expr::Call(callee.into(), arguments))
    }

    fn call(&mut self) -> ParserResult {
        let mut expr = self.primary()?;

        loop {
            if self.next_is(single(LeftParen)).is_some() {
                expr = self.finish_call(expr)?;
            } else if self.next_is(single(Dot)).is_some() {
                let name = self.consume(Identifier)?;
                expr = Expr::Get(expr.into(), name);
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn unary(&mut self) -> ParserResult {
        if let Some((op, token)) = self.next_is(|tt| match tt {
            Minus => Some(UnaryOp::Minus),
            Plus => Some(UnaryOp::Plus),
            _ => None,
        }) {
            Ok(Expr::Unary(
                OpWithToken::new(op, token),
                self.unary()?.into(),
            ))
        } else {
            self.call()
        }
    }

    fn multiplication(&mut self) -> ParserResult {
        let mut expr = self.unary()?;

        while let Some((op, token)) = self.next_is(|tt| match tt {
            Slash => Some(BinaryOp::Div),
            Star => Some(BinaryOp::Mul),
            Mod => Some(BinaryOp::Mod),
            _ => None,
        }) {
            let right = self.unary()?;

            expr = Expr::BinaryArith(expr.into(), OpWithToken::new(op, token), right.into());
        }

        Ok(expr)
    }

    fn addition(&mut self) -> ParserResult {
        let mut expr = self.multiplication()?;

        while let Some((op, token)) = self.next_is(|tt| match tt {
            Minus => Some(BinaryOp::Sub),
            Plus => Some(BinaryOp::Add),
            _ => None,
        }) {
            let right = self.multiplication()?;

            expr = Expr::BinaryArith(expr.into(), OpWithToken::new(op, token), right.into());
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> ParserResult {
        let mut expr = self.addition()?;

        while let Some((op, token)) = self.next_is(|tt| match tt {
            BangEqual => Some(BinaryCompOp::NotEqual),
            EqualEqual => Some(BinaryCompOp::Equal),
            Greater => Some(BinaryCompOp::GreaterThan),
            GreaterEqual => Some(BinaryCompOp::GreaterEqual),
            Less => Some(BinaryCompOp::LessThan),
            LessEqual => Some(BinaryCompOp::LessEqual),
            _ => None,
        }) {
            let right = self.addition()?;
            expr = Expr::BinaryComp(expr.into(), OpWithToken::new(op, token), right.into());
        }

        Ok(expr)
    }

    fn and(&mut self) -> ParserResult {
        let mut expr = self.comparison()?;

        while let Some((op, token)) = self.next_is(|tt| match tt {
            And => Some(BinaryLogicOp::And),
            _ => None,
        }) {
            let right = self.comparison()?;
            expr = Expr::BinaryLogic(Box::new(expr), OpWithToken::new(op, token), Box::new(right));
        }

        Ok(expr)
    }

    fn or(&mut self) -> ParserResult {
        let mut expr = self.and()?;

        while let Some((op, token)) = self.next_is(|tt| match tt {
            Or => Some(BinaryLogicOp::Or),
            _ => None,
        }) {
            let right = self.and()?;
            expr = Expr::BinaryLogic(Box::new(expr), OpWithToken::new(op, token), Box::new(right));
        }

        Ok(expr)
    }

    fn expression(&mut self) -> ParserResult {
        self.or()
    }

    fn assignment(&mut self) -> Result<Stmt, ParserError> {
        let expr = self.expression()?;

        if let Some((_, _token)) = self.next_is(single(Colon)) {
            if let Some((var_type, token)) = self.next_is_type() {
                self.consume(Equal)?;

                let value = self.expression()?;

                match expr {
                    Expr::Variable(token) => Ok(Stmt::VarStmt(token, Some(var_type), value)),
                    _ => Err(ParserError::ExpectedColon(
                        token.placement.line,
                        token.placement.starts_at - 1,
                    )),
                }
            } else {
                let token = self.advance().unwrap();

                // Happens when the type is not valid
                Err(ParserError::TypeNotDefined(
                    token.placement.line,
                    token.placement.starts_at,
                    token.placement.ends_at,
                ))
            }
        } else if let Some((_, token)) = self.next_is(single(Equal)) {
            let value = self.expression()?;

            match expr {
                Expr::Variable(token) => Ok(Stmt::VarStmt(token, None, value)),
                _ => Err(ParserError::ExpectedColon(
                    token.placement.line,
                    token.placement.starts_at - 1,
                )),
            }
        } else {
            Ok(Stmt::ExprStmt(expr))
        }
    }

    fn expression_statement(&mut self) -> Result<Stmt, ParserError> {
        self.assignment()
    }

    fn assert(&mut self) -> Result<Stmt, ParserError> {
        self.expression().map(Stmt::Assert)
    }

    fn block(&mut self) -> Result<Vec<Stmt>, ParserError> {
        let mut stmts: Vec<Stmt> = vec![];

        self.consume(Colon)?;
        self.consume(Indent)?;
        while self.next_is(single(Deindent)).is_none() && !self.is_at_end() {
            stmts.push(self.statement()?);
        }

        Ok(stmts)
    }

    fn if_statement(&mut self) -> Result<Stmt, ParserError> {
        let condition = self.expression()?;

        let then_branch = self.block()?;

        let mut else_branch: Option<Vec<Stmt>> = None;
        if self.next_is(single(Else)).is_some() {
            else_branch = Some(self.block()?);
        }

        Ok(Stmt::IfStmt(condition, then_branch, else_branch))
    }

    fn function(&mut self) -> Result<Stmt, ParserError> {
        let fun_id = self.consume(Identifier)?;
        self.consume(LeftParen)?;

        let mut params = vec![];
        if self.next_is(single(RightParen)).is_none() {
            get_params(self, params.as_mut())?;
            self.consume(RightParen)?;
        }

        let mut ret_type = VarType::PythonNone;
        if let Some((_, arrow_token)) = self.next_is(single(Arrow)) {
            if let Some((var_type, _)) = self.next_is_type() {
                ret_type = var_type;
            } else if self.next_is(single(Colon)).is_some() {
                let (line, starts_at, ends_at) = arrow_token.placement.as_tuple();
                return Err(ParserError::MissingFunctionReturnType(
                    line, starts_at, ends_at,
                ));
            }
        }

        let fun_body = self.block()?;

        Ok(Stmt::Function(fun_id, params, fun_body, ret_type))
    }

    // todo how to check for empty return?
    fn return_statement(&mut self, ret_token: Token) -> Result<Stmt, ParserError> {
        let expr = self.expression()?;

        Ok(Stmt::ReturnStmt(ret_token, Some(expr)))
    }

    fn from_import_statement(&mut self) -> Result<Stmt, ParserError> {
        let module = self.consume(Identifier)?;
        self.consume(Import)?;

        let mut imports = vec![self.consume(Identifier)?];
        while self.next_is(single(Comma)).is_some() {
            imports.push(self.consume(Identifier)?);
        }

        Ok(Stmt::FromImport(module, imports))
    }

    fn import_statement(&mut self) -> Result<Stmt, ParserError> {
        let module = self.consume(Identifier)?;

        Ok(Stmt::Import(module))
    }

    fn class_statement(&mut self, dataclass_token: Token) -> Result<Stmt, ParserError> {
        self.consume(Class)?;
        let identifier = self.consume(Identifier)?;
        self.consume(Colon)?;

        self.consume(Indent)?;

        let mut attrs = vec![];
        while self.next_is(single(Deindent)).is_none() {
            let attr_name = self.consume(Identifier)?;
            self.consume(Colon)?;
            let attr_type = self.consume_type()?;

            attrs.push((attr_name, attr_type))
        }

        Ok(Stmt::Class(dataclass_token, identifier, attrs))
    }

    fn statement(&mut self) -> Result<Stmt, ParserError> {
        if self.consume(Assert).is_ok() {
            self.assert()
        } else if self.consume(If).is_ok() {
            self.if_statement()
        } else if self.consume(Def).is_ok() {
            self.function()
        } else if let Ok(ret_token) = self.consume(Return) {
            self.return_statement(ret_token)
        } else if self.consume(From).is_ok() {
            self.from_import_statement()
        } else if self.consume(Import).is_ok() {
            self.import_statement()
        } else if let Ok(dataclass_token) = self.consume(Decorator) {
            self.class_statement(dataclass_token)
        } else if let Ok(class_token) = self.consume(Class) {
            let (line, starts_at, ends_at) = class_token.placement.as_tuple();
            Err(ParserError::RegularClass(line, starts_at, ends_at))
        } else {
            self.expression_statement()
        }
    }

    pub fn parse(&mut self) -> Result<Vec<Stmt>, Vec<Error>> {
        let mut statements: Vec<Stmt> = vec![];
        let mut errors: Vec<Error> = vec![];

        while !self.is_at_end() {
            match self.statement() {
                Ok(statement) => statements.push(statement),
                Err(error) => {
                    errors.push(Error::Parser(error));
                    self.sync(self.current_line);
                }
            }
        }

        if errors.is_empty() {
            Ok(statements)
        } else {
            Err(errors)
        }
    }
}

fn single(tt: TokenType) -> impl Fn(&TokenType) -> Option<()> {
    use std::mem::discriminant;
    move |other| {
        if discriminant(&tt) == discriminant(other) {
            Some(())
        } else {
            None
        }
    }
}

fn get_params(parser: &mut Parser, params: &mut Vec<(Token, VarType)>) -> Result<(), ParserError> {
    let first_param_id = parser.consume(Identifier)?;
    let colon_token = parser.consume(Colon)?;
    if let Ok(first_param_type) = parser.consume_type() {
        params.push((first_param_id, first_param_type));
    } else {
        let (line, starts_at, ends_at) = colon_token.placement.as_tuple();
        return Err(ParserError::MissingParameterType(line, starts_at, ends_at));
    }

    while parser.next_is(single(Comma)).is_some() {
        let param_id = parser.consume(Identifier)?;
        let colon_token = parser.consume(Colon)?;
        if let Ok(param_type) = parser.consume_type() {
            params.push((param_id, param_type));
        } else {
            let (line, starts_at, ends_at) = colon_token.placement.as_tuple();
            return Err(ParserError::MissingParameterType(line, starts_at, ends_at));
        }
    }

    Ok(())
}
