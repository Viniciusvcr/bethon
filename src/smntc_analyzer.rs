use crate::{
    environment::{Environment, SemanticEnvironment},
    error::{Error, SmntcError},
    expr::{
        operations::{BinaryCompOp, BinaryLogicOp, BinaryOp, UnaryOp},
        value::Value,
        Expr,
    },
    stmt::Stmt,
    token::{number_type::NumberType, Token, VarType},
};

use num_bigint::BigInt;
use num_traits::cast::ToPrimitive;
use std::{collections::HashMap, f64};

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Integer(BigInt),
    Float(f64),
    Boolean(bool),
    Null,
    Str(String),
    Fun(Vec<VarType>, VarType),
}

impl Default for Type {
    fn default() -> Self {
        Type::Null
    }
}

impl std::convert::From<&VarType> for Type {
    fn from(var_type: &VarType) -> Self {
        match var_type {
            VarType::Boolean => Type::Boolean(false),
            VarType::Integer => Type::Integer(num_bigint::BigInt::from(0)),
            VarType::Float => Type::Float(0.0),
            VarType::Str => Type::Str("".to_string()),
            VarType::PythonNone => Type::Null,
            VarType::Function => Type::Fun(Vec::default(), VarType::Integer),
        }
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Null => write!(f, "None"),
            Type::Integer(_) => write!(f, "int"),
            Type::Float(_) => write!(f, "float"),
            Type::Boolean(_) => write!(f, "bool"),
            Type::Str(_) => write!(f, "str"),
            Type::Fun(_, ret) => write!(f, "<function> -> {}", ret),
        }
    }
}

// The semantic analyzer
pub struct SemanticAnalyzer<'a> {
    types: HashMap<&'a Expr, Type>,
    symbol_table: SemanticEnvironment,
    errors: Vec<Error>,
}

impl<'a> Default for SemanticAnalyzer<'a> {
    fn default() -> Self {
        Self::new()
    }
}

type SemanticAnalyzerResult = Result<Type, SmntcError>;

impl<'a> SemanticAnalyzer<'a> {
    fn with_new_env<T>(&mut self, fun: impl Fn(&mut Self) -> T) -> T {
        self.symbol_table.push();
        let result = fun(self);
        self.symbol_table.pop();

        result
    }

    fn insert(&mut self, expr: &'a Expr, t: Type) {
        self.types.insert(expr, t);
    }

    fn insert_var(&mut self, id: &'a str, t: Type) -> Option<()> {
        self.symbol_table.define(id.to_string(), t).and(Some(()))
    }

    fn get_var(&mut self, id: &str) -> Option<Type> {
        self.symbol_table.get(id)
    }

    fn analyze_bin_arith(&mut self, op: &BinaryOp, a: &Expr, b: &Expr) -> SemanticAnalyzerResult {
        use BinaryOp::*;

        let type_a = self.analyze_one(a)?;
        let type_b = self.analyze_one(b)?;

        let expr_type = match (op, type_a, type_b) {
            (Add, Type::Integer(a), Type::Integer(b)) => Type::Integer(a + b),
            (Add, Type::Integer(a), Type::Float(b)) => Type::Float(a.to_f64().unwrap() + b),
            (Add, Type::Float(a), Type::Integer(b)) => Type::Float(a + b.to_f64().unwrap()),
            (Add, Type::Float(a), Type::Float(b)) => Type::Float(a + b),
            (Add, Type::Str(a), Type::Str(b)) => Type::Str(format!("{}{}", a, b)),
            (Sub, Type::Integer(a), Type::Integer(b)) => Type::Integer(a - b),
            (Sub, Type::Integer(a), Type::Float(b)) => Type::Float(a.to_f64().unwrap() - b),
            (Sub, Type::Float(a), Type::Integer(b)) => Type::Float(a - b.to_f64().unwrap()),
            (Sub, Type::Float(a), Type::Float(b)) => Type::Float(a - b),
            (Mul, Type::Integer(a), Type::Integer(b)) => Type::Integer(a * b),
            (Mul, Type::Integer(a), Type::Float(b)) => Type::Float(a.to_f64().unwrap() * b),
            (Mul, Type::Float(a), Type::Integer(b)) => Type::Float(a * b.to_f64().unwrap()),
            (Mul, Type::Float(a), Type::Float(b)) => Type::Float(a * b),
            (Div, Type::Integer(a), Type::Integer(b)) => Type::Integer(a / b),
            (Div, Type::Integer(a), Type::Float(b)) => Type::Float(a.to_f64().unwrap() / b),
            (Div, Type::Float(a), Type::Integer(b)) => Type::Float(a / b.to_f64().unwrap()),
            (Div, Type::Float(a), Type::Float(b)) => Type::Float(a / b),
            (Mod, Type::Integer(a), Type::Integer(b)) => Type::Integer(a % b),
            (Mod, Type::Integer(a), Type::Float(b)) => Type::Float(a.to_f64().unwrap() % b),
            (Mod, Type::Float(a), Type::Integer(b)) => Type::Float(a % b.to_f64().unwrap()),
            (Mod, Type::Float(a), Type::Float(b)) => Type::Float(a % b),
            (op, l, r) => return Err(SmntcError::IncompatibleBinArith(*op, l, r)),
        };

        Ok(expr_type)
    }

    fn analyze_bin_comp(
        &mut self,
        op: &BinaryCompOp,
        a: &Expr,
        b: &Expr,
    ) -> SemanticAnalyzerResult {
        use BinaryCompOp::*;

        let type_a = self.analyze_one(a)?;
        let type_b = self.analyze_one(b)?;

        let error_margin = f64::EPSILON;

        let expr_type = match (op, type_a, type_b) {
            (NotEqual, Type::Integer(a), Type::Integer(b)) => Type::Boolean(a != b),
            (NotEqual, Type::Integer(a), Type::Float(b)) => {
                Type::Boolean((a.to_f64().unwrap() - b).abs() > error_margin)
            }
            (NotEqual, Type::Float(a), Type::Integer(b)) => {
                Type::Boolean((a - b.to_f64().unwrap()).abs() > error_margin)
            }
            (NotEqual, Type::Float(a), Type::Float(b)) => {
                Type::Boolean((a - b).abs() > error_margin)
            }
            (NotEqual, Type::Boolean(a), Type::Boolean(b)) => Type::Boolean(a != b),
            (NotEqual, Type::Str(a), Type::Str(b)) => Type::Boolean(a.ne(&b)),
            (NotEqual, Type::Null, Type::Null) => Type::Boolean(false),
            (NotEqual, _, Type::Null) => Type::Boolean(true),
            (NotEqual, Type::Null, _) => Type::Boolean(true),
            (Equal, Type::Integer(a), Type::Integer(b)) => Type::Boolean(a == b),
            (Equal, Type::Integer(a), Type::Float(b)) => {
                Type::Boolean((a.to_f64().unwrap() - b).abs() < error_margin)
            }
            (Equal, Type::Float(a), Type::Integer(b)) => {
                Type::Boolean((a - b.to_f64().unwrap()).abs() < error_margin)
            }
            (Equal, Type::Float(a), Type::Float(b)) => Type::Boolean((a - b).abs() < error_margin),
            (Equal, Type::Boolean(a), Type::Boolean(b)) => Type::Boolean(a == b),
            (Equal, Type::Str(a), Type::Str(b)) => Type::Boolean(a.eq(&b)),
            (Equal, Type::Null, Type::Null) => Type::Boolean(true),
            (Equal, _, Type::Null) => Type::Boolean(false),
            (Equal, Type::Null, _) => Type::Boolean(false),
            (LessThan, Type::Integer(a), Type::Integer(b)) => Type::Boolean(a < b),
            (LessThan, Type::Integer(a), Type::Float(b)) => Type::Boolean(a.to_f64().unwrap() < b),
            (LessThan, Type::Float(a), Type::Integer(b)) => Type::Boolean(a < b.to_f64().unwrap()),
            (LessThan, Type::Float(a), Type::Float(b)) => Type::Boolean(a < b),
            (LessThan, Type::Str(a), Type::Str(b)) => Type::Boolean(a.lt(&b)),
            (LessEqual, Type::Integer(a), Type::Integer(b)) => Type::Boolean(a <= b),
            (LessEqual, Type::Integer(a), Type::Float(b)) => {
                Type::Boolean(a.to_f64().unwrap() <= b)
            }
            (LessEqual, Type::Float(a), Type::Integer(b)) => {
                Type::Boolean(a <= b.to_f64().unwrap())
            }
            (LessEqual, Type::Float(a), Type::Float(b)) => Type::Boolean(a <= b),
            (LessEqual, Type::Str(a), Type::Str(b)) => Type::Boolean(a.le(&b)),
            (GreaterThan, Type::Integer(a), Type::Integer(b)) => Type::Boolean(a > b),
            (GreaterThan, Type::Integer(a), Type::Float(b)) => {
                Type::Boolean(a.to_f64().unwrap() > b)
            }
            (GreaterThan, Type::Float(a), Type::Integer(b)) => {
                Type::Boolean(a > b.to_f64().unwrap())
            }
            (GreaterThan, Type::Float(a), Type::Float(b)) => Type::Boolean(a > b),
            (GreaterThan, Type::Str(a), Type::Str(b)) => Type::Boolean(a.gt(&b)),
            (GreaterEqual, Type::Integer(a), Type::Integer(b)) => Type::Boolean(a >= b),
            (GreaterEqual, Type::Integer(a), Type::Float(b)) => {
                Type::Boolean(a.to_f64().unwrap() >= b)
            }
            (GreaterEqual, Type::Float(a), Type::Integer(b)) => {
                Type::Boolean(a >= b.to_f64().unwrap())
            }
            (GreaterEqual, Type::Float(a), Type::Float(b)) => Type::Boolean(a >= b),
            (GreaterEqual, Type::Str(a), Type::Str(b)) => Type::Boolean(a.ge(&b)),
            (op, l, r) => return Err(SmntcError::IncompatibleComparation(*op, l, r, None)),
        };

        Ok(expr_type)
    }

    fn analyze_bin_logic(
        &mut self,
        op: &BinaryLogicOp,
        a: &Expr,
        b: &Expr,
    ) -> SemanticAnalyzerResult {
        use BinaryLogicOp::*;

        let type_a = self.analyze_one(a)?;
        let type_b = self.analyze_one(b)?;

        let expr_type = match (op, type_a, type_b) {
            (And, Type::Boolean(a), Type::Boolean(b)) => Type::Boolean(a && b),
            (Or, Type::Boolean(a), Type::Boolean(b)) => Type::Boolean(a || b),
            (op, l, r) => return Err(SmntcError::IncompatibleLogicOp(*op, l, r)),
        };

        Ok(expr_type)
    }

    fn analyze_logic_not(&mut self, exp: &Expr) -> SemanticAnalyzerResult {
        let exp_type = self.analyze_one(exp)?;

        match exp_type {
            Type::Boolean(b) => Ok(Type::Boolean(!b)),
            t => Err(SmntcError::IncompatibleLogicNot(t)),
        }
    }

    fn analyze_unary(&mut self, op: &UnaryOp, exp: &Expr) -> SemanticAnalyzerResult {
        let exp_type = self.analyze_one(exp)?;

        match (op, exp_type) {
            (_, Type::Integer(x)) => Ok(Type::Integer(x)),
            (_, Type::Float(x)) => Ok(Type::Float(x)),
            (op, t) => Err(SmntcError::IncompatibleUnaryOp(*op, t)),
        }
    }

    fn analyze_literal(&self, value: &Value) -> Type {
        match value {
            Value::PythonNone => Type::Null,
            Value::Bool(b) => Type::Boolean(*b),
            Value::Number(NumberType::Integer(x)) => Type::Integer(x.clone()),
            Value::Number(NumberType::Float(x)) => Type::Float(*x),
            Value::Str(x) => Type::Str(x.to_string()),
            Value::Fun(x) => Type::Fun(x.param_types(), x.ret_type.clone()),
        }
    }

    fn analyze_variable_expr(&mut self, id: &str, line: usize) -> SemanticAnalyzerResult {
        if let Some(t) = self.get_var(id) {
            Ok(t)
        } else {
            Err(SmntcError::VariableNotDeclared(line, id.to_string()))
        }
    }

    fn analyze_call_expr(&mut self, callee: &Expr, args: &[Expr]) -> SemanticAnalyzerResult {
        if let Type::Fun(param_type, ret_type) = self.analyze_one(callee)? {
            if param_type.len() != args.len() {
                return Err(SmntcError::WrongArity);
            }

            self.with_new_env(|analyzer| {
                for (arg, param_var_type) in args.iter().zip(&param_type) {
                    let arg_type = analyzer.analyze_one(arg)?;

                    let arg_var_type: VarType = arg_type.clone().into();
                    let param_type: Type = param_var_type.into();

                    if arg_var_type != *param_var_type {
                        return Err(SmntcError::MismatchedTypes(param_type, arg_type, None));
                    }
                }

                Ok(())
            })?;

            Ok((&ret_type).into())
        } else {
            Err(SmntcError::NotCallable)
        }
    }

    fn analyze_one(&mut self, exp: &Expr) -> SemanticAnalyzerResult {
        match exp {
            Expr::BinaryArith(a, op_and_token, b) => self.analyze_bin_arith(&op_and_token.op, a, b),
            Expr::BinaryComp(a, op_and_token, b) => self.analyze_bin_comp(&op_and_token.op, a, b),
            Expr::BinaryLogic(a, op_and_token, b) => self.analyze_bin_logic(&op_and_token.op, a, b),
            Expr::LogicNot((exp, _)) => self.analyze_logic_not(exp),
            Expr::Unary(op_and_token, exp) => self.analyze_unary(&op_and_token.op, exp),
            Expr::Grouping(exp) => self.analyze_one(exp),
            Expr::Literal(value_and_token) => Ok(self.analyze_literal(&value_and_token.op)),
            Expr::Variable(token, id) => self.analyze_variable_expr(id, token.placement().line),
            Expr::Call(callee, args) => self.analyze_call_expr(callee, args),
        }
    }

    // todo check "main" before everything (because scope)
    pub fn analyze(&mut self, stmts: &'a [Stmt]) -> Result<(), Vec<Error>> {
        for stmt in stmts {
            match stmt {
                Stmt::Assert(exp) => match self.analyze_one(exp) {
                    Ok(Type::Boolean(_)) => {}
                    Ok(t) => self.errors.push(Error::Smntc(SmntcError::MismatchedTypes(
                        Type::Boolean(true),
                        t,
                        None,
                    ))),
                    Err(err) => self.errors.push(Error::Smntc(err)),
                },
                Stmt::ExprStmt(exp) => match self.analyze_one(exp) {
                    Ok(t) => self.insert(&exp, t),
                    Err(err) => self.errors.push(Error::Smntc(err)),
                },
                Stmt::VarStmt(id, var_type, expr) => {
                    let error_line = expr.get_line();

                    match self.analyze_one(expr) {
                        Ok(t) => match (var_type, t.clone()) {
                            (Some(VarType::Boolean), Type::Boolean(_)) => {
                                if self.insert_var(id, t).is_some() {
                                    self.errors.push(Error::Smntc(
                                        SmntcError::VariableAlreadyDeclared(
                                            error_line,
                                            id.to_string(),
                                        ),
                                    ));
                                }
                            }
                            (Some(VarType::Integer), Type::Integer(_)) => {
                                if self.insert_var(id, t).is_some() {
                                    self.errors.push(Error::Smntc(
                                        SmntcError::VariableAlreadyDeclared(
                                            error_line,
                                            id.to_string(),
                                        ),
                                    ));
                                }
                            }
                            (Some(VarType::Float), Type::Float(_)) => {
                                if self.insert_var(id, t).is_some() {
                                    self.errors.push(Error::Smntc(
                                        SmntcError::VariableAlreadyDeclared(
                                            error_line,
                                            id.to_string(),
                                        ),
                                    ));
                                }
                            }
                            (Some(VarType::Str), Type::Str(_)) => {
                                if self.insert_var(id, t).is_some() {
                                    self.errors.push(Error::Smntc(
                                        SmntcError::VariableAlreadyDeclared(
                                            error_line,
                                            id.to_string(),
                                        ),
                                    ));
                                }
                            }
                            (Some(VarType::PythonNone), Type::Null) => {
                                if self.insert_var(id, t).is_some() {
                                    self.errors.push(Error::Smntc(
                                        SmntcError::VariableAlreadyDeclared(
                                            error_line,
                                            id.to_string(),
                                        ),
                                    ));
                                }
                            }
                            (None, _) => {
                                if self.insert_var(id, t).is_some() {
                                    self.errors.push(Error::Smntc(
                                        SmntcError::VariableAlreadyDeclared(
                                            error_line,
                                            id.to_string(),
                                        ),
                                    ));
                                }
                            }
                            (Some(expected), found) => {
                                self.errors
                                    .push(Error::Smntc(SmntcError::IncompatibleDeclaration(
                                        error_line,
                                        expected.clone(),
                                        found,
                                    )))
                            }
                        },
                        Err(err) => self.errors.push(Error::Smntc(err)),
                    }
                }
                // FIXME find a way to eval if and else branches without symbol_table colision
                Stmt::IfStmt(condition, then_branch, else_branch) => {
                    match self.analyze_one(condition) {
                        Ok(Type::Boolean(x)) => {
                            self.insert(condition, Type::Boolean(x));

                            if x {
                                self.analyze(then_branch).ok();
                            } else if else_branch.is_some() {
                                self.analyze(else_branch.as_ref().unwrap()).ok();
                            }
                        }
                        Ok(_) => self
                            .errors
                            .push(Error::Smntc(SmntcError::IfNotLogicalCondition)),
                        Err(err) => self.errors.push(Error::Smntc(err)),
                    };
                }
                Stmt::Function(id_token, params, body, ret_type) => {
                    let mut param_types = vec![];
                    for (_, var_type) in params {
                        param_types.push(var_type.clone())
                    }

                    if self
                        .insert_var(&id_token.lexeme, Type::Fun(param_types, ret_type.clone()))
                        .is_some()
                    {
                        self.errors
                            .push(Error::Smntc(SmntcError::VariableAlreadyDeclared(
                                id_token.placement.line,
                                id_token.lexeme(),
                            )));
                    }

                    if let Err(err) = self.with_new_env(|analyzer| {
                        for (token, var_type) in params {
                            if analyzer
                                .insert_var(&token.lexeme, var_type.into())
                                .is_some()
                            {
                                analyzer.errors.push(Error::Smntc(
                                    SmntcError::VariableAlreadyDeclared(
                                        token.placement.line,
                                        token.lexeme(),
                                    ),
                                ));
                            }
                        }

                        analyzer.analyze(&body).ok(); // Errors will already be pushed to self.errors

                        let return_stmts: Vec<(&Token, &Option<Expr>)> = body
                            .iter()
                            .filter_map(|stmt| match stmt {
                                Stmt::ReturnStmt(token, expr) => Some((token, expr)),
                                _ => None,
                            })
                            .collect();

                        if ret_type != &VarType::PythonNone && return_stmts.is_empty() {
                            return Err(SmntcError::MismatchedTypes(
                                ret_type.into(),
                                (&VarType::PythonNone).into(),
                                None,
                            ));
                        }

                        for (_, op_expr) in return_stmts {
                            match op_expr {
                                Some(expr) => {
                                    let x = analyzer.analyze_one(expr)?;
                                    let var_type: VarType = x.clone().into();

                                    if &var_type != ret_type {
                                        return Err(SmntcError::MismatchedTypes(
                                            ret_type.into(),
                                            x,
                                            None,
                                        ));
                                    }
                                }
                                None => {
                                    if ret_type != &VarType::PythonNone {
                                        return Err(SmntcError::MismatchedTypes(
                                            ret_type.into(),
                                            (&VarType::PythonNone).into(),
                                            None,
                                        ));
                                    }
                                }
                            }
                        }
                        Ok(())
                    }) {
                        self.errors.push(Error::Smntc(err))
                    }
                }
                Stmt::ReturnStmt(_, _) => {}
            }
        }

        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(self.errors.clone())
        }
    }

    pub fn new() -> Self {
        SemanticAnalyzer {
            types: HashMap::default(),
            symbol_table: Environment::default(),
            errors: Vec::default(),
        }
    }
}
