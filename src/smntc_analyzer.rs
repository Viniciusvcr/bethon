use crate::{
    environment::{Environment, SemanticEnvironment},
    error::{semantic::SmntcError, Error},
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
    Fun(SemanticEnvironment, Vec<VarType>, VarType, Vec<String>),
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
            VarType::Function => Type::Fun(
                SemanticEnvironment::default(),
                Vec::default(),
                VarType::Integer,
                Vec::default(),
            ),
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
            Type::Fun(_, _, ret, _) => write!(f, "<function> -> {}", ret),
        }
    }
}

#[allow(dead_code)]
// The semantic analyzer
pub struct SemanticAnalyzer<'a> {
    types: HashMap<&'a Expr, Type>, // can't retrieve from this hashmap
    symbol_table: SemanticEnvironment,
    errors: Vec<Error>,
    analyzing_function: bool,
    fun_ret_type: Option<VarType>,
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

    #[allow(dead_code)]
    fn insert(&mut self, expr: &'a Expr, t: Type) {
        self.types.insert(expr, t);
    }

    fn declare(&mut self, id: &str, t: Type) {
        self.symbol_table.define(id.to_string(), (t, false));
    }

    fn define(&mut self, id: &str, t: Type) -> Option<(Type, bool)> {
        self.symbol_table.define(id.to_string(), (t, true))
    }

    fn declare_env_vars(&mut self, body: &[Stmt]) -> Result<(), SmntcError> {
        let vec = body.iter().filter_map(|stmt| match stmt {
            Stmt::VarStmt(id, var_type, expr) => Some((id, var_type, expr)),
            _ => None,
        });

        for (id, var_type, expr) in vec {
            if var_type.is_none() {
                let evaluated_type = self.analyze_one(expr)?;
                self.declare(&id, evaluated_type);
            } else {
                self.declare(&id, var_type.as_ref().unwrap().into());
            }
        }

        let another_vec = body.iter().filter_map(|stmt| match stmt {
            Stmt::IfStmt(_, then_branch, else_branch) => Some((then_branch, else_branch)),
            _ => None,
        });

        for (then, else_) in another_vec {
            self.declare_env_vars(then)?;
            if let Some(else_branch) = else_ {
                self.declare_env_vars(else_branch)?;
            }
        }

        Ok(())
    }

    fn get_var(&mut self, id: &str) -> Option<Type> {
        let x = self.symbol_table.get(id);

        if let Some((var_type, defined)) = x {
            if !self.analyzing_function {
                if defined {
                    Some(var_type)
                } else {
                    None
                }
            } else {
                Some(var_type)
            }
        } else {
            None
        }
    }

    fn analyze_bin_arith(
        &mut self,
        op: &BinaryOp,
        a: &Expr,
        b: &Expr,
        original_expr: &Expr,
    ) -> SemanticAnalyzerResult {
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
            (op, l, r) => {
                let (line, starts_at, ends_at) = original_expr.placement();
                return Err(SmntcError::IncompatibleBinArith(
                    line, starts_at, ends_at, *op, l, r,
                ));
            }
        };

        Ok(expr_type)
    }

    fn analyze_bin_comp(
        &mut self,
        op: &BinaryCompOp,
        a: &Expr,
        b: &Expr,
        original_expr: &Expr,
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
            (op, l, r) => {
                let (line, starts_at, ends_at) = original_expr.placement();

                return Err(SmntcError::IncompatibleComparation(
                    line, starts_at, ends_at, *op, l, r,
                ));
            }
        };

        Ok(expr_type)
    }

    fn analyze_bin_logic(
        &mut self,
        op: &BinaryLogicOp,
        a: &Expr,
        b: &Expr,
        original_expr: &Expr,
    ) -> SemanticAnalyzerResult {
        use BinaryLogicOp::*;

        let type_a = self.analyze_one(a)?;
        let type_b = self.analyze_one(b)?;

        let expr_type = match (op, type_a, type_b) {
            (And, Type::Boolean(a), Type::Boolean(b)) => Type::Boolean(a && b),
            (Or, Type::Boolean(a), Type::Boolean(b)) => Type::Boolean(a || b),
            (op, l, r) => {
                let (line, starts_at, ends_at) = original_expr.placement();

                return Err(SmntcError::IncompatibleLogicOp(
                    line, starts_at, ends_at, *op, l, r,
                ));
            }
        };

        Ok(expr_type)
    }

    fn analyze_logic_not(&mut self, exp: &Expr) -> SemanticAnalyzerResult {
        let exp_type = self.analyze_one(exp)?;

        match exp_type {
            Type::Boolean(b) => Ok(Type::Boolean(!b)),
            t => {
                let (line, starts_at, ends_at) = exp.placement();
                Err(SmntcError::IncompatibleLogicNot(
                    line, starts_at, ends_at, t,
                ))
            }
        }
    }

    fn analyze_unary(
        &mut self,
        op: &UnaryOp,
        exp: &Expr,
        original_expr: &Expr,
    ) -> SemanticAnalyzerResult {
        let exp_type = self.analyze_one(exp)?;

        match (op, exp_type) {
            (_, Type::Integer(x)) => Ok(Type::Integer(x)),
            (_, Type::Float(x)) => Ok(Type::Float(x)),
            (op, t) => {
                let (line, starts_at, ends_at) = original_expr.placement();

                Err(SmntcError::IncompatibleUnaryOp(
                    line, starts_at, ends_at, *op, t,
                ))
            }
        }
    }

    fn analyze_literal(&self, value: &Value) -> Type {
        match value {
            Value::PythonNone => Type::Null,
            Value::Bool(b) => Type::Boolean(*b),
            Value::Number(NumberType::Integer(x)) => Type::Integer(x.clone()),
            Value::Number(NumberType::Float(x)) => Type::Float(*x),
            Value::Str(x) => Type::Str(x.to_string()),
            Value::Fun(x) => Type::Fun(
                SemanticEnvironment::default(),
                vec![],
                x.ret_type.clone(),
                vec![],
            ),
        }
    }

    fn analyze_variable_expr(&mut self, id: &str, token: &Token) -> SemanticAnalyzerResult {
        if let Some(t) = self.get_var(id) {
            Ok(t)
        } else {
            let (line, starts_at, ends_at) = token.placement.as_tuple();
            Err(SmntcError::VariableNotDeclared(
                line,
                starts_at,
                ends_at,
                id.to_string(),
            ))
        }
    }

    fn analyze_call_expr(&mut self, callee: &Expr, args: &[Expr]) -> SemanticAnalyzerResult {
        match self.analyze_one(callee)? {
            Type::Fun(env, param_type, ret_type, declared_keys) => {
                if param_type.len() != args.len() {
                    let (line, starts_at, mut ends_at) = callee.placement();

                    if !args.is_empty() {
                        let (_, _, last_arg_end) = args.last().unwrap().placement();

                        ends_at = last_arg_end + 1;
                    } else {
                        ends_at += 2;
                    }

                    return Err(SmntcError::WrongArity(
                        line,
                        starts_at,
                        ends_at,
                        param_type.len(),
                        args.len(),
                    ));
                } else if !self.analyzing_function {
                    // println!("{:#?}", env);

                    let old_env = std::mem::replace(&mut self.symbol_table, env.clone());
                    for (arg, param_var_type) in args.iter().zip(&param_type) {
                        let arg_type = self.analyze_one(arg)?;

                        let arg_var_type: VarType = arg_type.clone().into();
                        let param_type: Type = param_var_type.into();

                        if arg_var_type != *param_var_type {
                            let (line, (starts_at, ends_at)) =
                                (arg.get_line(), arg.get_expr_placement());
                            return Err(SmntcError::MismatchedTypes(
                                line, starts_at, ends_at, param_type, arg_type,
                            ));
                        }
                    }

                    for key in &declared_keys {
                        if let None = self.get_var(&key) {
                            self.errors.push(Error::Smntc(SmntcError::UnboundVar));
                        }
                    }
                    self.symbol_table = old_env;
                }

                Ok((&ret_type).into())
            }
            t => {
                let (line, starts_at, ends_at) = callee.placement();
                Err(SmntcError::NotCallable(line, starts_at, ends_at, t))
            }
        }
    }

    fn analyze_one(&mut self, exp: &Expr) -> SemanticAnalyzerResult {
        match exp {
            Expr::BinaryArith(a, op_and_token, b) => {
                self.analyze_bin_arith(&op_and_token.op, a, b, exp)
            }
            Expr::BinaryComp(a, op_and_token, b) => {
                self.analyze_bin_comp(&op_and_token.op, a, b, exp)
            }
            Expr::BinaryLogic(a, op_and_token, b) => {
                self.analyze_bin_logic(&op_and_token.op, a, b, exp)
            }
            Expr::LogicNot((exp, _)) => self.analyze_logic_not(exp),
            Expr::Unary(op_and_token, expr) => self.analyze_unary(&op_and_token.op, expr, exp),
            Expr::Grouping(exp) => self.analyze_one(exp),
            Expr::Literal(value_and_token) => Ok(self.analyze_literal(&value_and_token.op)),
            Expr::Variable(token, id) => self.analyze_variable_expr(id, token),
            Expr::Call(callee, args) => self.analyze_call_expr(callee, args),
        }
    }

    // todo check "main" before everything (because scope)
    pub fn analyze(&mut self, stmts: &[Stmt]) -> Result<(), Vec<Error>> {
        if let Err(err) = self.declare_env_vars(stmts) {
            self.errors.push(Error::Smntc(err));
        }

        for stmt in stmts {
            match stmt {
                Stmt::Assert(exp) => match self.analyze_one(exp) {
                    Ok(Type::Boolean(_)) => {}
                    Ok(t) => {
                        let (line, (starts_at, ends_at)) =
                            (exp.get_line(), exp.get_expr_placement());
                        self.errors.push(Error::Smntc(SmntcError::MismatchedTypes(
                            line,
                            starts_at,
                            ends_at,
                            Type::Boolean(true),
                            t,
                        )))
                    }
                    Err(err) => self.errors.push(Error::Smntc(err)),
                },
                Stmt::ExprStmt(exp) => match self.analyze_one(exp) {
                    Ok(_t) => { /*self.insert(&exp, t)*/ }
                    Err(err) => self.errors.push(Error::Smntc(err)),
                },
                Stmt::VarStmt(id, var_type, expr) => {
                    let (error_line, starts_at, ends_at) = expr.placement();

                    match self.analyze_one(expr) {
                        Ok(t) => match (var_type, t.clone()) {
                            (Some(VarType::Boolean), Type::Boolean(_)) => {
                                match self.define(id, t) {
                                    Some((_, true)) => self.errors.push(Error::Smntc(
                                        SmntcError::VariableAlreadyDeclared(
                                            error_line,
                                            starts_at,
                                            ends_at,
                                            id.to_string(),
                                        ),
                                    )),
                                    None | Some((_, false)) => {}
                                }
                            }
                            (Some(VarType::Integer), Type::Integer(_)) => {
                                match self.define(id, t) {
                                    Some((_, true)) => self.errors.push(Error::Smntc(
                                        SmntcError::VariableAlreadyDeclared(
                                            error_line,
                                            starts_at,
                                            ends_at,
                                            id.to_string(),
                                        ),
                                    )),
                                    None | Some((_, false)) => {}
                                }
                            }
                            (Some(VarType::Float), Type::Float(_)) => match self.define(id, t) {
                                Some((_, true)) => self.errors.push(Error::Smntc(
                                    SmntcError::VariableAlreadyDeclared(
                                        error_line,
                                        starts_at,
                                        ends_at,
                                        id.to_string(),
                                    ),
                                )),
                                None | Some((_, false)) => {}
                            },
                            (Some(VarType::Str), Type::Str(_)) => match self.define(id, t) {
                                Some((_, true)) => self.errors.push(Error::Smntc(
                                    SmntcError::VariableAlreadyDeclared(
                                        error_line,
                                        starts_at,
                                        ends_at,
                                        id.to_string(),
                                    ),
                                )),
                                None | Some((_, false)) => {}
                            },
                            (Some(VarType::PythonNone), Type::Null) => match self.define(id, t) {
                                Some((_, true)) => self.errors.push(Error::Smntc(
                                    SmntcError::VariableAlreadyDeclared(
                                        error_line,
                                        starts_at,
                                        ends_at,
                                        id.to_string(),
                                    ),
                                )),
                                None | Some((_, false)) => {}
                            },
                            (None, _) => match self.define(id, t) {
                                Some((_, true)) => self.errors.push(Error::Smntc(
                                    SmntcError::VariableAlreadyDeclared(
                                        error_line,
                                        starts_at,
                                        ends_at,
                                        id.to_string(),
                                    ),
                                )),
                                None | Some((_, false)) => {}
                            },
                            (Some(expected), found) => {
                                self.errors
                                    .push(Error::Smntc(SmntcError::IncompatibleDeclaration(
                                        error_line,
                                        starts_at,
                                        ends_at,
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
                            // self.insert(condition, Type::Boolean(x));

                            if x {
                                self.analyze(then_branch).ok();
                            } else if else_branch.is_some() {
                                self.analyze(else_branch.as_ref().unwrap()).ok();
                            }
                        }
                        Ok(t) => {
                            let (line, starts_at, ends_at) = condition.placement();
                            self.errors
                                .push(Error::Smntc(SmntcError::IfNotLogicalCondition(
                                    line, starts_at, ends_at, t,
                                )))
                        }
                        Err(err) => self.errors.push(Error::Smntc(err)),
                    };
                }
                Stmt::Function(id_token, params, body, ret_type) => {
                    self.analyzing_function = true;
                    self.fun_ret_type = Some(ret_type.clone());

                    let param_types: Vec<VarType> = params.iter().map(|(_, y)| y.clone()).collect();

                    if ret_type != &VarType::PythonNone && !self.validate_return(body) {
                        self.errors.push(Error::Smntc(SmntcError::MissingReturns(
                            id_token.placement.line,
                            id_token.placement.starts_at,
                            id_token.placement.ends_at,
                            ret_type.clone(),
                        )));
                    }

                    self.declare(
                        &id_token.lexeme,
                        Type::Fun(
                            SemanticEnvironment::default(),
                            param_types.clone(),
                            ret_type.clone(),
                            vec![],
                        ),
                    );

                    let (fun_env, declared_keys) = self.with_new_env(|analyzer| {
                        for (token, var_type) in params {
                            if analyzer.define(&token.lexeme, var_type.into()).is_some() {
                                let (line, starts_at, ends_at) = token.placement.as_tuple();
                                analyzer.errors.push(Error::Smntc(
                                    SmntcError::VariableAlreadyDeclared(
                                        line,
                                        starts_at,
                                        ends_at,
                                        token.lexeme(),
                                    ),
                                ));
                            }
                        }

                        analyzer.analyze(&body).ok(); // Errors will already be pushed to self.errors

                        (
                            analyzer.symbol_table.clone(),
                            analyzer.symbol_table.declared_keys(),
                        )
                    });

                    if let Some((_, true)) = self.define(
                        &id_token.lexeme,
                        Type::Fun(
                            fun_env,
                            param_types.clone(),
                            ret_type.clone(),
                            declared_keys,
                        ),
                    ) {
                        self.errors
                            .push(Error::Smntc(SmntcError::VariableAlreadyDeclared(
                                id_token.placement.line,
                                id_token.placement.starts_at,
                                id_token.placement.ends_at,
                                id_token.lexeme(),
                            )));
                    }

                    self.analyzing_function = false;
                    self.fun_ret_type = None;
                }
                Stmt::ReturnStmt(token, op_expr) => match op_expr {
                    Some(expr) => {
                        if self.fun_ret_type.is_some() {
                            match self.analyze_one(expr) {
                                Ok(x) => {
                                    let var_type: VarType = x.clone().into();

                                    if &var_type != self.fun_ret_type.as_ref().unwrap() {
                                        let (line, (starts_at, ends_at)) =
                                            (expr.get_line(), expr.get_expr_placement());

                                        self.errors.push(Error::Smntc(
                                            SmntcError::MismatchedTypes(
                                                line,
                                                starts_at,
                                                ends_at,
                                                self.fun_ret_type.as_ref().unwrap().into(),
                                                x,
                                            ),
                                        ));
                                    }
                                }
                                Err(err) => self.errors.push(Error::Smntc(err)),
                            }
                        } else {
                            let (line, starts_at, ends_at) = token.placement.as_tuple();

                            self.errors.push(Error::Smntc(SmntcError::TopLevelReturn(
                                line, starts_at, ends_at,
                            )))
                        }
                    }
                    None => {
                        if self.fun_ret_type.is_some() {
                            let (line, starts_at, ends_at) = token.placement.as_tuple();
                            self.errors.push(Error::Smntc(SmntcError::MismatchedTypes(
                                line,
                                starts_at,
                                ends_at,
                                self.fun_ret_type.as_ref().unwrap().into(),
                                (&VarType::PythonNone).into(),
                            )));
                        }
                    }
                },
            }
        }

        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(self.errors.clone())
        }
    }

    fn validate_return(&self, stmts: &[Stmt]) -> bool {
        if stmts
            .iter()
            .any(|stmt| matches!(stmt, Stmt::ReturnStmt(_, _)))
        {
            true
        } else {
            let if_stmt = stmts
                .iter()
                .filter_map(|stmt| match stmt {
                    Stmt::IfStmt(_, then_branch, else_branch) => Some((then_branch, else_branch)),
                    _ => None,
                })
                .collect::<Vec<(&Vec<Stmt>, &Option<Vec<Stmt>>)>>();

            let mut valid = false;
            for (then_branch, else_branch) in if_stmt.iter().rev() {
                valid |= self.validate_return(then_branch)
                    && self.validate_return(else_branch.as_ref().unwrap_or(&vec![]));

                if valid {
                    break;
                }
            }

            valid
        }
    }

    pub fn new() -> Self {
        SemanticAnalyzer {
            types: HashMap::default(),
            symbol_table: Environment::default(),
            errors: Vec::default(),
            analyzing_function: false,
            fun_ret_type: None,
        }
    }
}
