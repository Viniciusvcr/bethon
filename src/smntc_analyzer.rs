use crate::{
    error::{Error, SmntcError},
    expr::{BinaryCompOp, BinaryLogicOp, BinaryOp, Expr, UnaryOp, Value},
    stmt::Stmt,
    token::{NumberType, VarType},
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
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Null => write!(f, "None"),
            Type::Integer(_) => write!(f, "int"),
            Type::Float(_) => write!(f, "float"),
            Type::Boolean(_) => write!(f, "bool"),
            Type::Str(_) => write!(f, "str"),
        }
    }
}

// The semantic analyzer
#[derive(Default)]
pub struct SemanticAnalyzer<'a> {
    types: HashMap<&'a Expr, Type>,
    symbol_table: HashMap<&'a str, Type>,
}

type SemanticAnalyzerResult = Result<Type, SmntcError>;

impl<'a> SemanticAnalyzer<'a> {
    fn insert(&mut self, expr: &'a Expr, t: Type) {
        self.types.insert(expr, t);
    }

    fn insert_var(&mut self, id: &'a str, t: Type) -> Option<()> {
        if !self.symbol_table.contains_key(id) {
            self.symbol_table.insert(id, t);
            Some(())
        } else {
            None
        }
    }

    fn get_var(&mut self, id: &str) -> Option<&Type> {
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
        }
    }

    fn analyze_variable_expr(&mut self, id: &str, line: usize) -> SemanticAnalyzerResult {
        if let Some(t) = self.get_var(id) {
            Ok(t.clone())
        } else {
            Err(SmntcError::VariableNotDeclared(line, id.to_string()))
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
        }
    }

    pub fn analyze(&mut self, stmts: &'a [Stmt]) -> Result<(), Vec<Error>> {
        let mut errors: Vec<Error> = vec![];

        for stmt in stmts {
            match stmt {
                Stmt::Assert(exp) => match self.analyze_one(exp) {
                    Ok(Type::Boolean(_)) => {}
                    Ok(t) => errors.push(Error::Smntc(SmntcError::MismatchedTypes(
                        Type::Boolean(true),
                        t,
                        None,
                    ))),
                    Err(err) => errors.push(Error::Smntc(err)),
                },
                Stmt::ExprStmt(exp) => match self.analyze_one(exp) {
                    Ok(t) => self.insert(&exp, t),
                    Err(err) => errors.push(Error::Smntc(err)),
                },
                Stmt::VarStmt(id, var_type, expr) => {
                    let error_line = expr.get_line();

                    match self.analyze_one(expr) {
                        Ok(t) => match (var_type, t.clone()) {
                            (Some(VarType::Boolean), Type::Boolean(_)) => {
                                if self.insert_var(id, t).is_none() {
                                    errors.push(Error::Smntc(SmntcError::VariableAlreadyDeclared(
                                        error_line,
                                        id.to_string(),
                                    )));
                                }
                            }
                            (Some(VarType::Integer), Type::Integer(_)) => {
                                if self.insert_var(id, t).is_none() {
                                    errors.push(Error::Smntc(SmntcError::VariableAlreadyDeclared(
                                        error_line,
                                        id.to_string(),
                                    )));
                                }
                            }
                            (Some(VarType::Float), Type::Float(_)) => {
                                if self.insert_var(id, t).is_none() {
                                    errors.push(Error::Smntc(SmntcError::VariableAlreadyDeclared(
                                        error_line,
                                        id.to_string(),
                                    )));
                                }
                            }
                            (Some(VarType::Str), Type::Str(_)) => {
                                if self.insert_var(id, t).is_none() {
                                    errors.push(Error::Smntc(SmntcError::VariableAlreadyDeclared(
                                        error_line,
                                        id.to_string(),
                                    )));
                                }
                            }
                            (Some(VarType::PythonNone), Type::Null) => {
                                if self.insert_var(id, t).is_none() {
                                    errors.push(Error::Smntc(SmntcError::VariableAlreadyDeclared(
                                        error_line,
                                        id.to_string(),
                                    )));
                                }
                            }
                            (None, _) => {
                                if self.insert_var(id, t).is_none() {
                                    errors.push(Error::Smntc(SmntcError::VariableAlreadyDeclared(
                                        error_line,
                                        id.to_string(),
                                    )));
                                }
                            }
                            (Some(expected), found) => {
                                errors.push(Error::Smntc(SmntcError::IncompatibleDeclaration(
                                    error_line,
                                    expected.clone(),
                                    found,
                                )))
                            }
                        },
                        Err(err) => errors.push(Error::Smntc(err)),
                    }
                }
                Stmt::IfStmt(condition, then_branch, else_branch) => {
                    match self.analyze_one(condition) {
                        Ok(Type::Boolean(x)) => {
                            self.insert(condition, Type::Boolean(x));

                            if x {
                                if let Err(then_branch_errors) = self.analyze(then_branch) {
                                    for error in then_branch_errors {
                                        errors.push(error)
                                    }
                                }
                            } else if else_branch.is_some() {
                                if let Err(else_branch_errors) =
                                    self.analyze(else_branch.as_ref().unwrap())
                                {
                                    for error in else_branch_errors {
                                        errors.push(error)
                                    }
                                }
                            }
                        }
                        Ok(_) => errors.push(Error::Smntc(SmntcError::IfNotLogicalCondition)),
                        Err(err) => errors.push(Error::Smntc(err)),
                    };
                }
            }
        }

        println!("Types: {:?}\n", self.types);
        println!("Symbol Table: {:?}\n", self.symbol_table);

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }
}
