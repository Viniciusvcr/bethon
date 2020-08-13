use crate::{
    error::{Error, RuntimeError},
    expr::*,
    stmt::*,
    token::NumberType,
};
use num_traits::ToPrimitive;

type InterpreterResult = std::result::Result<Value, RuntimeError>;

#[derive(Default)]
pub struct Interpreter {}

impl Interpreter {
    fn binary_arith_op(
        &self,
        left: &Value,
        op_and_token: &OpWithToken<BinaryOp>,
        right: &Value,
    ) -> InterpreterResult {
        use BinaryOp::*;
        use Value::*;

        let (op, token) = op_and_token;

        match (op, left, right) {
            (Sub, Number(a), Number(b)) => Ok(Number(a.clone() - b.clone())),
            (Add, Number(a), Number(b)) => Ok(Number(a.clone() + b.clone())),
            (Div, Number(NumberType::Integer(a)), Number(NumberType::Integer(b))) => {
                if *b != num_bigint::BigInt::from(0) {
                    Ok(Value::Number(NumberType::Integer(a / b)))
                } else {
                    Err(RuntimeError::DivisionByZero(
                        token.placement().line,
                        token.placement().starts_at,
                        token.placement().ends_at,
                    ))
                }
            }
            (Div, Number(NumberType::Float(a)), Number(NumberType::Integer(b))) => {
                if *b != num_bigint::BigInt::from(0) {
                    Ok(Value::Number(NumberType::Float(a / b.to_f64().unwrap())))
                } else {
                    Err(RuntimeError::DivisionByZero(
                        token.placement().line,
                        token.placement().starts_at,
                        token.placement().ends_at,
                    ))
                }
            }
            (Div, Number(NumberType::Integer(a)), Number(NumberType::Float(b))) => {
                if *b != 0.0 {
                    Ok(Value::Number(NumberType::Float(a.to_f64().unwrap() / b)))
                } else {
                    Err(RuntimeError::DivisionByZero(
                        token.placement().line,
                        token.placement().starts_at,
                        token.placement().ends_at,
                    ))
                }
            }
            (Div, Number(NumberType::Float(a)), Number(NumberType::Float(b))) => {
                if *b != 0.0 {
                    Ok(Value::Number(NumberType::Float(a / b)))
                } else {
                    Err(RuntimeError::DivisionByZero(
                        token.placement().line,
                        token.placement().starts_at,
                        token.placement().ends_at,
                    ))
                }
            }
            (Mul, Number(a), Number(b)) => Ok(Number(a.clone() * b.clone())),
            (Mod, Number(a), Number(b)) => Ok(Number(a.clone() % b.clone())),
            _ => panic!(), // FIXME shouldn`t happen
        }
    }

    fn eval_binary_arith_expr(
        &self,
        left: &Expr,
        op_and_token: &OpWithToken<BinaryOp>,
        right: &Expr,
    ) -> InterpreterResult {
        let eval_left = self.eval_expr(left)?;
        let eval_right = self.eval_expr(right)?;

        self.binary_arith_op(&eval_left, op_and_token, &eval_right)
    }

    fn unary_op(&self, op_and_token: &OpWithToken<UnaryOp>, right: &Value) -> InterpreterResult {
        use UnaryOp::*;
        use Value::*;

        let (op, _token) = op_and_token;

        // FIXME these errors should be catched by the parser, therefore, shouldn't appear here
        match (op, right) {
            (Minus, PythonNone) => Err(RuntimeError::NotAllowed),
            (Minus, Bool(_)) => Err(RuntimeError::NotAllowed),
            (Minus, Number(NumberType::Integer(a))) => Ok(Number(NumberType::Integer(-a.clone()))),
            (Minus, Number(NumberType::Float(a))) => Ok(Number(NumberType::Float(-*a))),
            (Minus, Str(_)) => Err(RuntimeError::NotAllowed),
            (Plus, PythonNone) => Err(RuntimeError::NotAllowed),
            (Plus, Bool(_)) => Err(RuntimeError::NotAllowed),
            (Plus, Number(NumberType::Integer(a))) => Ok(Number(NumberType::Integer(a.clone()))),
            (Plus, Number(NumberType::Float(a))) => Ok(Number(NumberType::Float(*a))),
            (Plus, Str(_)) => Err(RuntimeError::NotAllowed),
        }
    }

    fn eval_unary_expr(
        &self,
        op_and_token: &OpWithToken<UnaryOp>,
        right: &Expr,
    ) -> InterpreterResult {
        let eval_right = self.eval_expr(right)?;

        self.unary_op(op_and_token, &eval_right)
    }

    fn eval_expr(&self, expr: &Expr) -> InterpreterResult {
        use Expr::*;
        match expr {
            BinaryArith(left, op_and_token, right) => {
                self.eval_binary_arith_expr(left, op_and_token, right)
            }
            Unary(op_and_token, right) => self.eval_unary_expr(op_and_token, right),
            Grouping(new_expr) => self.eval_expr(new_expr),
            Literal(value_and_token) => Ok(value_and_token.clone().0),
            _ => unimplemented!(),
        }
    }

    fn eval(&self, stmt: &Stmt) -> Option<Error> {
        use Stmt::*;
        match stmt {
            ExprStmt(expr) => match self.eval_expr(expr) {
                Ok(value) => {
                    println!("{}", value);
                    None
                }
                Err(error) => Some(Error::Runtime(error)),
            },
        }
    }

    pub fn interpret(&mut self, stmts: &[Stmt]) -> Option<Error> {
        for stmt in stmts {
            if let Some(evaluation) = self.eval(stmt) {
                return Some(evaluation);
            }
        }

        None
    }
}