use crate::{
    environment::Environment,
    error::{Error, RuntimeError},
    expr::*,
    stmt::*,
    token::NumberType,
};
use num_traits::ToPrimitive;

type InterpreterResult = std::result::Result<Value, RuntimeError>;

#[derive(Default)]
pub struct Interpreter {
    global_environment: Environment,
}

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

        // FIXME panic will never happen
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
            _ => panic!("interpreter::binary_arith_op failed unexpectedly"),
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

    fn unary_op(&self, op: &UnaryOp, right: &Value) -> InterpreterResult {
        use UnaryOp::*;
        use Value::*;

        // FIXME panic will never happen
        match (op, right) {
            (Minus, Number(NumberType::Integer(a))) => Ok(Number(NumberType::Integer(-a.clone()))),
            (Minus, Number(NumberType::Float(a))) => Ok(Number(NumberType::Float(-*a))),
            (Plus, Number(NumberType::Integer(a))) => Ok(Number(NumberType::Integer(a.clone()))),
            (Plus, Number(NumberType::Float(a))) => Ok(Number(NumberType::Float(*a))),
            _ => panic!("interpreter::unary_op failed unexpectedly"),
        }
    }

    fn eval_logic_not(&self, expr: &Expr) -> InterpreterResult {
        let value = self.eval_expr(expr)?;

        let evaluated_value = match value {
            Value::Bool(a) => Value::Bool(!a),
            _ => panic!("interpreter::eval_logic_not failed unexpectedly"),
        };

        Ok(evaluated_value)
    }

    fn eval_unary_expr(
        &self,
        op_and_token: &OpWithToken<UnaryOp>,
        right: &Expr,
    ) -> InterpreterResult {
        let (op, _token) = op_and_token;
        let eval_right = self.eval_expr(right)?;

        self.unary_op(op, &eval_right)
    }

    fn binary_comp_op(&self, left: &Value, op: &BinaryCompOp, right: &Value) -> InterpreterResult {
        let evaluated_value = match (op, left, right) {
            (BinaryCompOp::NotEqual, Value::Number(a), Value::Number(b)) => Value::Bool(a != b),
            (BinaryCompOp::NotEqual, Value::Bool(a), Value::Bool(b)) => Value::Bool(a != b),
            (BinaryCompOp::NotEqual, Value::Str(a), Value::Str(b)) => Value::Bool(a != b),
            (BinaryCompOp::NotEqual, Value::PythonNone, Value::PythonNone) => Value::Bool(false),
            (BinaryCompOp::NotEqual, _, Value::PythonNone) => Value::Bool(true),
            (BinaryCompOp::NotEqual, Value::PythonNone, _) => Value::Bool(true),
            (BinaryCompOp::Equal, Value::Number(a), Value::Number(b)) => Value::Bool(a == b),
            (BinaryCompOp::Equal, Value::Bool(a), Value::Bool(b)) => Value::Bool(a == b),
            (BinaryCompOp::Equal, Value::Str(a), Value::Str(b)) => Value::Bool(a == b),
            (BinaryCompOp::Equal, Value::PythonNone, Value::PythonNone) => Value::Bool(true),
            (BinaryCompOp::Equal, _, Value::PythonNone) => Value::Bool(false),
            (BinaryCompOp::Equal, Value::PythonNone, _) => Value::Bool(false),
            (BinaryCompOp::LessThan, Value::Number(a), Value::Number(b)) => Value::Bool(a < b),
            // TODO how to compare strings?
            (BinaryCompOp::LessThan, Value::Str(a), Value::Str(b)) => {
                Value::Bool(a.chars().count() < b.chars().count())
            }
            (BinaryCompOp::LessEqual, Value::Number(a), Value::Number(b)) => Value::Bool(a <= b),
            (BinaryCompOp::LessEqual, Value::Str(a), Value::Str(b)) => {
                Value::Bool(a.chars().count() <= b.chars().count())
            }
            (BinaryCompOp::GreaterThan, Value::Number(a), Value::Number(b)) => Value::Bool(a > b),
            (BinaryCompOp::GreaterThan, Value::Str(a), Value::Str(b)) => {
                Value::Bool(a.chars().count() > b.chars().count())
            }
            (BinaryCompOp::GreaterEqual, Value::Number(a), Value::Number(b)) => Value::Bool(a >= b),
            (BinaryCompOp::GreaterEqual, Value::Str(a), Value::Str(b)) => {
                Value::Bool(a.chars().count() >= b.chars().count())
            }
            _ => panic!("interpreter::binary_comp_op failed unexpectedly"),
        };

        Ok(evaluated_value)
    }

    fn eval_binary_comp_expr(
        &self,
        left: &Expr,
        op_and_token: &OpWithToken<BinaryCompOp>,
        right: &Expr,
    ) -> InterpreterResult {
        let (op, _token) = op_and_token;
        let eval_left = self.eval_expr(left)?;
        let eval_right = self.eval_expr(right)?;

        self.binary_comp_op(&eval_left, op, &eval_right)
    }

    fn binary_logic_op(
        &self,
        left: &Value,
        op: &BinaryLogicOp,
        right: &Value,
    ) -> InterpreterResult {
        // This is python's thing, don't judge me.
        let evaluated_value = match (op, left, right) {
            (BinaryLogicOp::And, Value::Bool(true), Value::PythonNone) => Value::PythonNone,
            (BinaryLogicOp::And, Value::Bool(false), Value::PythonNone) => Value::Bool(false),
            (BinaryLogicOp::And, Value::Bool(a), Value::Bool(b)) => Value::Bool(*a && *b),
            (BinaryLogicOp::And, Value::PythonNone, Value::Bool(_)) => Value::PythonNone,
            (BinaryLogicOp::Or, Value::Bool(true), Value::PythonNone) => Value::Bool(true),
            (BinaryLogicOp::Or, Value::Bool(false), Value::PythonNone) => Value::PythonNone,
            (BinaryLogicOp::Or, Value::Bool(a), Value::Bool(b)) => Value::Bool(*a || *b),
            (BinaryLogicOp::Or, Value::PythonNone, Value::Bool(b)) => Value::Bool(*b),
            _ => panic!("interpreter::binary_logic_op failed unexpectedly"),
        };

        Ok(evaluated_value)
    }

    fn eval_binary_logic_expr(
        &self,
        left: &Expr,
        op_and_token: &OpWithToken<BinaryLogicOp>,
        right: &Expr,
    ) -> InterpreterResult {
        let (op, _token) = op_and_token;
        let eval_left = self.eval_expr(left)?;
        let eval_right = self.eval_expr(right)?;

        self.binary_logic_op(&eval_left, op, &eval_right)
    }

    fn eval_expr(&self, expr: &Expr) -> InterpreterResult {
        use Expr::*;
        match expr {
            BinaryArith(left, op_and_token, right) => {
                self.eval_binary_arith_expr(left, op_and_token, right)
            }
            BinaryComp(left, op_and_token, right) => {
                self.eval_binary_comp_expr(left, op_and_token, right)
            }
            BinaryLogic(left, op_and_token, right) => {
                self.eval_binary_logic_expr(left, op_and_token, right)
            }
            LogicNot((expr, _token)) => self.eval_logic_not(expr),
            Unary(op_and_token, right) => self.eval_unary_expr(op_and_token, right),
            Grouping(new_expr) => self.eval_expr(new_expr),
            Literal(value_and_token) => Ok(value_and_token.clone().0),
        }
    }

    fn eval_var_stmt(&mut self, identifier: &str, expr: &Expr) -> Result<(), RuntimeError> {
        let value = self.eval_expr(expr)?;

        self.global_environment
            .insert(identifier.to_string(), value);

        Ok(())
    }

    fn eval(&mut self, stmt: &Stmt) -> Option<Error> {
        use Stmt::*;
        match stmt {
            Assert(expr) => match self.eval_expr(expr) {
                Ok(val) if val != Value::Bool(true) => {
                    // TODO: add line number
                    Some(Error::Runtime(RuntimeError::AssertionFailed))
                }
                Err(error) => Some(Error::Runtime(error)),
                _ => None,
            },
            ExprStmt(expr) => match self.eval_expr(expr) {
                Ok(value) => {
                    println!("{}", value); // TODO remove print
                    None
                }
                Err(error) => Some(Error::Runtime(error)),
            },
            VarStmt(identifier, _var_type, expr) => match self.eval_var_stmt(identifier, expr) {
                Ok(()) => None,
                Err(error) => Some(Error::Runtime(error)),
            },
        }
    }

    pub fn interpret(&mut self, stmts: Vec<Stmt>) -> Option<Error> {
        self.global_environment = Environment::default();

        for stmt in stmts {
            if let Some(evaluation) = self.eval(&stmt) {
                return Some(evaluation);
            }
        }

        println!("{:?}", self.global_environment);

        None
    }
}
