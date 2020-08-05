use crate::{
    error::{Error, RuntimeError},
    expr::*,
    stmt::*,
    token::NumberType,
};

type InterpreterResult = std::result::Result<Value, RuntimeError>;

pub struct Interpreter {}

impl Interpreter {
    fn binary_op(
        &self,
        left: &Value,
        op_and_token: &OpWithToken<BinaryOp>,
        right: &Value,
    ) -> InterpreterResult {
        use BinaryOp::*;
        use Value::*;

        let (op, token) = op_and_token;

        match (op, left, right) {
            (Sub, Number(a), Number(b)) => Ok(Number(*a - *b)),
            (Add, Number(a), Number(b)) => Ok(Number(*a + *b)),
            (Div, Number(NumberType::Integer(a)), Number(NumberType::Integer(b))) => {
                if *b != 0 {
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
                if *b != 0 {
                    Ok(Value::Number(NumberType::Float(a / *b as f64)))
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
                    Ok(Value::Number(NumberType::Float(*a as f64 / b)))
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
            (Mul, Number(a), Number(b)) => Ok(Number(*a * *b)),
            (Mod, Number(a), Number(b)) => Ok(Number(*a % *b)),
            (GreaterThan, Number(a), Number(b)) => Ok(Bool(a > b)),
            (GreaterEqual, Number(a), Number(b)) => Ok(Bool(a >= b)),
            (LessThan, Number(a), Number(b)) => Ok(Bool(a < b)),
            (LessEqual, Number(a), Number(b)) => Ok(Bool(a <= b)),
            (Equal, _, _) => Ok(Bool(left == right)),
            _ => panic!(),
        }
    }

    fn eval_binary_expr(
        &self,
        left: &Expr,
        binary_op_and_token: &OpWithToken<BinaryOp>,
        right: &Expr,
    ) -> InterpreterResult {
        let eval_left = self.eval_expr(left)?;
        let eval_right = self.eval_expr(right)?;

        self.binary_op(&eval_left, binary_op_and_token, &eval_right)
    }

    fn eval_expr(&self, expr: &Expr) -> InterpreterResult {
        use Expr::*;
        match expr {
            Binary(left, op_and_token, right) => self.eval_binary_expr(left, op_and_token, right),
            Unary(_op_and_token, _right) => {
                unimplemented!("eval of unary expression not implemented yet")
            }
            Grouping(new_expr) => self.eval_expr(new_expr),
            Literal(value_and_token) => Ok(value_and_token.clone().0),
            _ => unimplemented!("Logical expressions not implemented yet"),
        }
    }

    fn eval(&self, stmt: &Stmt) -> Result<String, Error> {
        use Stmt::*;
        match stmt {
            ExprStmt(expr) => match self.eval_expr(expr) {
                Ok(value) => Ok(value.to_string()),
                Err(error) => Err(Error::Runtime(error)),
            },
        }
    }

    pub fn interpret(&mut self, stmts: &Vec<Stmt>) -> Result<Vec<String>, Error> {
        let mut results: Vec<String> = vec![];

        for stmt in stmts {
            results.push(self.eval(stmt)?);
        }

        Ok(results)
    }

    pub fn new() -> Self {
        Self {}
    }
}
