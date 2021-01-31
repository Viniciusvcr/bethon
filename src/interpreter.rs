use crate::{
    environment::Environment,
    error::{runtime::RuntimeError, Error},
    expr::{operations::*, value::Value, *},
    stmt::*,
    token::number_type::NumberType,
};
use callable::Callable;
use num_traits::ToPrimitive;

pub type InterpreterResult = std::result::Result<Value, RuntimeError>;

#[derive(Default)]
pub struct Interpreter {
    environment: Environment<Value>,
}

impl Interpreter {
    fn with_new_env<T>(&mut self, fun: impl Fn(&mut Self) -> T) -> T {
        self.environment.push();
        let result = fun(self);
        self.environment.pop();
        result
    }

    fn binary_arith_op(
        &self,
        left: &Value,
        op_and_token: &OpWithToken<BinaryOp>,
        right: &Value,
    ) -> InterpreterResult {
        use BinaryOp::*;
        use Value::*;

        let (op, token) = op_and_token.as_tuple();

        match (op, left, right) {
            (Sub, Number(a), Number(b)) => Ok(Number(a.clone() - b.clone())),
            (Add, Number(a), Number(b)) => Ok(Number(a.clone() + b.clone())),
            (Div, Number(NumberType::Integer(a)), Number(NumberType::Integer(b))) => {
                if *b != num_bigint::BigInt::from(0) {
                    Ok(Value::Number(NumberType::Integer(a / b)))
                } else {
                    Err(RuntimeError::DivisionByZero(
                        token.placement.line,
                        token.placement.starts_at,
                        token.placement.ends_at,
                    ))
                }
            }
            (Div, Number(NumberType::Float(a)), Number(NumberType::Integer(b))) => {
                if *b != num_bigint::BigInt::from(0) {
                    Ok(Value::Number(NumberType::Float(a / b.to_f64().unwrap())))
                } else {
                    Err(RuntimeError::DivisionByZero(
                        token.placement.line,
                        token.placement.starts_at,
                        token.placement.ends_at,
                    ))
                }
            }
            (Div, Number(NumberType::Integer(a)), Number(NumberType::Float(b))) => {
                if *b != 0.0 {
                    Ok(Value::Number(NumberType::Float(a.to_f64().unwrap() / b)))
                } else {
                    Err(RuntimeError::DivisionByZero(
                        token.placement.line,
                        token.placement.starts_at,
                        token.placement.ends_at,
                    ))
                }
            }
            (Div, Number(NumberType::Float(a)), Number(NumberType::Float(b))) => {
                if *b != 0.0 {
                    Ok(Value::Number(NumberType::Float(a / b)))
                } else {
                    Err(RuntimeError::DivisionByZero(
                        token.placement.line,
                        token.placement.starts_at,
                        token.placement.ends_at,
                    ))
                }
            }
            (Mul, Number(a), Number(b)) => Ok(Number(a.clone() * b.clone())),
            (Mod, Number(a), Number(b)) => Ok(Number(a.clone() % b.clone())),
            _ => panic!("interpreter::binary_arith_op failed unexpectedly"),
        }
    }

    fn eval_binary_arith_expr(
        &mut self,
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

        match (op, right) {
            (Minus, Number(NumberType::Integer(a))) => Ok(Number(NumberType::Integer(-a.clone()))),
            (Minus, Number(NumberType::Float(a))) => Ok(Number(NumberType::Float(-*a))),
            (Plus, Number(NumberType::Integer(a))) => Ok(Number(NumberType::Integer(a.clone()))),
            (Plus, Number(NumberType::Float(a))) => Ok(Number(NumberType::Float(*a))),
            _ => panic!("interpreter::unary_op failed unexpectedly"),
        }
    }

    fn eval_logic_not(&mut self, expr: &Expr) -> InterpreterResult {
        let value = self.eval_expr(expr)?;

        let evaluated_value = match value {
            Value::Bool(a) => Value::Bool(!a),
            _ => panic!("interpreter::eval_logic_not failed unexpectedly"),
        };

        Ok(evaluated_value)
    }

    fn eval_unary_expr(
        &mut self,
        op_and_token: &OpWithToken<UnaryOp>,
        right: &Expr,
    ) -> InterpreterResult {
        let op = &op_and_token.op;
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
            // todo how to compare strings?
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
        &mut self,
        left: &Expr,
        op_and_token: &OpWithToken<BinaryCompOp>,
        right: &Expr,
    ) -> InterpreterResult {
        let (op, _token) = op_and_token.as_tuple();
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
        let evaluated_value = match (op, left, right) {
            (BinaryLogicOp::And, Value::Bool(a), Value::Bool(b)) => Value::Bool(*a && *b),
            (BinaryLogicOp::Or, Value::Bool(a), Value::Bool(b)) => Value::Bool(*a || *b),
            _ => panic!("interpreter::binary_logic_op failed unexpectedly"),
        };

        Ok(evaluated_value)
    }

    fn eval_binary_logic_expr(
        &mut self,
        left: &Expr,
        op_and_token: &OpWithToken<BinaryLogicOp>,
        right: &Expr,
    ) -> InterpreterResult {
        let op = &op_and_token.op;
        let eval_left = self.eval_expr(left)?;
        let eval_right = self.eval_expr(right)?;

        self.binary_logic_op(&eval_left, op, &eval_right)
    }

    fn eval_var_expr(&self, id: &str) -> Value {
        self.environment.get(id).unwrap()
    }

    fn eval_call_expr(&mut self, callee: &Expr, args: &[Expr]) -> InterpreterResult {
        if let Value::Fun(fun) = self.eval_expr(callee)? {
            let mut eval_args = vec![];

            for expr in args {
                eval_args.push(self.eval_expr(expr)?);
            }

            fun.call(self, &eval_args)
        } else {
            panic!("not callable")
        }
    }

    fn eval_expr(&mut self, expr: &Expr) -> InterpreterResult {
        match expr {
            Expr::BinaryArith(left, op_and_token, right) => {
                self.eval_binary_arith_expr(left, op_and_token, right)
            }
            Expr::BinaryComp(left, op_and_token, right) => {
                self.eval_binary_comp_expr(left, op_and_token, right)
            }
            Expr::BinaryLogic(left, op_and_token, right) => {
                self.eval_binary_logic_expr(left, op_and_token, right)
            }
            Expr::LogicNot((expr, _token)) => self.eval_logic_not(expr),
            Expr::Unary(op_and_token, right) => self.eval_unary_expr(op_and_token, right),
            Expr::Grouping(new_expr) => self.eval_expr(new_expr),
            Expr::Literal(value_and_token) => Ok(value_and_token.clone().op),
            Expr::Variable(token) => Ok(self.eval_var_expr(&token.lexeme())),
            Expr::Call(callee, args) => self.eval_call_expr(callee, args),
        }
    }

    fn eval_var_stmt(&mut self, identifier: &str, expr: &Expr) -> Result<(), RuntimeError> {
        let value = self.eval_expr(expr)?;

        self.environment.define(identifier.to_string(), value);

        Ok(())
    }

    // todo Change 'val' to the result of the expression
    fn assert_eval(&mut self, expr: &Expr) -> Result<(), RuntimeError> {
        match expr {
            Expr::BinaryComp(left, op_and_token, right) => {
                let value = self.eval_binary_comp_expr(left, op_and_token, right);

                match value {
                    Ok(val) if val != Value::Bool(true) => Err(RuntimeError::CompAssertionFailed(
                        op_and_token.get_token_line(),
                        format!("{}", left),
                        format!("{}", right),
                        op_and_token.op,
                        val,
                    )),
                    Err(error) => Err(error),
                    _ => Ok(()),
                }
            }
            _ => match self.eval_expr(expr) {
                Ok(val) if val != Value::Bool(true) => {
                    Err(RuntimeError::AssertionFailed(expr.get_line()))
                }
                Err(error) => Err(error),
                _ => Ok(()),
            },
        }
    }

    pub fn eval_func(&mut self, fun: &Callable, args: &[Value]) -> InterpreterResult {
        let old_env = std::mem::replace(&mut self.environment, fun.env.clone());

        let result = self.with_new_env(|interpreter| {
            for ((token, _), value) in fun.params.iter().zip(args) {
                interpreter
                    .environment
                    .define(token.lexeme(), value.clone());
            }

            for stmt in &fun.body {
                match interpreter.eval(stmt) {
                    Err(RuntimeError::Return(value)) => return Ok(value),
                    Err(x) => return Err(x),
                    Ok(()) => {}
                }
            }

            Ok(Value::PythonNone)
        });
        self.environment = old_env;

        result
    }

    fn eval(&mut self, stmt: &Stmt) -> Result<(), RuntimeError> {
        match stmt {
            Stmt::Assert(expr) => self.assert_eval(expr),
            Stmt::ExprStmt(expr) => match self.eval_expr(expr) {
                Ok(value) => {
                    println!("{}", value);
                    Ok(())
                }
                Err(error) => Err(error),
            },
            Stmt::VarStmt(token, _var_type, expr) => {
                match self.eval_var_stmt(&token.lexeme(), expr) {
                    Ok(()) => Ok(()),
                    Err(error) => Err(error),
                }
            }
            Stmt::IfStmt(condition, then_branch, else_branch) => match self.eval_expr(condition) {
                Ok(evalued_condition) => {
                    if evalued_condition == Value::Bool(true) {
                        for then_stmt in then_branch {
                            self.eval(then_stmt)?;
                        }

                        Ok(())
                    } else if else_branch.is_some() {
                        for else_stmt in else_branch.as_ref().unwrap() {
                            self.eval(else_stmt)?;
                        }

                        Ok(())
                    } else {
                        Ok(())
                    }
                }
                Err(error) => Err(error),
            },
            Stmt::Function(fun_id, params, fun_body, ret_type) => {
                self.environment.define(
                    fun_id.lexeme(),
                    Value::Fun(Callable::new(
                        self.environment.clone(),
                        fun_id.clone(),
                        params.clone(),
                        fun_body.clone(),
                        *ret_type,
                    )),
                );

                Ok(())
            }
            Stmt::ReturnStmt(_, expr) => Err(RuntimeError::Return(if let Some(expr) = expr {
                self.eval_expr(expr)?
            } else {
                Value::PythonNone
            })),
        }
    }

    pub fn interpret(&mut self, stmts: &[Stmt]) -> Option<Error> {
        self.environment = Environment::default();

        for stmt in stmts {
            if let Err(evaluation) = self.eval(stmt) {
                return Some(Error::Runtime(evaluation));
            }
        }

        None
    }
}
