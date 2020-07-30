use crate::token::{NumberType, Token};

type OpWithToken<Op> = (Op, Token);

#[derive(Clone, PartialEq, Debug)]
pub enum UnaryOp {
    Minus,
    Plus,
}

#[derive(Clone, PartialEq, Debug)]
pub enum BinaryOp {
    Equal,
    LessThan,
    LessEqual,
    GreaterThan,
    GreaterEqual,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

#[derive(Clone, PartialEq, Debug)]
pub enum LogicalOp {
    Not,
    And,
    Or,
}

#[derive(Clone, PartialEq, Debug)]
pub enum Value {
    PythonNone,
    Bool(bool),
    Number(NumberType),
    Str(String),
}

impl Value {
    pub fn show(&self) -> String {
        use Value::*;

        match self {
            PythonNone => "None".to_string(),
            Bool(value) => format!("{}", value),
            Number(value) => format!("{}", value),
            Str(value) => value.to_string(),
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum Expr {
    Binary(Box<Expr>, OpWithToken<BinaryOp>, Box<Expr>),
    Unary(OpWithToken<UnaryOp>, Box<Expr>),
    Grouping(Box<Expr>),
    Literal(OpWithToken<Value>),
    Logical(OpWithToken<LogicalOp>, Box<Expr>),
}

pub trait Visitor<T> {
    fn accept(&mut self, expr: &Expr) -> T {
        use Expr::*;

        match expr {
            Binary(left, (binary_op, token), right) => {
                self.visit_binary_expr(left, (binary_op, token), right)
            }
            Unary((unary_op, token), right) => self.visit_unary_expr((unary_op, token), right),
            Grouping(expr) => self.visit_grouping_expr(expr),
            Literal((value, token)) => self.visit_literal_expr((value, token)),
            Logical((logical_op, token), right) => {
                self.visit_logical_expr((logical_op, token), right)
            }
        }
    }

    fn visit_binary_expr(
        &mut self,
        left: &Expr,
        binary_op_and_token: (&BinaryOp, &Token),
        right: &Expr,
    ) -> T;
    fn visit_grouping_expr(&mut self, expr: &Expr) -> T;
    fn visit_literal_expr(&mut self, value_and_token: (&Value, &Token)) -> T;
    fn visit_unary_expr(&mut self, unary_op_and_token: (&UnaryOp, &Token), right: &Expr) -> T;
    fn visit_logical_expr(&mut self, logical_op_and_token: (&LogicalOp, &Token), right: &Expr)
        -> T;
}
