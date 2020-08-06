use crate::token::{NumberType, Token};

pub type OpWithToken<Op> = (Op, Token);

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
    pub fn to_string(&self) -> String {
        use Value::*;

        match self {
            PythonNone => "None".to_string(),
            Bool(false) => format!("False"),
            Bool(true) => format!("True"),
            Number(value) => format!("{}", value),
            Str(value) => value.to_string(),
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum Expr {
    Binary(Box<Expr>, OpWithToken<BinaryOp>, Box<Expr>), // TODO: separar logico, aritmetica e comparação
    Unary(OpWithToken<UnaryOp>, Box<Expr>),
    Grouping(Box<Expr>),
    Literal(OpWithToken<Value>),
}

// struct AnaliseSemantica {
//     types: std::collections::HashMap<Expr, Value>,
// }

// impl AnaliseSemantica {
//     // fn type_(&self, exp: &Expr) -> Type {
//     //     self.types[exp].uwnrap()
//     // }

//     fn analise(&mut self, exp: &Expr) -> Result<Type, Error> {
//         // pos condição: associar um tipo com exp ou retorna erro se tem um erro de tipo
//         use Expr::*;
//         match exp {
//             Binary(a, op, b) => {
//                 let type_a = self.analise(a)?;
//                 let type_b = self.analise(a)?;

//                 let type_ = match (type_a, op, type_b) {
//                     (Number, Plus, Number) => Number,
//                     _ => Error,
//                 };
//             }
//         }
//     }
// }

// fn add_number(a: NumberType, b: NumberType) -> NumberType {}
