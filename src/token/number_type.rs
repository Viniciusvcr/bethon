use num_bigint::BigInt;
use num_traits::cast::ToPrimitive;

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum NumberType {
    Float(f64),
    Integer(BigInt),
}

impl std::ops::Sub for NumberType {
    type Output = Self;
    fn sub(self, rhs: Self) -> Self::Output {
        use NumberType::*;
        match self {
            Float(a) => match rhs {
                Float(b) => Float(a - b),
                Integer(b) => Float(a - b.to_f64().unwrap()),
            },
            Integer(a) => match rhs {
                Float(b) => Float(a.to_f64().unwrap() - b),
                Integer(b) => Integer(a - b),
            },
        }
    }
}

impl std::ops::Add for NumberType {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        use NumberType::*;
        match self {
            Float(a) => match rhs {
                Float(b) => Float(a + b),
                Integer(b) => Float(a + b.to_f64().unwrap()),
            },
            Integer(a) => match rhs {
                Float(b) => Float(a.to_f64().unwrap() + b),
                Integer(b) => Integer(a + b),
            },
        }
    }
}

impl std::ops::Mul for NumberType {
    type Output = Self;
    fn mul(self, rhs: Self) -> Self::Output {
        use NumberType::*;
        match self {
            Float(a) => match rhs {
                Float(b) => Float(a * b),
                Integer(b) => Float(a * b.to_f64().unwrap()),
            },
            Integer(a) => match rhs {
                Float(b) => Float(a.to_f64().unwrap() * b),
                Integer(b) => Integer(a * b),
            },
        }
    }
}

impl std::ops::Div for NumberType {
    type Output = Self;
    fn div(self, rhs: Self) -> Self::Output {
        use NumberType::*;
        match self {
            Float(a) => match rhs {
                Float(b) => Float(a / b),
                Integer(b) => Float(a / b.to_f64().unwrap()),
            },
            Integer(a) => match rhs {
                Float(b) => Float(a.to_f64().unwrap() / b),
                Integer(b) => Integer(a / b),
            },
        }
    }
}

impl std::ops::Rem for NumberType {
    type Output = Self;
    fn rem(self, rhs: Self) -> Self::Output {
        use NumberType::*;
        match self {
            Float(a) => match rhs {
                Float(b) => Float(a % b),
                Integer(b) => Float(a % b.to_f64().unwrap()),
            },
            Integer(a) => match rhs {
                Float(b) => Float(a.to_f64().unwrap() % b),
                Integer(b) => Integer(a % b),
            },
        }
    }
}

impl std::fmt::Display for NumberType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            NumberType::Float(n) => write!(f, "{}", n),
            NumberType::Integer(n) => write!(f, "{}", n),
        }
    }
}
