#[derive(Debug, Clone, PartialEq, Copy, PartialOrd)]
pub enum NumberType {
    Float(f64),
    Integer(isize), // FIXME change to bigint
}

impl std::ops::Sub for NumberType {
    type Output = Self;
    fn sub(self, rhs: Self) -> Self::Output {
        use NumberType::*;
        match self {
            Float(a) => match rhs {
                Float(b) => Float(a - b),
                Integer(b) => Float(a - b as f64),
            },
            Integer(a) => match rhs {
                Float(b) => Float(a as f64 - b),
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
                Integer(b) => Float(a + b as f64),
            },
            Integer(a) => match rhs {
                Float(b) => Float(a as f64 + b),
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
                Integer(b) => Float(a * b as f64),
            },
            Integer(a) => match rhs {
                Float(b) => Float(a as f64 * b),
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
                Integer(b) => Float(a / b as f64),
            },
            Integer(a) => match rhs {
                Float(b) => Float(a as f64 / b),
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
                Integer(b) => Float(a % b as f64),
            },
            Integer(a) => match rhs {
                Float(b) => Float(a as f64 % b),
                Integer(b) => Integer(a % b),
            },
        }
    }
}

impl std::ops::Neg for NumberType {
    type Output = Self;
    fn neg(self) -> Self::Output {
        match self {
            NumberType::Integer(a) => NumberType::Integer(-a),
            NumberType::Float(a) => NumberType::Float(-a),
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

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    // Not interpret, spaces ou markers
    Comment,
    Newline,
    Blank,
    Space,
    Eof,

    // Types
    Bool,
    Int,
    Float,
    Str,

    // Literals
    Identifier(String),
    String(String),
    Number(NumberType),

    LeftParen,
    RightParen,
    LeftSqBracket,
    RightSqBracket,
    Colon,
    Comma,
    Dot,
    Equal,
    Minus,
    Plus,
    Slash,
    Mod,
    Star,
    Arrow,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Keywords
    Class,
    True,
    False,
    PythonNone,
    If,
    Elif,
    Else,
    Return,
    Def,
    PythonSelf,
    Print,

    // Logical Operators
    Not,
    Or,
    And,
}

#[derive(Debug, Clone, PartialEq)]
// Abstracts the data of the position of a Token on the source code
pub struct Placement {
    pub line: usize,
    pub starts_at: usize,
    pub ends_at: usize,
}

impl Placement {
    pub fn new(line: usize, starts_at: usize, ends_at: usize) -> Self {
        Self {
            line,
            starts_at,
            ends_at,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    tt: TokenType,
    placement: Placement,
}

impl Token {
    pub fn tt(&self) -> &TokenType {
        &self.tt
    }

    pub fn placement(&self) -> &Placement {
        &self.placement
    }

    pub fn new(tt: TokenType, line: usize, starts_at: usize, ends_at: usize) -> Self {
        let placement = Placement::new(line, starts_at, ends_at);
        Self { tt, placement }
    }
}
