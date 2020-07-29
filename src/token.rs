#[derive(Debug)]
pub enum NumberType {
    Float(f64),
    Integer(isize), // FIXME change to bigint
}

#[derive(Debug)]
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

#[derive(Debug)]
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

#[derive(Debug)]
pub struct Token {
    tt: TokenType,
    placement: Placement,
}

impl Token {
    pub fn new(tt: TokenType, line: usize, starts_at: usize, ends_at: usize) -> Self {
        let placement = Placement::new(line, starts_at, ends_at);
        Self { tt, placement }
    }
}
