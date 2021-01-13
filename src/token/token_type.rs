use super::number_type::NumberType;

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    // Not interpret, spaces ou markers
    Comment,
    Newline,
    Blank,
    Indent,
    Deindent,
    Eof,

    // Types
    Bool,
    Int,
    Float,
    Str,

    // Literals
    Identifier,
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
    BangEqual,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Keywords
    Assert,
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