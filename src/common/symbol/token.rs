use super::token_type::TokenType;

#[derive(Debug, Clone, PartialEq, Default)]
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

    pub fn as_tuple(&self) -> (usize, usize, usize) {
        (self.line, self.starts_at, self.ends_at)
    }
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Token {
    pub tt: TokenType,
    pub lexeme: String,
    pub placement: Placement,
}

impl Token {
    pub fn tt(&self) -> &TokenType {
        &self.tt
    }

    pub fn placement(&self) -> &Placement {
        &self.placement
    }

    pub fn lexeme(&self) -> String {
        self.lexeme.clone()
    }

    pub fn new(
        tt: TokenType,
        lexeme: String,
        line: usize,
        starts_at: usize,
        ends_at: usize,
    ) -> Self {
        let placement = Placement::new(line, starts_at, ends_at);
        Self {
            tt,
            lexeme,
            placement,
        }
    }
}
