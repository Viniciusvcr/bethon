use crate::error::{Error, ScannerError};
use crate::token::{NumberType, Token, TokenType};

pub struct Scanner<'a> {
    start: usize,
    current_char: usize,
    current_line: usize,
    source_code: &'a str,
    tokens: Vec<Token>,
}

impl<'a> Scanner<'a> {
    pub fn new(source_code: &'a str) -> Self {
        Self {
            start: 0,
            current_char: 0,
            current_line: 1,
            source_code,
            tokens: vec![],
        }
    }
    fn is_at_end(&self) -> bool {
        self.current_char >= self.source_code.len()
    }

    fn advance(&mut self) -> Result<char, Error> {
        if let Some(c) = self.source_code.chars().nth(self.current_char) {
            self.current_char += 1;
            Ok(c)
        } else {
            Err(Error::UnexpectedFail)
        }
    }

    fn source_substring(&self, start: usize, finish: usize) -> &str {
        &self.source_code[start..finish]
    }

    fn add_token(&mut self, tt: TokenType) {
        // TODO starts_at and ends_at
        self.tokens.push(Token::new(tt, self.current_line, 0, 0));
    }

    fn match_char(&mut self, expected: char) -> Result<bool, Error> {
        if let Some(c) = self.source_code.chars().nth(self.current_char) {
            if self.is_at_end() {
                Ok(false)
            } else if c != expected {
                Ok(false)
            } else {
                self.advance()?;
                Ok(true)
            }
        } else {
            // TODO change to Error::UnexpectedFail
            panic!("Unexpected fail: Something went wrong while parsing the file");
        }
    }
    fn is_digit(&self, c: char) -> bool {
        c >= '0' && c <= '9'
    }

    fn is_alpha(&self, c: char) -> bool {
        (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'
    }

    fn is_alphanumeric(&self, c: char) -> bool {
        self.is_alpha(c) || self.is_digit(c)
    }

    fn newline(&mut self) -> TokenType {
        self.current_line += 1;

        TokenType::Newline
    }

    fn peek_char(&self) -> char {
        if self.is_at_end() {
            '\0'
        } else {
            match self.source_code.chars().nth(self.current_char) {
                Some(c) => c,
                // TODO change to Error::UnexpectedFail
                None => panic!("Lexer::match_char failed"),
            }
        }
    }

    fn peek_next_char(&self) -> char {
        if self.is_at_end() {
            '\0'
        } else {
            match self.source_code.chars().nth(self.current_char + 1) {
                Some(c) => c,
                // TODO change to Error::UnexpectedFail
                None => panic!("Lexer::match_char failed"),
            }
        }
    }

    fn comment(&mut self) -> Result<TokenType, Error> {
        self.advance()?;
        while self.peek_char() != '\n' && !self.is_at_end() {
            self.advance()?;
        }

        Ok(TokenType::Comment)
    }

    fn string(&mut self) -> Result<TokenType, Error> {
        let starting_line = self.current_line;
        while self.peek_char() != '"' && !self.is_at_end() {
            if self.peek_char() == '\n' {
                self.current_line += 1;
            }

            self.advance()?;
        }

        if self.is_at_end() {
            return Err(Error::Scanner(ScannerError::UnterminatedString(
                starting_line,
            )));
        } else {
            self.advance()?;
        }

        let string_value = self.source_substring(self.start + 1, self.current_char - 1);
        Ok(TokenType::String(String::from(string_value)))
    }

    fn number(&mut self) -> Result<TokenType, Error> {
        while self.is_digit(self.peek_char()) {
            self.advance()?;
        }

        // Look for a fractional part
        if self.peek_char() == '.' && self.is_digit(self.peek_next_char()) {
            self.advance()?;
            while self.is_digit(self.peek_char()) {
                self.advance()?;
            }

            if self.peek_char() == '.' {
                self.advance()?;
                while self.is_digit(self.peek_char()) {
                    self.advance()?;
                }
                return Err(Error::Scanner(ScannerError::InvalidToken(
                    self.current_line,
                    0,
                    0,
                    format!(
                        "Failed parsing number {}",
                        self.source_substring(self.start, self.current_char)
                    ),
                )));
            }
        } else if self.peek_char() == ',' {
            return Err(Error::Scanner(ScannerError::InvalidToken(
                self.current_line,
                0,
                0,
                "Numbers are formatted with '.' instead of ','".to_string(),
            )));
        }

        let str_number = self.source_substring(self.start, self.current_char);
        if let Ok(number) = str_number.parse::<isize>() {
            Ok(TokenType::Number(NumberType::Integer(number)))
        } else if let Ok(number) = str_number.parse::<f64>() {
            Ok(TokenType::Number(NumberType::Float(number)))
        } else {
            Err(Error::Scanner(ScannerError::InvalidToken(
                self.current_line,
                0,
                0,
                format!("Failed parsing number {}", str_number),
            )))
        }
    }

    fn is_keyword(&self, token: &str) -> Option<TokenType> {
        use TokenType::*;

        match token {
            "and" => Some(And),
            "class" => Some(Class),
            "false" => Some(False),
            "true" => Some(True),
            "def" => Some(Def),
            "if" => Some(If),
            "elif" => Some(Elif),
            "else" => Some(Else),
            "None" => Some(PythonNone),
            "or" => Some(Or),
            "not" => Some(Not),
            "print" => Some(Print),
            "return" => Some(Return),
            "self" => Some(PythonSelf),
            "int" => Some(Int),
            "float" => Some(Float),
            "str" => Some(Str),
            "bool" => Some(Bool),
            _ => None,
        }
    }

    fn identifier(&mut self) -> Result<TokenType, Error> {
        while self.is_alphanumeric(self.peek_char()) {
            self.advance()?;
        }

        // Check if the text is a reserved word
        let text = self.source_substring(self.start, self.current_char);
        if let Some(tt) = self.is_keyword(&text) {
            Ok(tt)
        } else {
            Ok(TokenType::Identifier(String::from(text)))
        }
    }

    fn scan_token(&mut self) -> Result<TokenType, Error> {
        use TokenType::*;
        let c = self.advance()?;

        match c {
            '(' => Ok(LeftParen),
            ')' => Ok(RightParen),
            '[' => Ok(LeftSqBracket),
            ']' => Ok(RightSqBracket),
            ',' => Ok(Comma),
            '.' => Ok(Dot),
            '-' => match self.match_char('>') {
                Ok(true) => Ok(Arrow),
                Ok(false) => Ok(Minus),
                Err(err) => Err(err),
            },
            '+' => Ok(Plus),
            '/' => Ok(Slash),
            '*' => Ok(Star),
            '=' => match self.match_char('=') {
                Ok(true) => Ok(EqualEqual),
                Ok(false) => Ok(Equal),
                Err(err) => Err(err),
            },
            '<' => match self.match_char('=') {
                Ok(true) => Ok(LessEqual),
                Ok(false) => Ok(Less),
                Err(err) => Err(err),
            },
            '>' => match self.match_char('=') {
                Ok(true) => Ok(GreaterEqual),
                Ok(false) => Ok(Greater),
                Err(err) => Err(err),
            },
            ':' => Ok(Colon),
            '#' => self.comment(),
            ' ' | '\t' => Ok(Space),
            '\r' => Ok(Blank),
            '\n' => Ok(self.newline()),
            '"' => self.string(),
            _ => {
                if self.is_digit(c) {
                    self.number()
                } else if self.is_alpha(c) {
                    self.identifier()
                } else {
                    // FIXME counters
                    Err(Error::Scanner(ScannerError::InvalidToken(
                        self.current_line,
                        0,
                        0,
                        "".to_string(),
                    )))
                }
            }
        }
    }

    pub fn scan_tokens(&mut self) -> Result<&Vec<Token>, Error> {
        use TokenType::*;
        while !self.is_at_end() {
            self.start = self.current_char;
            match self.scan_token() {
                Ok(token) => match token {
                    Newline | Blank | Comment => (),
                    _ => self.add_token(token),
                },
                Err(error) => return Err(error),
            }
        }

        self.add_token(Eof);
        Ok(&self.tokens)
    }
}
