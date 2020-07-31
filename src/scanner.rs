use crate::error::{Error, ScannerError};
use crate::token::{NumberType, Token, TokenType};
use std::str::Chars;

pub fn create_code_vec(source_code: &str) -> std::vec::Vec<String> {
    let source_code_len = source_code.chars().count();

    let mut line: String = "".to_string();
    if source_code_len > 0 {
        let mut vec_lines: std::vec::Vec<String> = vec![];
        for c in source_code.chars() {
            if c == '\n' {
                vec_lines.push(line.clone());
                line = "".to_string();
            } else {
                line.push(c);
            }
        }
        if line != "" {
            vec_lines.push(line.clone());
        }
        vec_lines
    } else {
        vec![]
    }
}

pub struct Scanner<'a> {
    start_token: usize,
    end_token: usize,
    current_line: usize,
    source_code: &'a str,
    chars: Chars<'a>,
    tokens: Vec<Token>,
}

impl<'a> Scanner<'a> {
    pub fn new(source_code: &'a str) -> Self {
        Self {
            current_line: 1,
            source_code,
            start_token: 0,
            end_token: 0,
            chars: source_code.chars(),
            tokens: vec![],
        }
    }

    fn take_while(&mut self, fun: impl Fn(char) -> bool) {
        while self.peek().map(&fun).unwrap_or(false) {
            self.advance();
        }
    }

    fn consumed(&self) -> &'a str {
        let mut iter = self.source_code.chars();
        let mut len = 0;
        while !std::ptr::eq(iter.as_str(), self.chars.as_str()) {
            len += iter.next().map(char::len_utf8).unwrap_or(0);
        }
        &self.source_code[..len]
    }

    fn is_at_end(&self) -> bool {
        self.chars.clone().next().is_none()
    }

    fn advance(&mut self) -> Option<char> {
        self.end_token += 1;
        self.chars.next()
    }

    fn add_token(&mut self, tt: TokenType) {
        self.tokens.push(Token::new(
            tt,
            self.current_line,
            self.start_token,
            self.end_token,
        ));
    }

    fn match_char(&mut self, expected: char) -> Result<bool, Error> {
        if let Some(c) = self.peek() {
            if self.is_at_end() || c != expected {
                Ok(false)
            } else {
                self.advance();
                Ok(true)
            }
        } else {
            Err(Error::UnexpectedFail)
        }
    }

    fn newline(&mut self) -> TokenType {
        self.current_line += 1;
        // self.source_code = self.chars.as_str();

        TokenType::Newline
    }

    fn peek(&mut self) -> Option<char> {
        self.chars.clone().next()
    }

    fn comment(&mut self) -> Result<TokenType, Error> {
        self.take_while(|c| c != '\n');

        Ok(TokenType::Comment)
    }

    fn string(&mut self) -> Result<TokenType, Error> {
        let starting_line = self.current_line;
        self.take_while(|ch| ch != '"');
        if self.peek() == Some('"') {
            let string = TokenType::String(self.consumed()[1..].into());
            self.advance();
            Ok(string)
        } else {
            Err(Error::Scanner(ScannerError::UnterminatedString(
                starting_line,
            )))
        }
    }

    fn number(&mut self) -> Result<TokenType, Error> {
        self.take_while(is_digit);
        if Some('.') == self.peek() {
            self.advance();
            if is_digit(self.peek().unwrap_or(' ')) {
                self.take_while(is_digit);
                if Some('.') == self.peek() {
                    self.advance();
                    self.take_while(is_digit);
                    return Err(Error::Scanner(ScannerError::InvalidToken(
                        self.current_line,
                        self.start_token,
                        self.end_token,
                        format!("Failed parsing number {}", &self.consumed()),
                    )));
                } else {
                    Ok(TokenType::Number(NumberType::Float(
                        self.consumed().parse::<f64>().unwrap(),
                    )))
                }
            } else {
                Err(Error::Scanner(ScannerError::InvalidToken(
                    self.current_line,
                    self.start_token,
                    self.end_token,
                    format!("Failed parsing number {}", &self.consumed()),
                )))
            }
        } else {
            Ok(TokenType::Number(NumberType::Integer(
                self.consumed().parse::<isize>().unwrap(),
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
        self.take_while(is_alphanumeric);

        // Check if the text is a reserved word
        let text = self.consumed();
        if let Some(tt) = self.is_keyword(&text) {
            Ok(tt)
        } else {
            Ok(TokenType::Identifier(String::from(text)))
        }
    }

    fn scan_token(&mut self) -> Result<TokenType, Error> {
        use TokenType::*;
        let c = self.advance();

        match c {
            Some('(') => Ok(LeftParen),
            Some(')') => Ok(RightParen),
            Some('[') => Ok(LeftSqBracket),
            Some(']') => Ok(RightSqBracket),
            Some(',') => Ok(Comma),
            Some('.') => Ok(Dot),
            Some('-') => match self.match_char('>') {
                Ok(true) => Ok(Arrow),
                Ok(false) => Ok(Minus),
                Err(err) => Err(err),
            },
            Some('%') => Ok(Mod),
            Some('+') => Ok(Plus),
            Some('/') => Ok(Slash),
            Some('*') => Ok(Star),
            Some('=') => match self.match_char('=') {
                Ok(true) => Ok(EqualEqual),
                Ok(false) => Ok(Equal),
                Err(err) => Err(err),
            },
            Some('<') => match self.match_char('=') {
                Ok(true) => Ok(LessEqual),
                Ok(false) => Ok(Less),
                Err(err) => Err(err),
            },
            Some('>') => match self.match_char('=') {
                Ok(true) => Ok(GreaterEqual),
                Ok(false) => Ok(Greater),
                Err(err) => Err(err),
            },
            Some(':') => Ok(Colon),
            Some('#') => self.comment(),
            Some(' ') | Some('\t') => Ok(Space),
            Some('\r') => Ok(Blank),
            Some('\n') => Ok(self.newline()),
            Some('"') => self.string(),
            Some(_) => {
                if is_digit(c.unwrap()) {
                    self.number()
                } else if is_alpha(c.unwrap()) {
                    self.identifier()
                } else {
                    Err(Error::Scanner(ScannerError::InvalidToken(
                        self.current_line,
                        self.start_token,
                        self.end_token,
                        "Invalid character".to_string(),
                    )))
                }
            }
            None => panic!(),
        }
    }

    pub fn scan_tokens(&mut self) -> Result<&Vec<Token>, Error> {
        use TokenType::*;
        while !self.is_at_end() {
            self.start_token = self.end_token;
            match self.scan_token() {
                Ok(token) => match token {
                    Newline => {
                        self.start_token = 0;
                        self.end_token = 0
                    }
                    Space | Blank | Comment => (), // FIXME Spaces will be used in identation
                    _ => self.add_token(token),
                },
                Err(error) => return Err(error),
            }
            self.source_code = self.chars.as_str();
        }
        self.add_token(Eof);
        Ok(&self.tokens)
    }
}

fn is_digit(c: char) -> bool {
    c >= '0' && c <= '9'
}

fn is_alpha(c: char) -> bool {
    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'
}

fn is_alphanumeric(c: char) -> bool {
    is_digit(c) || is_alpha(c)
}
