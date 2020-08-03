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

    fn advance(&mut self) -> Option<char> {
        if let Some(ch) = self.chars.next() {
            self.end_token += 1;
            Some(ch)
        } else {
            None
        }
    }

    fn add_token(&mut self, tt: TokenType) {
        self.tokens.push(Token::new(
            tt,
            self.current_line,
            self.start_token,
            self.end_token,
        ));
    }

    fn match_char(&mut self, expected: char) -> bool {
        if Some(expected) == self.peek() {
            self.advance();
            true
        } else {
            false
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

    fn comment(&mut self) -> TokenType {
        self.take_while(|c| c != '\n');

        TokenType::Comment
    }

    fn string(&mut self) -> Option<TokenType> {
        self.take_while(|ch| ch != '"');
        if self.peek() == Some('"') {
            let string = TokenType::String(self.consumed()[1..].into());
            self.advance();
            Some(string)
        } else {
            None
        }
    }

    fn number(&mut self) -> Option<TokenType> {
        self.take_while(is_digit);
        if Some('.') == self.peek() {
            self.advance();
            if is_digit(self.peek().unwrap_or(' ')) {
                self.take_while(is_digit);
                if Some('.') == self.peek() {
                    self.advance();
                    self.take_while(is_digit);
                    None
                } else {
                    Some(TokenType::Number(NumberType::Float(
                        self.consumed().parse::<f64>().unwrap(),
                    )))
                }
            } else {
                None
            }
        } else {
            Some(TokenType::Number(NumberType::Integer(
                self.consumed().parse::<isize>().unwrap(),
            )))
        }
    }

    fn is_keyword(&self, token: &str) -> Option<TokenType> {
        use TokenType::*;

        match token {
            "and" => Some(And),
            "class" => Some(Class),
            "False" => Some(False),
            "True" => Some(True),
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

    fn identifier(&mut self) -> TokenType {
        self.take_while(is_alphanumeric);

        // Check if the text is a reserved word
        let text = self.consumed();
        if let Some(tt) = self.is_keyword(&text) {
            tt
        } else {
            TokenType::Identifier(String::from(text))
        }
    }

    fn match_or_else(&mut self, expected: char, tt: TokenType, default: TokenType) -> TokenType {
        if self.match_char(expected) {
            tt
        } else {
            default
        }
    }

    fn scan_token(&mut self) -> Result<Option<TokenType>, Error> {
        use TokenType::*;

        if let Some(c) = self.advance() {
            let token = match c {
                '(' => LeftParen,
                ')' => RightParen,
                '[' => LeftSqBracket,
                ']' => RightSqBracket,
                ',' => Comma,
                '.' => Dot,
                '-' => self.match_or_else('>', TokenType::Arrow, TokenType::Minus),
                '%' => Mod,
                '+' => Plus,
                '/' => Slash,
                '*' => Star,
                '=' => self.match_or_else('=', TokenType::EqualEqual, TokenType::Equal),
                '<' => self.match_or_else('<', TokenType::LessEqual, TokenType::Less),
                '>' => self.match_or_else('>', TokenType::GreaterEqual, TokenType::Greater),
                ':' => Colon,
                '#' => self.comment(),
                ' ' | '\t' => Space,
                '\r' => Blank,
                '\n' => self.newline(),
                '"' => {
                    if let Some(string) = self.string() {
                        string
                    } else {
                        return Err(Error::Scanner(ScannerError::UnterminatedString(
                            self.current_line,
                        )));
                    }
                }
                c => {
                    if is_digit(c) {
                        if let Some(number) = self.number() {
                            number
                        } else {
                            return Err(Error::Scanner(ScannerError::InvalidToken(
                                self.current_line,
                                self.start_token,
                                self.end_token,
                                format!("Failed parsing number {}", &self.consumed()),
                            )));
                        }
                    } else if is_alpha(c) {
                        self.identifier()
                    } else {
                        return Err(Error::Scanner(ScannerError::InvalidToken(
                            self.current_line,
                            self.start_token,
                            self.end_token,
                            "Invalid character".to_string(),
                        )));
                    }
                }
            };
            Ok(Some(token))
        } else {
            Ok(None)
        }
    }

    pub fn scan_tokens(&mut self) -> Result<&Vec<Token>, Error> {
        use TokenType::*;

        loop {
            self.start_token = self.end_token;

            if let Some(token) = self.scan_token()? {
                match token {
                    Newline => {
                        self.start_token = 0;
                        self.end_token = 0
                    }
                    Space | Blank | Comment => (), // FIXME Spaces will be used in identation
                    _ => self.add_token(token),
                }
            } else {
                break;
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
