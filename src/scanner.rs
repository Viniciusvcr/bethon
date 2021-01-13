use crate::error::{Error, ScannerError};
use crate::token::{number_type::NumberType, token_type::TokenType, Token};
use num_bigint::BigInt;
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
        if !line.is_empty() {
            vec_lines.push(line);
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
    current_indent_level: i32,
    space_indent: bool,
    tab_indent: bool,
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
            current_indent_level: 0,
            space_indent: false,
            tab_indent: false,
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

    fn add_token(&mut self, tt: TokenType, lexeme: String) {
        self.tokens.push(Token::new(
            tt,
            lexeme,
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

    // todo make level_const dynamic
    fn scan_indentation(&mut self) -> i32 {
        let mut spaces = 0;

        while let Some(ch) = self.peek() {
            if ch == '#' {
                self.comment();
            } else if ch == '\n' {
                spaces = 0;
                self.current_line += 1;
            } else if ch == ' ' {
                spaces += 1;
                self.space_indent = true;
            } else if ch == '\t' {
                spaces += 1;
                self.tab_indent = true;
            } else {
                break;
            }

            self.advance();
        }

        let level_const = if self.space_indent { 4 } else { 1 };

        spaces / level_const
    }

    fn indent_to(&mut self, indent_level: i32) {
        match indent_level.cmp(&self.current_indent_level) {
            std::cmp::Ordering::Greater => {
                for _ in 0..indent_level {
                    self.add_token(TokenType::Indent, self.consumed().to_string());
                }
            }
            std::cmp::Ordering::Less => {
                for _ in 0..self.current_indent_level - indent_level {
                    self.add_token(TokenType::Deindent, self.consumed().to_string());
                }
            }
            std::cmp::Ordering::Equal => (),
        }
    }

    fn newline(&mut self) -> Result<TokenType, ScannerError> {
        self.start_token = 0;
        self.end_token = 0;

        let line_indent_level = self.scan_indentation();
        self.indent_to(line_indent_level);
        self.current_line += 1;

        self.current_indent_level = line_indent_level;

        if self.tab_indent && self.space_indent {
            Err(ScannerError::MismatchedIdent(
                self.current_line,
                self.start_token,
                self.end_token,
            ))
        } else {
            Ok(TokenType::Newline)
        }
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
                self.consumed().parse::<BigInt>().unwrap(),
            )))
        }
    }

    fn get_keyword(&self, token: &str) -> Option<TokenType> {
        use TokenType::*;

        match token {
            "and" => Some(And),
            "assert" => Some(Assert),
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

    fn indentifier_or_keyword(&mut self) -> TokenType {
        self.take_while(is_alphanumeric);

        let text = self.consumed();
        self.get_keyword(&text).unwrap_or(TokenType::Identifier)
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
                '<' => self.match_or_else('=', TokenType::LessEqual, TokenType::Less),
                '>' => self.match_or_else('=', TokenType::GreaterEqual, TokenType::Greater),
                ':' => Colon,
                '#' => self.comment(),
                ' ' | '\r' => Blank,
                '\n' => match self.newline() {
                    Ok(newline) => newline,
                    Err(error) => return Err(Error::Scanner(error)),
                },
                '"' => {
                    if let Some(string) = self.string() {
                        string
                    } else {
                        return Err(Error::Scanner(ScannerError::UnterminatedString(
                            self.current_line,
                        )));
                    }
                }
                '!' => {
                    if self.match_char('=') {
                        BangEqual
                    } else {
                        return Err(Error::Scanner(ScannerError::LonelyBangSign(
                            self.current_line,
                            self.start_token,
                            self.end_token,
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
                        self.indentifier_or_keyword()
                    } else {
                        return Err(Error::Scanner(ScannerError::InvalidCharacter(
                            self.current_line,
                            self.start_token,
                            self.end_token,
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

        let initial_indent_level = self.scan_indentation();
        self.indent_to(initial_indent_level);
        self.current_indent_level = initial_indent_level;
        self.source_code = self.chars.as_str();

        loop {
            self.start_token = self.end_token;

            if let Some(token) = self.scan_token()? {
                match token {
                    Newline | Blank | Comment => (),
                    _ => self.add_token(token, self.consumed().to_string()),
                }
            } else {
                break;
            }
            self.source_code = self.chars.as_str();
        }

        let final_indent_level = self.scan_indentation();
        self.indent_to(final_indent_level);
        self.add_token(Eof, "EOF".to_string());
        Ok(&self.tokens)
    }
}

fn is_digit(c: char) -> bool {
    ('0'..='9').contains(&c)
}

fn is_alpha(c: char) -> bool {
    ('a'..='z').contains(&c) || ('A'..='Z').contains(&c) || c == '_'
}

fn is_alphanumeric(c: char) -> bool {
    is_digit(c) || is_alpha(c)
}
