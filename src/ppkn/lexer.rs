use std::fmt::Debug;

pub fn tokenize(source: &str) -> Vec<Token> {
    Lexer::new(source).tokenize()
}

struct Lexer<'src> {
    // TODO: use iterator
    source: &'src str,
    position: usize,
    indentation: usize,
    tokens: Vec<Token>,
}

#[derive(PartialEq, Eq, Hash, Clone, Copy)]
pub struct Token {
    pub position: u16,
    pub length: u8,
    pub kind: TokenKind,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum TokenKind {
    // Single-character tokens.
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    Comma,
    Dot,
    Minus,
    Plus,
    Colon,
    Semicolon,
    Slash,
    Star,
    Rem,

    // One or two character tokens.
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    RightArrow,

    // Literals.
    Identifier,
    Integer,
    String,
    Decimal,

    // Indentation
    Linend,
    Indent,
    Dedent,
    Eof,

    // Keywords.
    Use,
    Let,
    Mut,
    And,
    Class,
    Else,
    False,
    For,
    Fun,
    If,
    Or,
    Return,
    Super,
    This,
    True,
    While,

    // Errors
    UnterminatedString,
}

impl<'s> Lexer<'s> {
    fn new(source: &'s str) -> Self {
        Self { source, position: 0, indentation: 0, tokens: vec![] }
    }

    fn tokenize(mut self) -> Vec<Token> {
        while self.position < self.source.len() {
            self.read_next_token();
        }
        while !self.tokens.last().is_some_and(|eof| eof.kind == TokenKind::Eof) {
            self.read_next_token();
        }
        self.tokens
    }

    fn read_next_token(&mut self) {
        use TokenKind::*;
        let start = self.position;
        let chr = self.read();
        let kind = match chr {
            '(' => LeftParen,
            ')' => RightParen,
            '{' => LeftBrace,
            '}' => RightBrace,
            '[' => LeftBracket,
            ']' => RightBracket,
            ',' => Comma,
            '.' => Dot,
            '-' => {
                if self.expect('>') {
                    RightArrow
                } else {
                    Minus
                }
            }
            '+' => Plus,
            ':' => Colon,
            ';' => Semicolon,
            '/' => Slash,
            '*' => Star,
            '%' => Rem,
            '!' => {
                if self.expect('=') {
                    BangEqual
                } else {
                    Bang
                }
            }
            '=' => {
                if self.expect('=') {
                    EqualEqual
                } else {
                    Equal
                }
            }
            '>' => {
                if self.expect('=') {
                    GreaterEqual
                } else {
                    Greater
                }
            }
            '<' => {
                if self.expect('=') {
                    LessEqual
                } else {
                    Less
                }
            }
            '\t' | '\x0C' | '\r' | ' ' => {
                while matches!(self.peek(), '\t' | '\x0C' | '\r' | ' ') {
                    self.read();
                }
                return self.read_next_token();
            }
            '#' => {
                while self.peek() != '\n' {
                    self.read();
                }
                return self.read_next_token();
            }
            '0'..='9' => {
                self.position -= 1;
                self.number()
            }
            // TODO: more clever Linend insertion
            '\n' => {
                if self.position == self.source.len() {
                    if self.indentation > 0 {
                        self.indentation -= 1;
                        self.tokens.push(Token::new(start, 1, Linend));
                        Dedent
                    } else {
                        self.tokens.push(Token::new(start, 1, Linend));
                        Eof
                    }
                } else {
                    while matches!(self.peek(), '\t' | '\x0C' | '\r' | ' ') {
                        self.read();
                    }
                    let current_indent = self.position - start - 1;
                    if self.peek() != '\n' {
                        if current_indent == self.indentation {
                            self.tokens.push(Token::new(start + 1, current_indent, Linend));
                        }
                        while current_indent > self.indentation {
                            self.tokens.push(Token::new(start + current_indent, 1, Indent));
                            self.indentation += 1;
                        }
                        while current_indent < self.indentation {
                            self.tokens.push(Token::new(start, 1, Linend));
                            self.tokens.push(Token::new(start + 1, current_indent, Dedent));
                            self.indentation -= 1;
                        }
                    }
                    return self.read_next_token();
                }
            }
            '"' => self.string(),
            _ => {
                while !matches!(self.peek(), '\t'..='/' | ':'..='@' | '['..='^' | '{'..='~') {
                    self.read();
                }
                let word = &self.source[start..self.position];
                match word {
                    "and" => And,
                    "class" => Class,
                    "else" => Else,
                    "false" => False,
                    "for" => For,
                    "fun" => Fun,
                    "if" => If,
                    "let" => Let,
                    "mut" => Mut,
                    "or" => Or,
                    // Why have many keywords when few do trick
                    // "println" => Println,
                    // "print" => Print,
                    "return" => Return,
                    "super" => Super,
                    "this" => This,
                    "true" => True,
                    "use" => Use,
                    "while" => While,
                    _ => Identifier,
                }
            }
        };
        let length = self.position - start;
        self.tokens.push(Token::new(start, length, kind));
    }

    fn string(&mut self) -> TokenKind {
        loop {
            let chr = self.read();
            match chr {
                '"' => break TokenKind::String,
                '\n' => break TokenKind::UnterminatedString,
                '\\' => self.skip(),
                _ => {}
            };
        }
    }

    fn number(&mut self) -> TokenKind {
        while self.peek().is_ascii_digit() {
            self.read();
        }
        if self.peek() != '.' {
            return TokenKind::Integer;
        }
        while self.peek().is_ascii_digit() {
            self.read();
        }
        TokenKind::Decimal
    }

    fn skip(&mut self) {
        self.position += self.peek().len_utf8();
    }

    fn read(&mut self) -> char {
        match self.source[self.position..].chars().next() {
            Some(chr) => {
                self.position += chr.len_utf8();
                chr
            }
            None => '\n',
        }
    }

    fn peek(&self) -> char {
        self.source[self.position..].chars().next().unwrap_or('\n')
    }

    // TODO: replace with if_matches<T>(...) -> T
    fn expect(&mut self, chr: char) -> bool {
        if self.peek() == chr {
            self.read();
            true
        } else {
            false
        }
    }
}

impl Token {
    fn new(position: usize, length: usize, kind: TokenKind) -> Self {
        use std::convert::TryInto;
        Self { position: position.try_into().unwrap(), length: length.try_into().unwrap(), kind }
    }

    pub fn span(&self) -> (u32, u32) {
        let start = u32::from(self.position);
        let end = start + u32::from(self.length);
        (start, end)
    }
}

impl Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let start = self.position;
        let end = usize::from(self.position) + usize::from(self.length);
        write!(f, "{:?}[{}:{}]", self.kind, start, end)
    }
}
