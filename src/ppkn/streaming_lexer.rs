#![allow(dead_code)]

use std::{collections::VecDeque};

use super::common::{Position, Span};

const IGNORE_COMMENTS: bool = true;

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct Token {
    line: u16,
    column: u16,
    length: u16,
    pub kind: TokenKind,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum TokenKind {
    // Comments
    Comment,

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
    ColonSpace,

    // Literals.
    Identifier,
    Integer,
    Abobik,
    String,
    Decimal,

    // Indentation
    Linend,
    Indent,
    Dedent,
    Eof,

    // Soft keywords
    Fun,
    Import,

    // Hard keywords.
    As,
    Use,
    Let,
    Pub,
    Mut,
    And,
    Class,
    Else,
    False,
    For,
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

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Lexer<'l, Lines: Iterator<Item = &'l str>> {
    line: usize,
    indent: usize,
    lines: Lines,
    buffer: VecDeque<Token>,
    literals: VecDeque<&'l str>,
}

impl<'l, Lines: Iterator<Item = &'l str>> Lexer<'l, Lines> {
    pub fn new(lines: Lines) -> Self {
        Self { line: 0, indent: 0, lines, buffer: VecDeque::new(), literals: VecDeque::new() }
    }

    pub fn next(&mut self) -> Token {
        use TokenKind::*;
        if self.buffer.is_empty() {
            self.tokenize_line();
        }
        let token = self.buffer.pop_front().unwrap();
        if matches!(token.kind, Identifier | String | Integer | Decimal) {
            self.literals.pop_front();
        }
        token
    }

    pub fn peek(&mut self) -> Token {
        if self.buffer.is_empty() {
            self.tokenize_line();
        }
        self.buffer[0]
    }

    pub fn peek_nth(&mut self, n: usize) -> Token {
        while n >= self.buffer.len() {
            self.tokenize_line();
        }
        self.buffer[n]
    }

    pub fn skip_if(&mut self, pred: impl FnOnce(Token) -> bool) {
        if pred(self.peek()) {
            self.skip();
        }
    }

    pub fn skip_while(&mut self, mut pred: impl FnMut(Token) -> bool) {
        while pred(self.peek()) {
            self.skip();
        }
    }

    pub fn literal(&mut self) -> &'l str {
        self.literals.front().unwrap()
    }

    pub fn skip(&mut self) {
        self.next();
    }

    pub fn reset_line_position(&mut self) -> usize {
        let position = self.line;
        self.buffer.iter_mut().for_each(|token| token.line = 0);
        self.line = 0;
        position
    }

    fn tokenize_line(&mut self) {
        use TokenKind::*;
        self.line += 1;
        let Some(line) = self.lines.next() else {
            if self.indent > 0 {
                (0..self.indent).for_each(|_| self.push_token(0, 1, Dedent));
                self.push_token(0, 1, Linend);
                self.indent = 0;
            }
            self.push_token(0, 1, Eof);
            return;
        };
        // let mut chars = line.char_indices().peekable();
        let mut chars = PeekableChars::new(line);
        chars.skip_while(|chr| matches!(chr, ' ' | '\t'));
        let indent = chars.position;
        if indent != self.indent {
            (self.indent..indent).for_each(|column| self.push_token(column, 1, Indent));
            (indent..self.indent).for_each(|_| self.push_token(0, indent.max(1), Dedent));
            self.indent = indent;
        }
        loop {
            let start = chars.position;
            let kind = match chars.next() {
                '(' => LeftParen,
                ')' => RightParen,
                '{' => LeftBrace,
                '}' => RightBrace,
                '[' => LeftBracket,
                ']' => RightBracket,
                ',' => Comma,
                '.' => Dot,
                '-' => chars.if_next('>', RightArrow, Minus),
                '+' => Plus,
                ':' => chars.if_peek(' ', ColonSpace, Colon),
                ';' => Semicolon,
                '/' => Slash,
                '*' => Star,
                '%' => Rem,
                '!' => chars.if_next('=', BangEqual, Bang),
                '=' => chars.if_next('=', EqualEqual, Equal),
                '>' => chars.if_next('=', GreaterEqual, Greater),
                '<' => chars.if_next('=', LessEqual, Less),
                '\t' | '\x0C' | '\r' | ' ' => continue,
                '\n' => return self.push_token(start, 1, Linend),
                '#' => {
                    chars.skip_while(|chr| chr != '\n');
                    if IGNORE_COMMENTS {
                        continue;
                    } else {
                        Comment
                    }
                }
                '0'..='9' => {
                    chars.skip_while(|chr| matches!(chr, '0'..='9'));
                    if chars.peek() != '.' {
                        self.literals.push_back(&line[start..chars.position]);
                        Integer
                    } else {
                        chars.next();
                        chars.skip_while(|chr| matches!(chr, '0'..='9'));
                        self.literals.push_back(&line[start..chars.position]);
                        Decimal
                    }
                }
                '\0'..'\t' => todo!(),
                _ => {
                    chars.skip_while(|chr| !matches!(chr, '\0'..'0'|':'..'A'|'['..'a'|'{'..='~'));
                    let word = &line[start..chars.position];
                    match word {
                        "fun" if start == 0 => Fun,
                        "import" if start == 0 => Import,
                        "as" => As,
                        "or" => Or,
                        "and" => And,
                        "true" => True,
                        "false" => False,
                        "if" => If,
                        "else" => Else,
                        "class" => Class,
                        "for" => For,
                        "let" => Let,
                        "pub" => Pub,
                        "mut" => Mut,
                        "use" => Use,
                        "this" => This,
                        "while" => While,
                        "super" => Super,
                        "return" => Return,
                        _ => {
                            self.literals.push_back(word);
                            Identifier
                        }
                    }
                }
            };
            self.push_token(start, chars.position - start, kind);
        }
    }

    fn push_token(&mut self, column: usize, length: usize, kind: TokenKind) {
        let token = Token::new(self.line, column, length, kind);
        self.buffer.push_back(token);
    }
}

struct PeekableChars<'s> {
    string: &'s str,
    position: usize,
}

impl<'s> PeekableChars<'s> {
    fn new(string: &'s str) -> Self {
        Self { string, position: 0 }
    }

    fn skip_while(&mut self, mut pred: impl FnMut(char) -> bool) {
        while pred(self.peek()) {
            self.next();
        }
    }

    fn if_next<T>(&mut self, expected: char, then: T, otherwise: T) -> T {
        if self.peek() == expected {
            self.next();
            then
        } else {
            otherwise
        }
    }

    fn if_peek<T>(&mut self, expected: char, then: T, otherwise: T) -> T {
        if self.peek() == expected {
            then
        } else {
            otherwise
        }
    }

    fn next_indexed(&mut self) -> (usize, char) {
        let position = self.position;
        let chr = self.next();
        (position, chr)
    }

    fn next(&mut self) -> char {
        let chr = self.peek();
        self.position += chr.len_utf8();
        chr
    }

    fn peek(&self) -> char {
        if let Some(chr) = self.string[self.position..].chars().next() {
            chr
        } else {
            unreachable!()
        }
    }
}

impl Token {
    pub fn new(line: usize, column: usize, length: usize, kind: TokenKind) -> Self {
        Self { line: line as u16, column: column as u16, length: length as u16, kind }
    }

    pub fn span(&self) -> Span {
        let start = Position::new(self.line.into(), self.column.into());
        let end = Position::new(self.line.into(), (self.column + self.length).into());
        Span::new(start, end)
    }
}
