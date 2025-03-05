#![allow(unused)]
pub fn tokenize(source: &str) -> Vec<Token> {
	Lexer::new(source).tokenize()
}

struct Lexer<'src> {
	// TODO: use iterator
	source: &'src str,
	position: usize,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct Token {
	position: u16,
	length: u8,
	kind: TokenKind,
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

	// One or two character tokens.
	Bang,
	BangEqual,
	Equal,
	EqualEqual,
	Greater,
	GreaterEqual,
	Less,
	LessEqual,

	// Literals.
	Identifier,
	Integer,
	String,
	Decimal,

	// Indentation
	Indent,
	Dedent,
	Eof,

	// Keywords.
	And,
	Class,
	Else,
	False,
	For,
	Fun,
	If,
	Or,
	Print,
	Println,
	Return,
	Super,
	This,
	True,
	While,

	// Errors
	UnterminatedString,
}

impl Token {
	fn new(position: usize, length: usize, kind: TokenKind) -> Self {
		use std::convert::TryInto;
		Self { position: position.try_into().unwrap(), length: length.try_into().unwrap(), kind }
	}
}

impl<'s> Lexer<'s> {
	fn new(source: &'s str) -> Self {
		Self { source, position: 0 }
	}

	fn tokenize(&mut self) -> Vec<Token> {
		let mut tokens = vec![];
		// Separate loop with faster check.
		// TODO: check if it really improves performance
		while self.position < self.source.len() {
			tokens.push(self.next());
		}
		while !tokens.last().is_some_and(|eof| eof.kind == TokenKind::Eof) {
			tokens.push(self.next());
		}
		tokens
	}

	fn next(&mut self) -> Token {
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
			'-' => Minus,
			'+' => Plus,
			':' => Colon,
			';' => Semicolon,
			'/' => Slash,
			'*' => Star,
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
				return self.next();
			}
			'#' => {
				while self.peek() != '\n' {
					self.read();
				}
				return self.next();
			}
			'0'..='9' => {
				self.position -= 1;
				self.number()
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
					"fun" => Fun,
					"for" => For,
					"if" => If,
					"or" => Or,
					"print" => Print,
					"println" => Println,
					"return" => Return,
					"super" => Super,
					"this" => This,
					"true" => True,
					"while" => While,
					_ => Identifier,
				}
			}
		};
		let length = self.position - start;
		Token::new(start, length, kind)
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
		while matches!(self.peek(), '0'..='9') {
			self.read();
		}
		if self.peek() != '.' {
			return TokenKind::Integer;
		}
		while matches!(self.peek(), '0'..='9') {
			self.read();
		}
		TokenKind::Decimal
	}

	fn skip(&mut self) {
		self.position += self.peek().len_utf8();
	}

	fn read(&mut self) -> char {
		let chr = self.peek();
		self.position += chr.len_utf8();
		chr
	}

	fn peek(&self) -> char {
		match self.source[self.position..].chars().next() {
			Some(ch) => ch,
			None => '\n',
		}
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
