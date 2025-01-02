use super::token::{Token, TokenKind};

pub fn tokenize(source: &str) -> Vec<Token> {
	Tokenizer::new(source).tokenize()
}

struct Tokenizer<'a> {
	source: &'a str,
	position: usize,
}

impl<'a> Tokenizer<'a> {
	pub fn new(source: &'a str) -> Self {
		Self { source, position: 0 }
	}

	pub fn tokenize(&mut self) -> Vec<Token<'a>> {
		let mut tokens = vec![];
		while self.position < self.source.len() {
			let token_start = self.position;
			if let Some(kind) = self.next_token() {
				let source = &self.source[token_start..self.position];
				tokens.push(Token::new(source, kind));
			}
		}
		// let eof = Token::new(&self.source[self.source.len()..], TokenKind::Eof);
		let eof = Token::new("", TokenKind::Eof);
		tokens.push(eof);
		tokens
	}

	fn next_token(&mut self) -> Option<TokenKind> {
		let Some(ch) = self.next_char() else {
			return None;
		};
		use TokenKind::*;
		match ch {
			'(' => Some(LeftParen),
			')' => Some(RightParen),
			'{' => Some(LeftBrace),
			'}' => Some(RightBrace),
			',' => Some(Comma),
			'.' => Some(Dot),
			'-' => Some(Minus),
			'+' => Some(Plus),
			':' => Some(Colon),
			';' => Some(Semicolon),
			'/' => Some(Slash),
			'*' => Some(Star),
			'!' => Some(if self.consume('=') { BangEqual } else { Bang }),
			'=' => Some(if self.consume('=') { EqualEqual } else { Equal }),
			'>' => Some(if self.consume('=') { GreaterEqual } else { Greater }),
			'<' => Some(if self.consume('=') { LessEqual } else { Less }),
			' ' | '\r' | '\t' | '\n' => None,
			'#' => {
				self.consume_while(|ch| ch != '\n');
				None
			}
			'0'..='9' => {
				self.position -= 1;
				Some(self.number())
			}
			'"' | '\'' => {
				self.position -= 1;
				Some(String(self.string()))
			}
			ch => {
				self.position -= ch.len_utf8();
				Some(self.keyword_or_identifier())
			}
		}
	}

	fn number(&mut self) -> TokenKind {
		let mut number: u64 = 0;
		while let Some(ch) = self.next_char() {
			if let Some(digit) = ch.to_digit(10) {
				number = number.wrapping_mul(10).wrapping_add(digit as u64);
			} else if ch == '.' {
				break;
			} else {
				self.position -= 1;
				return TokenKind::Integer(number);
			}
		}
		let mut number = number as f64;
		let mut precision = 0.1;
		while let Some(ch) = self.next_char() {
			if let Some(digit) = ch.to_digit(10) {
				number += precision * digit as f64;
				precision *= 0.1;
			} else {
				self.position -= 1;
				break;
			}
		}
		return TokenKind::Float(number);
	}

	fn string(&mut self) -> Box<str> {
		let mut string = String::new();
		let boundary = self.next_char().unwrap();
		while let Some(ch) = self.next_char() {
			if ch == boundary {
				break;
			} else if ch == '\\' {
				string.push(match self.next_char().unwrap_or('\\') {
					'n' => '\n',
					'r' => '\r',
					't' => '\t',
					ch => ch,
				});
			} else {
				string.push(ch);
			}
		}
		string.into()
	}

	fn keyword_or_identifier(&mut self) -> TokenKind {
		let mut name = String::new();
		while let Some(ch) = self.next_char() {
			if "(){},.-+:;/*!=><#'\"\t\n\r ".contains(ch) {
				self.position -= ch.len_utf8();
				break;
			}
			name.push(if ch == '\\' { self.next_char().unwrap_or('\\') } else { ch });
		}
		match name.as_str() {
			"and" => TokenKind::And,
			"class" => TokenKind::Class,
			"else" => TokenKind::Else,
			"false" => TokenKind::False,
			"fun" => TokenKind::Fun,
			"for" => TokenKind::For,
			"if" => TokenKind::If,
			"or" => TokenKind::Or,
			"print" => TokenKind::Print,
			"println" => TokenKind::Println,
			"return" => TokenKind::Return,
			"super" => TokenKind::Super,
			"this" => TokenKind::This,
			"true" => TokenKind::True,
			"while" => TokenKind::While,
			_ => TokenKind::Identifier(name.into()),
		}
	}

	fn consume(&mut self, target: char) -> bool {
		let position = self.position;
		if let Some(ch) = self.next_char() {
			if ch == target {
				return true;
			}
			self.position = position;
		}
		false
	}

	fn consume_while(&mut self, pred: fn(char) -> bool) {
		loop {
			let position = self.position;
			if let Some(ch) = self.next_char() {
				if pred(ch) {
					continue;
				} else {
					self.position = position;
				}
			}
			break;
		}
	}

	fn next_char(&mut self) -> Option<char> {
		if let Some(ch) = self.source[self.position..].chars().next() {
			self.position += ch.len_utf8();
			Some(ch)
		} else {
			None
		}
	}
}
