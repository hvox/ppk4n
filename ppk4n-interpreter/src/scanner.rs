use std::ops::Range;

#[derive(Debug, Clone)]
pub struct Token {
	pub range: Range<usize>,
	pub typ: TokenType,
}

#[derive(Debug, Clone)]
pub enum TokenType {
	Plus,
	Minus,
	Number(f64),
	String(String),
	EOF,
}

#[derive(Debug, Clone)]
pub struct ScannerError {
	pub position: usize,
}

pub fn scan_tokens(source: &str) -> (Vec<Token>, Vec<ScannerError>) {
	let source_bytes = source.as_bytes();
	let mut current = 0;
	let mut tokens = vec![];
	let mut errors = vec![];
	while current < source_bytes.len() {
		use TokenType::*;
		match source_bytes[current] {
			b'+' => tokens.push(Token::new(Plus, current..current + 1)),
			b'-' => tokens.push(Token::new(Minus, current..current + 1)),
			b'#' => {
				while current < source_bytes.len() && source_bytes[current] != b'\n' {
					current += 1;
				}
			}
			b'"' => {
				let token = scan_string(source, current);
				current = token.range.end;
				tokens.push(token);
				continue;
			}
			b' ' => {}
			b'\t' => {}
			b'\r' => {}
			b'\n' => {}
			unexpected_character => {
				errors.push(ScannerError { position: current });
			}
		}
		current += 1;
	}
	tokens.push(Token::new(TokenType::EOF, current..current));
	(tokens, errors)
}

fn scan_string(source: &str, start: usize) -> Token {
	let mut string = vec![];
	let mut i = start + 1;
	while i < source.len() && source.as_bytes()[i] != b'"' {
		string.push(source.as_bytes()[i]);
		i += 1;
	}
	let end = if i < source.len() { i + 1 } else { i };
	let value = String::from_utf8(string).unwrap();
	Token::new(TokenType::String(value), start..end)
}

impl Token {
	fn new(typ: TokenType, range: Range<usize>) -> Token {
		Token { typ, range }
	}
}
