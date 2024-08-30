use std::ops::Range;

#[derive(Debug, Clone)]
pub struct Token<'a> {
	pub source: &'a str,
	// pub kind: TokenKind,
}

#[derive(Debug, Clone)]
pub enum TokenKind {
	Word, // TODO: split this kind
	Comment,
	Indent,
	Dedent,
	EOF,
}

#[derive(Debug, Clone)]
pub struct ScannerError {
	pub position: usize,
}

pub fn scan_tokens(source: &str) -> (Vec<Token>, Vec<ScannerError>) {
	let mut tokens = vec![];
	let mut errors = vec![];
	let mut indents = vec![0];
	let mut chars = source.char_indices().peekable();
	while let Some((i, char)) = chars.next() {
		let j = i + char.len_utf8();
		tokens.push(Token::new(match char {
			'"' => {
				let string = scan_string(source, i).source;
				for _ in string.chars() {
					chars.next();
				}
				string
			}
			char if char.is_ascii_punctuation() => &source[i..j],
			'\t' | ' ' | '\r' => continue,
			'\n' => {
				let mut indentation = 0;
				while let Some((_, 'a')) = chars.peek() {
					if !char.is_ascii_alphanumeric() {
						break;
					}
					chars.next();
				}
				continue;
			}
			char => {
				let mut j = i + char.len_utf8();
				while let Some((i, char)) = chars.peek() {
					if !char.is_ascii_alphanumeric() {
						break;
					}
					j += char.len_utf8();
					chars.next();
				}
				&source[i..j]
			}
		}));
	}
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
	Token { source: &source[start..end] }
}

impl Token<'_> {
	fn new(source: &str) -> Token {
		Token { source }
	}
}
