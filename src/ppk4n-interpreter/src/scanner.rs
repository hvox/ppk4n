#[allow(unused)]
#[derive(Debug, Clone)]
pub struct Token<'a> {
	pub source: &'a str,
	// pub kind: TokenKind,
}

#[allow(unused)]
#[derive(Debug, Clone)]
pub enum TokenKind {
	Word, // TODO: split this kind
	Comment,
	Indent,
	Dedent,
	EOF,
}

pub fn scan_tokens(source: &str) -> Vec<&str> {
	// TODO: use state machine and just append characters to last token
	let mut tokens = vec![];
	let mut indents = vec![0];
	let mut chars = source.char_indices().peekable();
	while let Some((i, char)) = chars.next() {
		let j = i + char.len_utf8();
		tokens.push(match char {
			'"' => {
				let string = scan_string(source, i);
				for _ in string.chars().skip(1) {
					chars.next();
				}
				string
			}
			'\t' | ' ' | '\r' => continue,
			'\n' => {
				let mut indent = 0;
				while let Some(&(i, char)) = chars.peek() {
					if !char.is_ascii_whitespace() {
						break;
					}
					indent += 1;
					chars.next();
					if indents.last().is_some_and(|&x| x < indent) {
						tokens.push(&source[i..(i + 1)]);
					}
				}
				if indents.last().is_some_and(|&x| x < indent) {
					indents.push(indent);
				} else if indents.last().is_some_and(|&x| x > indent) {
					for _ in 0..(indents[indents.len() - 1] - indent) {
						tokens.push(&source[j..j]);
					}
					indents.push(indent);
				}
				continue;
			}
			char if char.is_ascii_punctuation() => &source[i..j],
			char => {
				let mut j = i + char.len_utf8();
				while let Some((_, char)) = chars.peek() {
					if !char.is_ascii_alphanumeric() {
						break;
					}
					j += char.len_utf8();
					chars.next();
				}
				&source[i..j]
			}
		});
	}
	tokens
}

fn scan_string(source: &str, start: usize) -> &str {
	let mut i = start + 1;
	while i < source.len() && source.as_bytes()[i] != b'"' {
		i += 1;
	}
	let end = if i < source.len() { i + 1 } else { i };
	&source[start..end]
}

#[allow(unused)]
impl Token<'_> {
	fn new(source: &str) -> Token {
		Token { source }
	}
}
