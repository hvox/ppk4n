#![allow(dead_code)]
#[derive(Clone, Debug, PartialEq)]
pub struct Token<'a> {
	pub source: &'a str,
	pub kind: TokenKind,
}

#[derive(Clone, Debug, PartialEq)]
pub enum TokenKind {
	// Single-character tokens.
	LeftParen,
	RightParen,
	LeftBrace,
	RightBrace,
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
	Identifier(Box<str>),
	String(Box<str>),
	Integer(u64),
	Float(f64),

	// Indentation
	Indent,
	Dedent,

	// Keywords.
	And,
	Class,
	Else,
	False,
	Fun,
	For,
	If,
	Or,
	Print,
	Println,
	Return,
	Super,
	This,
	True,
	While,

	Eof,
}

impl<'a> Token<'a> {
	pub fn new(source: &'a str, kind: TokenKind) -> Self {
		Self { source, kind }
	}

	// pub fn start_position_in(&self, source: &str) -> usize {
	// 	self.source.as_ptr() as usize - source.as_ptr() as usize
	// }
	//
	// pub fn end_position_in(&self, source: &str) -> usize {
	// 	self.start_position_in(source) + self.source.len()
	// }
}
