pub mod tokenizer;
pub mod token;
pub mod parser;
pub mod ast;
pub mod typechecker;
pub mod ir;
pub mod interpreter;

use ir::Program;

pub struct PpknError<'a> {
	pub typ: &'static str,
	pub message: &'static str,
	pub location: &'a str,
}

pub fn parse(source: &str) -> Result<Program, PpknError> {
	let tokens = tokenizer::tokenize(source);
	let ast = parser::parse(tokens)?;
	let program = typechecker::typecheck(ast)?;
	Ok(program)
}

impl<'a> From<typechecker::TypeError<'a>> for PpknError<'a> {
	fn from(error: typechecker::TypeError<'a>) -> Self {
		Self { typ: "TypeError", message: error.message, location: error.location }
	}
}

impl<'a> From<parser::SyntaxError<'a>> for PpknError<'a> {
	fn from(error: parser::SyntaxError<'a>) -> Self {
		Self { typ: "SyntaxError", message: error.message, location: error.source }
	}
}
