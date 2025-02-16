pub mod tokenizer;
pub mod token;
pub mod parser;
pub mod ast;
pub mod typechecker;
pub mod ir;
pub mod interpreter;

pub mod mir_generator;
pub mod mir;
pub mod mir_interpreter;
pub mod transpiler;
pub mod mir_compiler;

use mir::Program;

pub struct PpknError<'a> {
	pub typ: &'static str,
	pub message: &'static str,
	pub location: &'a str,
}

pub fn parse(source: &str) -> Result<Program, PpknError> {
	let tokens = tokenizer::tokenize(source);
	let ast = parser::parse(tokens)?;
	let program = mir_generator::typecheck(ast)?;
	Ok(program)
}

pub fn run(source: &str) -> Result<(), PpknError> {
	const DEBUG_PARSING: bool = false;
	let program = parse(source)?;
	#[cfg(debug_assertions)]
	if DEBUG_PARSING {
		for func in &program.functions {
			println!("fun {} {:?} -> {:?}:", func.name, func.params, func.result);
			for stmt in &func.body {
				println!("  {:12} {:?}", stmt.source.replace("\n", "\\n"), stmt.kind);
			}
		}
	}
	program.run()?;
	Ok(())
}

impl<'a> From<mir_generator::TypeError<'a>> for PpknError<'a> {
	fn from(error: mir_generator::TypeError<'a>) -> Self {
		Self { typ: "TypeError", message: error.message, location: error.location }
	}
}

impl<'a> From<parser::SyntaxError<'a>> for PpknError<'a> {
	fn from(error: parser::SyntaxError<'a>) -> Self {
		Self { typ: "SyntaxError", message: error.message, location: error.source }
	}
}

impl<'a> From<mir_interpreter::RuntimeError<'a>> for PpknError<'a> {
	fn from(error: mir_interpreter::RuntimeError<'a>) -> Self {
		Self { typ: "RuntimeError", message: error.message, location: error.location }
	}
}
