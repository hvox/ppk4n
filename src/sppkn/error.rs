#![allow(unused)]

use std::rc::Rc;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Error {
	pub module: Str,
	pub cause_location: (u32, u32),
	pub message: String,
	pub kind: PpknErrorKind,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum PpknErrorKind {
	LoadError,
	SyntaxError,
	TypeError,
	NameError,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct SyntaxError {
	pub cause_location: (u32, u32),
	pub message: &'static str,
}

type Str = Rc<str>;

impl Error {
	pub fn new<S>(module: S, location: (u32, u32), kind: PpknErrorKind, message: String) -> Self
	where
		S: Into<Str>,
	{
		let module = module.into();
		Self { module, cause_location: location, message, kind }
	}
}

impl SyntaxError {
	pub fn new(location: (u32, u32), message: &'static str) -> Self {
		Self { cause_location: location, message }
	}
}

impl SyntaxError {
	pub fn into_error(self, module: Str) -> Error {
		let cause_location = self.cause_location;
		let message = self.message.into();
		let kind = PpknErrorKind::SyntaxError;
		Error { module, cause_location, message, kind }
	}
}

impl From<SyntaxError> for Error {
	fn from(error: SyntaxError) -> Self {
		let cause_location = error.cause_location;
		let message = error.message.into();
		let kind = PpknErrorKind::SyntaxError;
		Self { module: "TODO".into(), cause_location, message, kind }
	}
}
