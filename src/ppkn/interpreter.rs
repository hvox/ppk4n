#![allow(unused_variables)]
use std::rc::Rc;

use super::ir::{Instr, InstrKind, Program, Type};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct RuntimeError<'a> {
	pub location: &'a str,
	pub message: &'static str,
}

enum Interruption<'a> {
	Error(RuntimeError<'a>),
	Return(Value),
}

impl<'a> Program<'a> {
	pub fn run(&self) -> Result<(), RuntimeError<'a>> {
		let main = self.functions.get_index_of("main").expect("Function main() not found");
		self.exec_fn(main, vec![])?;
		Ok(())
	}

	fn exec_fn(&self, function_idx: usize, args: Vec<Value>) -> Result<Value, RuntimeError<'a>> {
		let f = &self.functions[function_idx];
		let mut locals: Vec<Value> =
			f.locals.iter().map(|(_, typ)| Value::from_type(*typ)).collect();
		for (i, arg) in args.into_iter().enumerate() {
			locals[i] = arg;
		}
		for instr in &self.functions[function_idx].code {
			if let Err(interruption) = self.exec_instr(&mut locals, instr) {
				match interruption {
					Interruption::Error(runtime_error) => return Err(runtime_error),
					Interruption::Return(value) => return Ok(value),
				}
			}
		}
		Ok(Value::None)
	}

	fn exec_instr(
		&self,
		locals: &mut Vec<Value>,
		instr: &Instr<'a>,
	) -> Result<Value, Interruption<'a>> {
		Ok(match &instr.kind {
			InstrKind::Definition(var_idx, op) => {
				locals[*var_idx] = self.exec_instr(locals, op)?;
				Value::None
			}
			InstrKind::Assignment(var_idx, op) => {
				locals[*var_idx] = self.exec_instr(locals, op)?;
				Value::None
			}
			InstrKind::Return(expr) => {
				return Err(Interruption::Return(self.exec_instr(locals, expr)?))
			}
			InstrKind::While(condition, code) => {
				while self.exec_instr(locals, condition)?.is_true() {
					for instr in code {
						self.exec_instr(locals, instr)?;
					}
				}
				Value::None
			}
			InstrKind::Print(expr) => {
				let value = self.exec_instr(locals, expr)?;
				match value {
					Value::String(s) => print!("{}", s),
					Value::Int(value) => print!("{}", value),
					Value::Float(value) => print!("{}", value),
					Value::Bool(value) => print!("{}", value),
					Value::None => {}
				}
				Value::None
			}
			InstrKind::Integer(x) => Value::Int(*x),
			InstrKind::Float(x) => Value::Float(*x),
			InstrKind::Variable(x) => locals[*x].clone(),
			InstrKind::String(x) => Value::String(x.to_string().into()),
			InstrKind::Call(f, args) => {
				let mut arguments = Vec::with_capacity(args.len());
				for arg in args {
					arguments.push(self.exec_instr(locals, arg)?);
				}
				self.exec_fn(*f, arguments).unwrap_or(Value::None)
			}
			InstrKind::AddI64(lhs, rhs) => Value::Int(
				self.exec_instr(locals, lhs)?.integer() + self.exec_instr(locals, rhs)?.integer(),
			),
			InstrKind::SubI64(lhs, rhs) => Value::Int(
				self.exec_instr(locals, lhs)?.integer() - self.exec_instr(locals, rhs)?.integer(),
			),
			InstrKind::MulI64(lhs, rhs) => Value::Int(
				self.exec_instr(locals, lhs)?.integer() * self.exec_instr(locals, rhs)?.integer(),
			),
			InstrKind::DivI64(lhs, rhs) => {
				let rhs = self.exec_instr(locals, rhs)?.integer();
				if rhs == 0 {
					return Err(Interruption::Error(RuntimeError {
						location: instr.source,
						message: "division by zero",
					}));
				}
				Value::Int(self.exec_instr(locals, lhs)?.integer() / rhs)
			}
			InstrKind::AddF64(lhs, rhs) => Value::Float(todo!()),
			InstrKind::LessI64(lhs, rhs) => Value::Bool(
				self.exec_instr(locals, lhs)?.integer() < self.exec_instr(locals, rhs)?.integer(),
			),
		})
	}
}

#[derive(Debug, Clone)]
enum Value {
	String(Rc<str>),
	Int(i64),
	Float(f64),
	Bool(bool),
	None,
}

impl Value {
	fn integer(&self) -> i64 {
		match self {
			Self::Int(x) => *x,
			Self::Float(x) => *x as i64,
			_ => unreachable!(),
		}
	}

	fn is_true(&self) -> bool {
		match self {
			Value::String(value) => !value[..].is_empty(),
			Value::Int(value) => *value != 0,
			Value::Float(value) => *value != 0.0,
			Value::Bool(value) => *value,
			Value::None => false,
		}
	}

	fn from_type(typ: Type) -> Self {
		match typ {
			Type::I64 => Self::Int(0),
			Type::F64 => Self::Float(0.0),
			Type::Str => Self::String("".into()),
			Type::Void => Self::None,
		}
	}
}
