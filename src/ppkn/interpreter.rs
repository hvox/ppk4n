#![allow(unused_variables)]
use std::rc::Rc;

use super::ir::{Instr, InstrKind, Program, Type};

impl Program<'_> {
	pub fn run(&self) {
		let main = self.functions.get_index_of("main").expect("Function main() not found");
		self.exec_fn(main, vec![]);
	}

	fn exec_fn(&self, function_idx: usize, args: Vec<Value>) -> Option<Value> {
		let f = &self.functions[function_idx];
		let mut locals: Vec<Value> =
			f.locals.iter().map(|(_, typ)| Value::from_type(*typ)).collect();
		for (i, arg) in args.into_iter().enumerate() {
			locals[i] = arg;
		}
		for instr in &self.functions[function_idx].code {
			if let Some(value) = self.exec_instr(&mut locals, instr) {
				return Some(value);
			}
		}
		return None;
	}

	fn exec_instr(&self, locals: &mut Vec<Value>, instr: &Instr) -> Option<Value> {
		match &instr.kind {
			InstrKind::Definition(var_idx, op) => {
				locals[*var_idx] = self.eval_op(locals, &op);
			}
			InstrKind::Assignment(var_idx, op) => {
				locals[*var_idx] = self.eval_op(locals, &op);
			}
			InstrKind::Return(expr) => return Some(self.eval_op(locals, expr)),
			InstrKind::While(condition, code) => {
				while self.eval_op(locals, condition).is_true() {
					for instr in code {
						if let Some(result) = self.exec_instr(locals, instr) {
							return Some(result);
						}
					}
				}
			}
			InstrKind::Print(op) => {
				let value = self.eval_op(locals, op);
				match value {
					Value::String(s) => print!("{}", s),
					Value::Int(value) => print!("{}", value),
					Value::Float(value) => print!("{}", value),
					Value::Bool(value) => print!("{}", value),
					Value::None => {}
				}
			}
			_ => {
				self.eval_op(locals, instr);
			}
		}
		None
	}

	fn eval_op(&self, locals: &mut Vec<Value>, op: &Instr) -> Value {
		match &op.kind {
			InstrKind::Integer(x) => Value::Int(*x),
			InstrKind::Float(x) => Value::Float(*x),
			InstrKind::Variable(x) => locals[*x].clone(),
			InstrKind::String(x) => Value::String(x.to_string().into()),
			InstrKind::Call(f, args) => {
				let mut arguments = Vec::with_capacity(args.len());
				for arg in args {
					arguments.push(self.eval_op(locals, arg));
				}
				self.exec_fn(*f, arguments).unwrap_or(Value::None)
			}
			InstrKind::AddI64(lhs, rhs) => Value::Int(
				self.eval_op(locals, lhs).integer() + self.eval_op(locals, rhs).integer(),
			),
			InstrKind::SubI64(lhs, rhs) => Value::Int(
				self.eval_op(locals, lhs).integer() - self.eval_op(locals, rhs).integer(),
			),
			InstrKind::MulI64(lhs, rhs) => Value::Int(
				self.eval_op(locals, lhs).integer() * self.eval_op(locals, rhs).integer(),
			),
			InstrKind::DivI64(lhs, rhs) => Value::Int(
				self.eval_op(locals, lhs).integer() / self.eval_op(locals, rhs).integer(),
			),
			InstrKind::AddF64(op, op1) => todo!(),
			InstrKind::LessI64(lhs, rhs) => Value::Bool(
				self.eval_op(locals, lhs).integer() < self.eval_op(locals, rhs).integer(),
			),
			_ => unreachable!(),
		}
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
			Value::String(value) => &value[..] != "",
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
