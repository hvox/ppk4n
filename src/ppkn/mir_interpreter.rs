#![allow(unused)]
use std::{any::Any, mem::transmute, rc::Rc};

use super::mir::*;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct RuntimeError<'a> {
	pub location: &'a str,
	pub message: &'static str,
}

struct Interruption<'a> {
	result: Result<u64, RuntimeError<'a>>,
}

impl<'a> Program<'a> {
	pub fn run(&self) -> Result<(), RuntimeError<'a>> {
		for (i, f) in self.functions.iter().enumerate() {
			if &*f.name == "main" {
				self.eval_fn(i, vec![])?;
				return Ok(());
			}
		}
		unreachable!()
	}

	fn eval_fn(&self, fn_index: usize, args: Vec<u64>) -> Result<u64, RuntimeError<'a>> {
		let f = &self.functions[fn_index];
		let mut locals = args;
		locals.extend(vec![0; f.locals.len()]);
		for stmt in &f.body {
			if let Err(interruption) = self.eval_unit(&mut locals, stmt) {
				return interruption.result;
			}
		}
		Ok(0)
	}

	fn eval_unit(&self, locals: &mut [u64], instr: &InstrCntrl) -> Result<(), Interruption<'a>> {
		match &*instr.kind {
			InstrKindCntrl::Call(f, params) => {
				let mut args = vec![];
				for param in params {
					args.push(self.eval_instr(locals, param)?);
				}
				self.eval_fn(*f, args).map_err(|err| Interruption { result: Err(err) })?;
			}
			InstrKindCntrl::DefI64(id, value) | InstrKindCntrl::SetI64(id, value) => {
				locals[*id] = (self.eval_i64(locals, value)?) as u64
			}
			InstrKindCntrl::DefF64(id, value) | InstrKindCntrl::SetF64(id, value) => {
				locals[*id] = (self.eval_f64(locals, value)?).to_bits()
			}
			InstrKindCntrl::DefVec(id, value, typ) => locals[*id] = self.eval_vec(locals, value, typ)?,
			InstrKindCntrl::SetVec(id) => {
				let vec = Rc::new(Vec::<u64>::new());
				locals[*id] = Rc::into_raw(vec) as u64;
			}
			InstrKindCntrl::DefU64(_, instr_u64) => todo!(),
			InstrKindCntrl::DefStr(_, instr_str) => todo!(),
			InstrKindCntrl::DefBool(_, instr_bool) => todo!(),
			InstrKindCntrl::SetU64(_, instr_u64) => todo!(),
			InstrKindCntrl::SetStr(_, instr_str) => todo!(),
			InstrKindCntrl::SetBool(_, instr_bool) => todo!(),
			InstrKindCntrl::PrintStr(value) => print!("{}", self.eval_str(locals, value)?),
			InstrKindCntrl::PrintlnStr(value) => println!("{}", self.eval_str(locals, value)?),
			InstrKindCntrl::While(instr_bool, vec) => todo!(),
			InstrKindCntrl::Block(stmts) => {
				for stmt in stmts {
					self.eval_unit(locals, stmt);
				}
			}
			InstrKindCntrl::Return(instr) => {
				return Err(Interruption { result: Ok(self.eval_instr(locals, instr)?) });
			}
			InstrKindCntrl::Drop(instr) => todo!(),
			InstrKindCntrl::Push(id, instr) => {
				let mut vec = unsafe { Rc::from_raw(locals[*id] as *mut Vec<u64>) };
				Rc::make_mut(&mut vec).push(self.eval_instr(locals, instr)?);
				Rc::into_raw(vec);
			}
		};
		Ok(())
	}

	fn eval_i64(&self, locals: &mut [u64], instr: &InstrI64) -> Result<i64, Interruption<'a>> {
		use InstrKindI64::*;
		Ok(match &*instr.kind {
			Add(lhs, rhs) => self.eval_i64(locals, lhs)? + self.eval_i64(locals, rhs)?,
			Sub(lhs, rhs) => self.eval_i64(locals, lhs)? - self.eval_i64(locals, rhs)?,
			Mult(lhs, rhs) => self.eval_i64(locals, lhs)? * self.eval_i64(locals, rhs)?,
			Div(lhs, rhs) => self.eval_i64(locals, lhs)? / self.eval_i64(locals, rhs)?,
			Rem(lhs, rhs) => self.eval_i64(locals, lhs)? % self.eval_i64(locals, rhs)?,
			And(lhs, rhs) => self.eval_i64(locals, lhs)? & self.eval_i64(locals, rhs)?,
			Xor(lhs, rhs) => self.eval_i64(locals, lhs)? ^ self.eval_i64(locals, rhs)?,
			Or(lhs, rhs) => self.eval_i64(locals, lhs)? | self.eval_i64(locals, rhs)?,
			Return(instr) => {
				return Err(Interruption { result: Ok(self.eval_instr(locals, instr)?) });
			}
			Variable(idx) => locals[*idx] as i64,
			Call(_) => todo!(),
			Value(val) => *val,
		})
	}

	fn eval_f64(&self, locals: &mut [u64], instr: &InstrF64) -> Result<f64, Interruption<'a>> {
		use InstrKindF64::*;
		Ok(match &*instr.kind {
			Add(lhs, rhs) => self.eval_f64(locals, lhs)? + self.eval_f64(locals, rhs)?,
			Sub(lhs, rhs) => self.eval_f64(locals, lhs)? - self.eval_f64(locals, rhs)?,
			Mult(lhs, rhs) => self.eval_f64(locals, lhs)? * self.eval_f64(locals, rhs)?,
			Div(lhs, rhs) => self.eval_f64(locals, lhs)? / self.eval_f64(locals, rhs)?,
			Return(instr) => {
				return Err(Interruption { result: Ok(self.eval_instr(locals, instr)?) });
			}
			Variable(idx) => f64::from_bits(locals[*idx]),
			Call(_) => todo!(),
			Value(val) => *val,
		})
	}

	fn eval_str(&self, locals: &mut [u64], instr: &InstrStr) -> Result<Str, Interruption<'a>> {
		use InstrKindStr::*;
		Ok(match &*instr.kind {
			Add(instr_str, instr_str1) => todo!(),
			Return(instr) => todo!(),
			Variable(_) => todo!(),
			Value(value) => value.clone(),
			Call(_) => todo!(),
			CastI64(instr) => self.eval_i64(locals, instr)?.to_string().into(),
			CastF64(instr) => self.eval_f64(locals, instr)?.to_string().into(),
			CastVec(instr_vec, typ) => match typ {
				Type::Unit => unreachable!(),
				Type::Bool => todo!(),
				Type::I64 => {
					let vec =
						unsafe { Rc::from_raw(self.eval_vec(locals, instr_vec, typ)? as *const Vec<u64>) };
					let s = format!("{:?}", vec);
					Rc::into_raw(vec);
					s.into()
				}
				Type::U64 => todo!(),
				Type::F64 => todo!(),
				Type::Str => todo!(),
				Type::Vec(_) => unreachable!(),
			},
		})
	}

	fn eval_vec(&self, locals: &mut [u64], instr: &InstrVec, typ: &Type) -> Result<u64, Interruption<'a>> {
		use InstrKindVec::*;
		Ok(match &*instr.kind {
			Return(instr) => todo!(),
			Variable(idx) => locals[*idx],
			Value(_) => todo!(),
			Call(_) => todo!(),
			Empty => {
				let vec = Rc::new(Vec::<u64>::new());
				Rc::into_raw(vec) as u64
			}
		})
	}

	fn eval_instr(&self, locals: &mut [u64], instr: &Instr) -> Result<u64, Interruption<'a>> {
		use InstrKindStr::*;
		Ok(match &instr.kind {
			InstrKind::Cntrl(instr) => {
				self.eval_unit(locals, instr)?;
				0
			}
			InstrKind::Bool(instr_bool) => todo!(),
			InstrKind::I64(instr) => self.eval_i64(locals, instr)? as u64,
			InstrKind::U64(instr_u64) => todo!(),
			InstrKind::F64(instr_f64) => todo!(),
			InstrKind::Str(instr) => self.eval_str(locals, instr)?.as_ptr() as u64,
			InstrKind::Vec(instr_vec, typ) => self.eval_vec(locals, instr_vec, typ)?,
		})
	}
}
