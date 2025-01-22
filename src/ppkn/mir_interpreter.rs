#![allow(unused)]
use std::mem::transmute;

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
			if let Err(interruption) = self.eval_unit(&mut locals, &stmt) {
				return interruption.result;
			}
		}
		Ok(0)
	}

	fn eval_unit(&self, locals: &mut Vec<u64>, instr: &InstrCntrl) -> Result<(), Interruption<'a>> {
		Ok(match &*instr.kind {
			InstrKindCntrl::Call(f, params) => {
				let mut args = vec![];
				for param in params {
					args.push(self.eval_instr(locals, param)?);
				}
				self.eval_fn(*f, args).map_err(|err| Interruption { result: Err(err) })?;
			}
			InstrKindCntrl::DefI64(id, value) | InstrKindCntrl::SetI64(id, value) => {
				locals[*id] = unsafe { transmute(self.eval_i64(locals, value)?) }
			}
			InstrKindCntrl::DefF64(id, value) | InstrKindCntrl::SetF64(id, value) => {
				locals[*id] = unsafe { transmute(self.eval_f64(locals, value)?) }
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
			InstrKindCntrl::Return(instr) => todo!(),
			InstrKindCntrl::Drop(instr) => todo!(),
		})
	}

	fn eval_i64(&self, locals: &mut Vec<u64>, instr: &InstrI64) -> Result<i64, Interruption<'a>> {
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
			Return(instr) => todo!(),
			Variable(idx) => unsafe { transmute(locals[*idx]) },
			Call(_) => todo!(),
			Value(val) => *val,
		})
	}

	fn eval_f64(&self, locals: &mut Vec<u64>, instr: &InstrF64) -> Result<f64, Interruption<'a>> {
		use InstrKindF64::*;
		Ok(match &*instr.kind {
			Add(lhs, rhs) => self.eval_f64(locals, lhs)? + self.eval_f64(locals, rhs)?,
			Sub(lhs, rhs) => self.eval_f64(locals, lhs)? - self.eval_f64(locals, rhs)?,
			Mult(lhs, rhs) => self.eval_f64(locals, lhs)? * self.eval_f64(locals, rhs)?,
			Div(lhs, rhs) => self.eval_f64(locals, lhs)? / self.eval_f64(locals, rhs)?,
			Return(instr) => todo!(),
			Variable(idx) => unsafe { transmute(locals[*idx]) },
			Call(_) => todo!(),
			Value(val) => *val,
		})
	}

	fn eval_str(&self, locals: &mut Vec<u64>, instr: &InstrStr) -> Result<Str, Interruption<'a>> {
		use InstrKindStr::*;
		Ok(match &*instr.kind {
			Add(instr_str, instr_str1) => todo!(),
			Return(instr) => todo!(),
			Variable(_) => todo!(),
			Value(value) => value.clone(),
			Call(_) => todo!(),
			CastI64(instr) => self.eval_i64(locals, instr)?.to_string().into(),
		})
	}

	fn eval_instr(&self, locals: &mut Vec<u64>, instr: &Instr) -> Result<u64, Interruption<'a>> {
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
		})
	}
}
