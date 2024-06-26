use super::{instr::Instr, valtype::Type, Wasm};
use std::{collections::HashMap, mem::transmute};

impl Wasm {
	pub fn instantiate<'a>(&'a self, environment: Environment<'a>) -> Runtime {
		Runtime::from(&self, environment)
	}
}

type Environment<'a> = HashMap<String, HashMap<String, &'a dyn Fn(Vec<Value>) -> Vec<Value>>>;
// type DynFn = dyn Fn(Vec<Value>) -> Vec<Value>;

pub struct Runtime<'a> {
	module: &'a Wasm,
	environment: Environment<'a>,
	// memory: ...,
	// globals: ...,
}

#[derive(Debug)]
pub enum Value {
	I32(i32),
	I64(i64),
	F32(f32),
	F64(f64),
	Str(String),
}

#[derive(Clone, Copy, Debug)]
pub struct RawValue(u64);

struct State {
	locals: Vec<RawValue>,
	stack: Vec<RawValue>,
}

enum BlockResult {
	Return,
	Break(u32),
	End,
}

impl Runtime<'_> {
	pub fn from<'a>(module: &'a Wasm, environment: Environment<'a>) -> Runtime<'a> {
		Runtime { module, environment }
	}

	pub fn call(&mut self, function_name: String) -> Vec<RawValue> {
		let f = self.module.exports[&function_name];
		let mut state = State { locals: vec![], stack: vec![] };
		self.eval_fn(&mut state, f);
		state.stack
	}

	fn eval_fn(&mut self, state: &mut State, f: usize) {
		if f < self.module.imports.len() {
			let (typ, module, name) = &self.module.imports[f];
			let args = typ.params.iter().map(|t| state.stack.pop().unwrap().to(t)).rev().collect();
			state.stack.extend(self.environment[module][name](args).into_iter().map(Value::raw));
		} else {
			let f = f - self.module.imports.len();
			let (t, locals, body) = &self.module.functions[f];
			let locals = locals.iter().map(|_| RawValue::NONE).collect();
			let mut state = State { locals, stack: vec![] };
			self.eval_block(&mut state, body);
		}
	}

	fn eval_block(&mut self, state: &mut State, block: &Vec<Instr>) -> BlockResult {
		macro_rules! pop {
			($typ:ident) => {
				state.stack.pop().unwrap().$typ()
			};
			() => {
				state.stack.pop().unwrap()
			};
		}
		use Instr::*;
		for instr in block {
			match instr {
				Unreachable => panic!("Reached unreachable code"),
				Nop => (),
				Block(_, block) => match self.eval_block(state, block) {
					BlockResult::Return => return BlockResult::Return,
					BlockResult::Break(x) => todo!(),
					BlockResult::End => (),
				},
				Loop(_, block) => match self.eval_block(state, block) {
					BlockResult::Return => return BlockResult::Return,
					BlockResult::Break(x) => todo!(),
					BlockResult::End => (),
				},
				IfElse(_, then, els) => {
					let block = if pop!(u32) != 0 { then } else { els };
					match self.eval_block(state, block) {
						BlockResult::Return => return BlockResult::Return,
						BlockResult::Break(x) => todo!(),
						BlockResult::End => (),
					}
				}
				Else => unreachable!(),
				End => unreachable!(),
				Br(depth) => return BlockResult::Break(*depth),
				BrIf(depth) => {
					if state.stack.pop().unwrap().u32() != 0 {
						return BlockResult::Break(*depth);
					}
				}
				BrTable(_, _) => todo!(),
				Return => return BlockResult::Return,
				Call(f) => self.eval_fn(state, *f as usize),
				CallIndirrect(_) => todo!(),
				Null(_) => todo!(),
				IsNull => todo!(),
				Func(_) => todo!(),
				Drop => _ = pop!(),
				Select => {
					let i = if pop!(u32) != 0 { 1 } else { 0 };
					let options = [pop!(), pop!()];
					state.stack.push(options[i]);
				}
				SelectVec(_) => todo!(),
				LocalGet(i) => state.stack.push(state.locals[*i as usize]),
				LocalSet(i) => state.locals[*i as usize] = pop!(),
				LocalTee(i) => state.locals[*i as usize] = *state.stack.last().unwrap(),
				GlobalGet(_) => todo!(),
				GlobalSet(_) => todo!(),
				ConstI32(value) => state.stack.push(RawValue::from(*value)),
				ConstI64(value) => state.stack.push(RawValue::from(*value)),
				ConstF32(value) => state.stack.push(RawValue::from(*value)),
				ConstF64(value) => state.stack.push(RawValue::from(*value)),
				i32_eqz => todo!(),
				i32_eq => todo!(),
				i32_ne => todo!(),
				i32_lt_s => todo!(),
				i32_lt_u => todo!(),
				i32_gt_s => todo!(),
				i32_gt_u => todo!(),
				i32_le_s => todo!(),
				i32_le_u => todo!(),
				i32_ge_s => todo!(),
				i32_ge_u => todo!(),
				i64_eqz => todo!(),
				i64_eq => todo!(),
				i64_ne => todo!(),
				i64_lt_s => todo!(),
				i64_lt_u => todo!(),
				i64_gt_s => todo!(),
				i64_gt_u => todo!(),
				i64_le_s => todo!(),
				i64_le_u => todo!(),
				i64_ge_s => todo!(),
				i64_ge_u => todo!(),
				f32_eq => todo!(),
				f32_ne => todo!(),
				f32_lt => todo!(),
				f32_gt => todo!(),
				f32_le => todo!(),
				f32_ge => todo!(),
				f64_eq => todo!(),
				f64_ne => todo!(),
				f64_lt => todo!(),
				f64_gt => todo!(),
				f64_le => todo!(),
				f64_ge => todo!(),
				i32_clz => todo!(),
				i32_ctz => todo!(),
				i32_popcnt => todo!(),
				i32_add => todo!(),
				i32_sub => todo!(),
				i32_mul => todo!(),
				i32_div_s => todo!(),
				i32_div_u => todo!(),
				i32_rem_s => todo!(),
				i32_rem_u => todo!(),
				i32_and => todo!(),
				i32_or => todo!(),
				i32_xor => todo!(),
				i32_shl => todo!(),
				i32_shr_s => todo!(),
				i32_shr_u => todo!(),
				i32_rotl => todo!(),
				i32_rotr => todo!(),
				i64_clz => todo!(),
				i64_ctz => todo!(),
				i64_popcnt => todo!(),
				i64_add => todo!(),
				i64_sub => todo!(),
				i64_mul => todo!(),
				i64_div_s => todo!(),
				i64_div_u => todo!(),
				i64_rem_s => todo!(),
				i64_rem_u => todo!(),
				i64_and => todo!(),
				i64_or => todo!(),
				i64_xor => todo!(),
				i64_shl => todo!(),
				i64_shr_s => todo!(),
				i64_shr_u => todo!(),
				i64_rotl => todo!(),
				i64_rotr => todo!(),
				f32_abs => todo!(),
				f32_neg => todo!(),
				f32_ceil => todo!(),
				f32_floor => todo!(),
				f32_trunc => todo!(),
				f32_nearest => todo!(),
				f32_sqrt => todo!(),
				f32_add => todo!(),
				f32_sub => todo!(),
				f32_mul => todo!(),
				f32_div => todo!(),
				f32_min => todo!(),
				f32_max => todo!(),
				f32_copysign => todo!(),
				f64_abs => todo!(),
				f64_neg => todo!(),
				f64_ceil => todo!(),
				f64_floor => todo!(),
				f64_trunc => todo!(),
				f64_nearest => todo!(),
				f64_sqrt => todo!(),
				f64_add => todo!(),
				f64_sub => todo!(),
				f64_mul => todo!(),
				f64_div => todo!(),
				f64_min => todo!(),
				f64_max => todo!(),
				f64_copysign => todo!(),
				i32_wrap_i64 => todo!(),
				i32_trunc_f32_s => todo!(),
				i32_trunc_f32_u => todo!(),
				i32_trunc_f64_s => todo!(),
				i32_trunc_f64_u => todo!(),
				i64_extend_i32_s => todo!(),
				i64_extend_i32_u => todo!(),
				i64_trunc_f32_s => todo!(),
				i64_trunc_f32_u => todo!(),
				i64_trunc_f64_s => todo!(),
				i64_trunc_f64_u => todo!(),
				f32_convert_i32_s => todo!(),
				f32_convert_i32_u => todo!(),
				f32_convert_i64_s => todo!(),
				f32_convert_i64_u => todo!(),
				f32_demote_f64 => todo!(),
				f64_convert_i32_s => todo!(),
				f64_convert_i32_u => todo!(),
				f64_convert_i64_s => todo!(),
				f64_convert_i64_u => todo!(),
				f64_promote_f32 => todo!(),
				i32_reinterpret_f32 => todo!(),
				i64_reinterpret_f64 => todo!(),
				f32_reinterpret_i32 => todo!(),
				f64_reinterpret_i64 => todo!(),
				i32_extend8_s => todo!(),
				i32_extend16_s => todo!(),
				i64_extend8_s => todo!(),
				i64_extend16_s => todo!(),
				i64_extend32_s => todo!(),
			}
		}
		BlockResult::End
	}
}

impl Value {
	pub fn raw(self) -> RawValue {
		RawValue::from(self)
	}
}

impl RawValue {
	pub const NONE: RawValue = RawValue(0);
	pub fn i32(self) -> i32 {
		self.0 as i32
	}
	pub fn i64(self) -> i64 {
		self.0 as i64
	}
	pub fn u32(self) -> u32 {
		self.0 as u32
	}
	pub fn u64(self) -> u64 {
		self.0 as u64
	}
	pub fn f32(self) -> f32 {
		unsafe { transmute(self.u32()) }
	}
	pub fn f64(self) -> f64 {
		unsafe { transmute(self.u64()) }
	}
	pub fn to(self, typ: &Type) -> Value {
		match typ {
			Type::None => todo!(),
			Type::Str24 => todo!(),
			Type::Str16 => todo!(),
			Type::Str8 => todo!(),
			Type::Str => todo!(),
			Type::Ref => todo!(),
			Type::Fun => todo!(),
			Type::Vec => todo!(),
			Type::F64 => Value::F64(self.f64()),
			Type::F32 => Value::F32(self.f32()),
			Type::I64 => Value::I64(self.i64()),
			Type::I32 => Value::I32(self.i32()),
		}
	}
}

impl std::fmt::Display for RawValue {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}", self.i64())
	}
}

impl std::convert::From<i32> for RawValue {
	fn from(value: i32) -> Self {
		unsafe { RawValue(transmute(value as i64)) }
	}
}

impl std::convert::From<i64> for RawValue {
	fn from(value: i64) -> Self {
		unsafe { RawValue(transmute(value)) }
	}
}

impl std::convert::From<f32> for RawValue {
	fn from(value: f32) -> Self {
		unsafe { RawValue::from(transmute::<f32, i32>(value)) }
	}
}

impl std::convert::From<f64> for RawValue {
	fn from(value: f64) -> Self {
		unsafe { RawValue(transmute(value)) }
	}
}

impl std::convert::From<Value> for RawValue {
	fn from(value: Value) -> Self {
		match value {
			Value::I32(value) => Self::from(value),
			Value::I64(value) => Self::from(value),
			Value::F32(value) => Self::from(value),
			Value::F64(value) => Self::from(value),
			Value::Str(value) => todo!(),
		}
	}
}
