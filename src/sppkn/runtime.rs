#![allow(unused)]

use std::{collections::HashMap, fmt::Display};

use super::{
	error::{Error, PpknErrorKind},
	lir::{Bytecode, Func, Op},
};

const LOGGING: bool = false;

impl Bytecode {
	pub fn create_runtime(&self) -> Runtime {
		Runtime::new(self)
	}
}

pub struct Runtime<'a> {
	program: &'a Bytecode,
	function: usize,
	objects: Objects,
	globals: Vec<u64>,
	stack: Vec<u64>,
	bp: usize,
}

struct Objects {
	values: Vec<Object>,
}

#[derive(Debug, Clone)]
enum Object {
	String(Box<str>),
}

impl Objects {
	fn new() -> Self {
		Self { values: vec![Object::String("".into())] }
	}

	fn insert(&mut self, object: Object) -> u64 {
		let handle = self.values.len();
		self.values.push(object);
		handle as u64
	}

	fn get(&self, handle: u64) -> Object {
		self.values[handle as usize].clone()
	}
}

impl Display for Object {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Object::String(string) => write!(f, "{}", string),
		}
	}
}

impl<'a> Runtime<'a> {
	pub fn new(program: &'a Bytecode) -> Self {
		let objects = Objects::new();
		let mut globals = Vec::with_capacity(program.globals.len());
		for &global in &program.globals {
			globals.push(global);
		}
		Runtime { program, function: 0xABEBA, objects, globals, stack: vec![0x54484520454e44], bp: 1 }
	}

	pub fn run(&mut self) -> Result<(), Error> {
		let (main, _) = self.program.functions.get_index(0).unwrap();
		self.eval_function(main)
	}

	fn error(&self, location: u32, message: &str) -> Result<(), Error> {
		let func = &self.program.functions[self.function];
		let error = Error {
			module: func.module.clone(),
			cause_location: (location, location + 1),
			message: message.into(),
			kind: PpknErrorKind::RuntimeError,
		};
		Err(error)
	}

	fn save_frame(&mut self, ip: usize, func: usize) -> (&'a Func, &'a Vec<Op>, usize) {
		let function = &self.program.functions[func];
		let old_bp = self.bp;
		self.bp = self.stack.len() - function.signature.parameters.len();
		self.stack.extend(vec![0; function.locals.len() - function.signature.parameters.len()]);
		self.stack.push(self.function as u64);
		self.stack.push(old_bp as u64);
		self.stack.push(ip as u64);
		self.function = func;
		let code = &function.code;
		(function, code, 0)
	}

	fn load_frame(&mut self) -> (&'a Func, &'a Vec<Op>, usize) {
		let function = &self.program.functions[self.function];
		let results = self.stack.split_off(self.stack.len() - function.signature.results.len());
		let old_stack_top = self.bp;
		let frame_location = old_stack_top + function.locals.len();
		self.function = self.stack[frame_location + 0] as usize;
		self.bp = self.stack[frame_location + 1] as usize;
		let ip = self.stack[frame_location + 2] as usize;
		self.stack.truncate(old_stack_top);
		self.stack.extend(results);
		let function = &self.program.functions[self.function];
		let code = &function.code;
		(function, code, ip)
	}

	fn call_import(&mut self, memory: &mut Vec<u8>, import_id: usize) {
		let import = &self.program.imports[import_id];
		match (import.namespace.as_ref(), import.func_name.as_ref()) {
			("string", "new_utf8") => {
				let length = self.stack.pop().unwrap() as usize;
				let ptr = self.stack.pop().unwrap() as usize;
				let string = String::from_utf8_lossy(&memory[ptr..ptr + length]);
				let handle = self.objects.insert(Object::String(string.into()));
				self.stack.push(handle);
			}
			("string", "print") => {
				let handle = self.stack.pop().unwrap();
				let string = self.objects.get(handle);
				println!("{}", string);
			}
			call => unimplemented!("{:?}", call),
		}
	}

	fn eval_function(&mut self, function: &str) -> Result<(), Error> {
		let program = self.program;
		let mut memory = self.program.data.clone();
		memory.resize((1 << 16) - memory.len(), 0);
		// TODO: unite stacks
		// let mut call_stack = vec![];
		let mut ip = 0;
		let entry_function = self.program.functions.get_index_of(function).unwrap();
		let mut function = &self.program.functions[entry_function];
		let mut code = &function.code;
		self.function = entry_function;
		// let code =
		loop {
			if LOGGING {
				// eprint!("{:?} > ", self.stack);
				// eprintln!("IP={:02} OP={:?}", ip, &code[ip]);
				eprintln!("{:02} {:<13} \x1b[90m{:?}\x1b[0m", ip, format!("{:?}", &code[ip]), self.stack);
			}
			use Op::*;
			match &code[ip] {
				Unreachable => todo!(),
				Nop => {}
				Block(block_type, _) => todo!(),
				Loop(block_type, _) => todo!(),
				IfThen(_, size) => {
					if self.stack.pop().unwrap() == 0 {
						ip += size;
					}
				}
				Else(size) => ip += size - 1,
				End => {
					if self.function == entry_function {
						return Ok(());
					} else {
						(function, code, ip) = self.load_frame();
					}
				}
				Br(_) => todo!(),
				BrIf(_) => todo!(),
				JumpInto(items) => todo!(),
				Return => todo!(),
				CallFunc(idx) => {
					(function, code, ip) = self.save_frame(ip, *idx);
					continue;
				}
				CallImport(index) => self.call_import(&mut memory, *index),
				CallIndirect(_) => todo!(),
				Drop => _ = self.stack.pop(),
				Select(value_type) => todo!(),
				GlobalGet(idx) => self.stack.push(self.globals[*idx]),
				GlobalSet(idx) => self.globals[*idx] = self.stack.pop().unwrap(),
				LocalGet(idx) => self.stack.push(self.stack[self.bp + idx]),
				LocalSet(idx) => self.stack[self.bp + idx] = self.stack.pop().unwrap(),
				LocalTee(_) => todo!(),
				MemorySize => todo!(),
				MemoryGrow => todo!(),
				MemoryCopy => {
					let size = self.stack.pop().unwrap() as usize;
					let src = self.stack.pop().unwrap() as usize;
					let dst = self.stack.pop().unwrap() as usize;
					memory.copy_within(src..src + size, dst);
				}
				// TODO: shifts or rotations should respect bit-sizes of arguments
				F32Const(value) => {
					let result = *value;
					self.stack.push(result.to_bits() as u64);
				}
				F32Load(offset) => {
					let i: usize = self.stack.pop().unwrap() as usize + offset;
					let result = f32::from_le_bytes(memory[i..i + 4].try_into().unwrap());
					self.stack.push(result.to_bits() as u64);
				}
				F32Store(offset) => {
					let x: f32 = f32::from_bits(self.stack.pop().unwrap() as u32);
					let i: usize = self.stack.pop().unwrap() as usize + offset;
					memory[i..i + 4].copy_from_slice(&x.to_le_bytes());
				}
				F32Abs => {
					let x: f32 = f32::from_bits(self.stack.pop().unwrap() as u32);
					let result = x.abs();
					self.stack.push(result.to_bits() as u64);
				}
				F32Ceil => {
					let x: f32 = f32::from_bits(self.stack.pop().unwrap() as u32);
					let result = x.ceil();
					self.stack.push(result.to_bits() as u64);
				}
				F32Floor => {
					let x: f32 = f32::from_bits(self.stack.pop().unwrap() as u32);
					let result = x.floor();
					self.stack.push(result.to_bits() as u64);
				}
				F32Nearest => {
					let x: f32 = f32::from_bits(self.stack.pop().unwrap() as u32);
					let result = x.round();
					self.stack.push(result.to_bits() as u64);
				}
				F32Neg => {
					let x: f32 = f32::from_bits(self.stack.pop().unwrap() as u32);
					let result = -x;
					self.stack.push(result.to_bits() as u64);
				}
				F32Sqrt => {
					let x: f32 = f32::from_bits(self.stack.pop().unwrap() as u32);
					let result = x.sqrt();
					self.stack.push(result.to_bits() as u64);
				}
				F32Trunc => {
					let x: f32 = f32::from_bits(self.stack.pop().unwrap() as u32);
					let result = x.trunc();
					self.stack.push(result.to_bits() as u64);
				}
				F32Copysign => {
					let y: f32 = f32::from_bits(self.stack.pop().unwrap() as u32);
					let x: f32 = f32::from_bits(self.stack.pop().unwrap() as u32);
					let result = x.copysign(y);
					self.stack.push(result.to_bits() as u64);
				}
				F32Add => {
					let y: f32 = f32::from_bits(self.stack.pop().unwrap() as u32);
					let x: f32 = f32::from_bits(self.stack.pop().unwrap() as u32);
					let result = x + y;
					self.stack.push(result.to_bits() as u64);
				}
				F32Div => {
					let y: f32 = f32::from_bits(self.stack.pop().unwrap() as u32);
					let x: f32 = f32::from_bits(self.stack.pop().unwrap() as u32);
					let result = x / y;
					self.stack.push(result.to_bits() as u64);
				}
				F32Max => {
					let y: f32 = f32::from_bits(self.stack.pop().unwrap() as u32);
					let x: f32 = f32::from_bits(self.stack.pop().unwrap() as u32);
					let result = x.max(y);
					self.stack.push(result.to_bits() as u64);
				}
				F32Min => {
					let y: f32 = f32::from_bits(self.stack.pop().unwrap() as u32);
					let x: f32 = f32::from_bits(self.stack.pop().unwrap() as u32);
					let result = x.min(y);
					self.stack.push(result.to_bits() as u64);
				}
				F32Mul => {
					let y: f32 = f32::from_bits(self.stack.pop().unwrap() as u32);
					let x: f32 = f32::from_bits(self.stack.pop().unwrap() as u32);
					let result = x * y;
					self.stack.push(result.to_bits() as u64);
				}
				F32Sub => {
					let y: f32 = f32::from_bits(self.stack.pop().unwrap() as u32);
					let x: f32 = f32::from_bits(self.stack.pop().unwrap() as u32);
					let result = x - y;
					self.stack.push(result.to_bits() as u64);
				}
				F32Eq => {
					let y: f32 = f32::from_bits(self.stack.pop().unwrap() as u32);
					let x: f32 = f32::from_bits(self.stack.pop().unwrap() as u32);
					let result = x == y;
					self.stack.push(result as u64);
				}
				F32Ge => {
					let y: f32 = f32::from_bits(self.stack.pop().unwrap() as u32);
					let x: f32 = f32::from_bits(self.stack.pop().unwrap() as u32);
					let result = x >= y;
					self.stack.push(result as u64);
				}
				F32Gt => {
					let y: f32 = f32::from_bits(self.stack.pop().unwrap() as u32);
					let x: f32 = f32::from_bits(self.stack.pop().unwrap() as u32);
					let result = x > y;
					self.stack.push(result as u64);
				}
				F32Le => {
					let y: f32 = f32::from_bits(self.stack.pop().unwrap() as u32);
					let x: f32 = f32::from_bits(self.stack.pop().unwrap() as u32);
					let result = x <= y;
					self.stack.push(result as u64);
				}
				F32Lt => {
					let y: f32 = f32::from_bits(self.stack.pop().unwrap() as u32);
					let x: f32 = f32::from_bits(self.stack.pop().unwrap() as u32);
					let result = x < y;
					self.stack.push(result as u64);
				}
				F32Ne => {
					let y: f32 = f32::from_bits(self.stack.pop().unwrap() as u32);
					let x: f32 = f32::from_bits(self.stack.pop().unwrap() as u32);
					let result = x != y;
					self.stack.push(result as u64);
				}
				I32Const(value) => {
					let result = *value;
					self.stack.push(result as u64);
				}
				I32Load(offset) => {
					let i: usize = self.stack.pop().unwrap() as usize + offset;
					let result = i32::from_le_bytes(memory[i..i + 4].try_into().unwrap());
					self.stack.push(result as u64);
				}
				I32Store(offset) => {
					let x: i32 = self.stack.pop().unwrap() as i32;
					let i: usize = self.stack.pop().unwrap() as usize + offset;
					memory[i..i + 4].copy_from_slice(&x.to_le_bytes());
				}
				I32And => {
					let y: i32 = self.stack.pop().unwrap() as i32;
					let x: i32 = self.stack.pop().unwrap() as i32;
					let result = x & y;
					self.stack.push(result as u64);
				}
				I32Or => {
					let y: i32 = self.stack.pop().unwrap() as i32;
					let x: i32 = self.stack.pop().unwrap() as i32;
					let result = x | y;
					self.stack.push(result as u64);
				}
				I32Xor => {
					let y: i32 = self.stack.pop().unwrap() as i32;
					let x: i32 = self.stack.pop().unwrap() as i32;
					let result = x ^ y;
					self.stack.push(result as u64);
				}
				I32Add(location) => {
					let y: i32 = self.stack.pop().unwrap() as i32;
					let x: i32 = self.stack.pop().unwrap() as i32;
					let result = x.checked_add(y).unwrap();
					self.stack.push(result as u64);
				}
				I32Div(location) => {
					let y: i32 = self.stack.pop().unwrap() as i32;
					let x: i32 = self.stack.pop().unwrap() as i32;
					let result = x.checked_div(y).unwrap();
					self.stack.push(result as u64);
				}
				I32Mul(location) => {
					let y: i32 = self.stack.pop().unwrap() as i32;
					let x: i32 = self.stack.pop().unwrap() as i32;
					let result = x.checked_mul(y).unwrap();
					self.stack.push(result as u64);
				}
				I32Rem(location) => {
					let y: i32 = self.stack.pop().unwrap() as i32;
					let x: i32 = self.stack.pop().unwrap() as i32;
					let result = x.checked_rem(y).unwrap();
					self.stack.push(result as u64);
				}
				I32Rotl => {
					let y: u32 = self.stack.pop().unwrap() as u32;
					let x: i32 = self.stack.pop().unwrap() as i32;
					let result = x.rotate_left(y);
					self.stack.push(result as u64);
				}
				I32Rotr => {
					let y: u32 = self.stack.pop().unwrap() as u32;
					let x: i32 = self.stack.pop().unwrap() as i32;
					let result = x.rotate_right(y);
					self.stack.push(result as u64);
				}
				I32Shl(location) => {
					let y: u32 = self.stack.pop().unwrap() as u32;
					let x: i32 = self.stack.pop().unwrap() as i32;
					let result = x.checked_shl(y).unwrap();
					self.stack.push(result as u64);
				}
				I32Shr(location) => {
					let y: u32 = self.stack.pop().unwrap() as u32;
					let x: i32 = self.stack.pop().unwrap() as i32;
					let result = x.checked_shr(y).unwrap();
					self.stack.push(result as u64);
				}
				I32Sub(location) => {
					let y: i32 = self.stack.pop().unwrap() as i32;
					let x: i32 = self.stack.pop().unwrap() as i32;
					let result = x.checked_sub(y).unwrap();
					self.stack.push(result as u64);
				}
				I32Eqz => {
					let x: i32 = self.stack.pop().unwrap() as i32;
					let result = x == 0;
					self.stack.push(result as u64);
				}
				I32Eq => {
					let y: i32 = self.stack.pop().unwrap() as i32;
					let x: i32 = self.stack.pop().unwrap() as i32;
					let result = x != y;
					self.stack.push(result as u64);
				}
				I32Ge => {
					let y: i32 = self.stack.pop().unwrap() as i32;
					let x: i32 = self.stack.pop().unwrap() as i32;
					let result = x >= y;
					self.stack.push(result as u64);
				}
				I32Gt => {
					let y: i32 = self.stack.pop().unwrap() as i32;
					let x: i32 = self.stack.pop().unwrap() as i32;
					let result = x > y;
					self.stack.push(result as u64);
				}
				I32Le => {
					let y: i32 = self.stack.pop().unwrap() as i32;
					let x: i32 = self.stack.pop().unwrap() as i32;
					let result = x <= y;
					self.stack.push(result as u64);
				}
				I32Lt => {
					let y: i32 = self.stack.pop().unwrap() as i32;
					let x: i32 = self.stack.pop().unwrap() as i32;
					let result = x < y;
					self.stack.push(result as u64);
				}
				I32Ne => {
					let y: i32 = self.stack.pop().unwrap() as i32;
					let x: i32 = self.stack.pop().unwrap() as i32;
					let result = x != y;
					self.stack.push(result as u64);
				}
				U32Const(value) => {
					let result = *value;
					self.stack.push(result as u64);
				}
				U32Load(offset) => {
					let i: usize = self.stack.pop().unwrap() as usize + offset;
					let result = u32::from_le_bytes(memory[i..i + 4].try_into().unwrap());
					self.stack.push(result as u64);
				}
				U32Store(offset) => {
					let x: u32 = self.stack.pop().unwrap() as u32;
					let i: usize = self.stack.pop().unwrap() as usize + offset;
					memory[i..i + 4].copy_from_slice(&x.to_le_bytes());
				}
				U32And => {
					let y: u32 = self.stack.pop().unwrap() as u32;
					let x: u32 = self.stack.pop().unwrap() as u32;
					let result = x & y;
					self.stack.push(result as u64);
				}
				U32Or => {
					let y: u32 = self.stack.pop().unwrap() as u32;
					let x: u32 = self.stack.pop().unwrap() as u32;
					let result = x | y;
					self.stack.push(result as u64);
				}
				U32Xor => {
					let y: u32 = self.stack.pop().unwrap() as u32;
					let x: u32 = self.stack.pop().unwrap() as u32;
					let result = x ^ y;
					self.stack.push(result as u64);
				}
				U32Add(location) => {
					let y: u32 = self.stack.pop().unwrap() as u32;
					let x: u32 = self.stack.pop().unwrap() as u32;
					let result = x.checked_add(y).unwrap();
					self.stack.push(result as u64);
				}
				U32Div(location) => {
					let y: u32 = self.stack.pop().unwrap() as u32;
					let x: u32 = self.stack.pop().unwrap() as u32;
					let result = x.checked_div(y).unwrap();
					self.stack.push(result as u64);
				}
				U32Mul(location) => {
					let y: u32 = self.stack.pop().unwrap() as u32;
					let x: u32 = self.stack.pop().unwrap() as u32;
					let result = x.checked_mul(y).unwrap();
					self.stack.push(result as u64);
				}
				U32Rem(location) => {
					let y: u32 = self.stack.pop().unwrap() as u32;
					let x: u32 = self.stack.pop().unwrap() as u32;
					let result = x.checked_rem(y).unwrap();
					self.stack.push(result as u64);
				}
				U32Rotl => {
					let y: u32 = self.stack.pop().unwrap() as u32;
					let x: u32 = self.stack.pop().unwrap() as u32;
					let result = x.rotate_left(y);
					self.stack.push(result as u64);
				}
				U32Rotr => {
					let y: u32 = self.stack.pop().unwrap() as u32;
					let x: u32 = self.stack.pop().unwrap() as u32;
					let result = x.rotate_right(y);
					self.stack.push(result as u64);
				}
				U32Shl(location) => {
					let y: u32 = self.stack.pop().unwrap() as u32;
					let x: u32 = self.stack.pop().unwrap() as u32;
					let result = x.checked_shl(y).unwrap();
					self.stack.push(result as u64);
				}
				U32Shr(location) => {
					let y: u32 = self.stack.pop().unwrap() as u32;
					let x: u32 = self.stack.pop().unwrap() as u32;
					let result = x.checked_shr(y).unwrap();
					self.stack.push(result as u64);
				}
				U32Sub(location) => {
					let y: u32 = self.stack.pop().unwrap() as u32;
					let x: u32 = self.stack.pop().unwrap() as u32;
					let result = x.checked_sub(y).unwrap();
					self.stack.push(result as u64);
				}
				U32Eqz => {
					let x: u32 = self.stack.pop().unwrap() as u32;
					let result = x == 0;
					self.stack.push(result as u64);
				}
				U32Eq => {
					let y: u32 = self.stack.pop().unwrap() as u32;
					let x: u32 = self.stack.pop().unwrap() as u32;
					let result = x != y;
					self.stack.push(result as u64);
				}
				U32Ge => {
					let y: u32 = self.stack.pop().unwrap() as u32;
					let x: u32 = self.stack.pop().unwrap() as u32;
					let result = x >= y;
					self.stack.push(result as u64);
				}
				U32Gt => {
					let y: u32 = self.stack.pop().unwrap() as u32;
					let x: u32 = self.stack.pop().unwrap() as u32;
					let result = x > y;
					self.stack.push(result as u64);
				}
				U32Le => {
					let y: u32 = self.stack.pop().unwrap() as u32;
					let x: u32 = self.stack.pop().unwrap() as u32;
					let result = x <= y;
					self.stack.push(result as u64);
				}
				U32Lt => {
					let y: u32 = self.stack.pop().unwrap() as u32;
					let x: u32 = self.stack.pop().unwrap() as u32;
					let result = x < y;
					self.stack.push(result as u64);
				}
				U32Ne => {
					let y: u32 = self.stack.pop().unwrap() as u32;
					let x: u32 = self.stack.pop().unwrap() as u32;
					let result = x != y;
					self.stack.push(result as u64);
				}
				F64Const(value) => {
					let result = *value;
					self.stack.push(result.to_bits() as u64);
				}
				F64Load(offset) => {
					let i: usize = self.stack.pop().unwrap() as usize + offset;
					let result = f64::from_le_bytes(memory[i..i + 8].try_into().unwrap());
					self.stack.push(result.to_bits() as u64);
				}
				F64Store(offset) => {
					let x: f64 = f64::from_bits(self.stack.pop().unwrap());
					let i: usize = self.stack.pop().unwrap() as usize + offset;
					memory[i..i + 8].copy_from_slice(&x.to_le_bytes());
				}
				F64Abs => {
					let x: f64 = f64::from_bits(self.stack.pop().unwrap());
					let result = x.abs();
					self.stack.push(result.to_bits() as u64);
				}
				F64Ceil => {
					let x: f64 = f64::from_bits(self.stack.pop().unwrap());
					let result = x.ceil();
					self.stack.push(result.to_bits() as u64);
				}
				F64Floor => {
					let x: f64 = f64::from_bits(self.stack.pop().unwrap());
					let result = x.floor();
					self.stack.push(result.to_bits() as u64);
				}
				F64Nearest => {
					let x: f64 = f64::from_bits(self.stack.pop().unwrap());
					let result = x.round();
					self.stack.push(result.to_bits() as u64);
				}
				F64Neg => {
					let x: f64 = f64::from_bits(self.stack.pop().unwrap());
					let result = -x;
					self.stack.push(result.to_bits() as u64);
				}
				F64Sqrt => {
					let x: f64 = f64::from_bits(self.stack.pop().unwrap());
					let result = x.sqrt();
					self.stack.push(result.to_bits() as u64);
				}
				F64Trunc => {
					let x: f64 = f64::from_bits(self.stack.pop().unwrap());
					let result = x.trunc();
					self.stack.push(result.to_bits() as u64);
				}
				F64Copysign => {
					let y: f64 = f64::from_bits(self.stack.pop().unwrap());
					let x: f64 = f64::from_bits(self.stack.pop().unwrap());
					let result = x.copysign(y);
					self.stack.push(result.to_bits() as u64);
				}
				F64Add => {
					let y: f64 = f64::from_bits(self.stack.pop().unwrap());
					let x: f64 = f64::from_bits(self.stack.pop().unwrap());
					let result = x + y;
					self.stack.push(result.to_bits() as u64);
				}
				F64Div => {
					let y: f64 = f64::from_bits(self.stack.pop().unwrap());
					let x: f64 = f64::from_bits(self.stack.pop().unwrap());
					let result = x / y;
					self.stack.push(result.to_bits() as u64);
				}
				F64Max => {
					let y: f64 = f64::from_bits(self.stack.pop().unwrap());
					let x: f64 = f64::from_bits(self.stack.pop().unwrap());
					let result = x.max(y);
					self.stack.push(result.to_bits() as u64);
				}
				F64Min => {
					let y: f64 = f64::from_bits(self.stack.pop().unwrap());
					let x: f64 = f64::from_bits(self.stack.pop().unwrap());
					let result = x.min(y);
					self.stack.push(result.to_bits() as u64);
				}
				F64Mul => {
					let y: f64 = f64::from_bits(self.stack.pop().unwrap());
					let x: f64 = f64::from_bits(self.stack.pop().unwrap());
					let result = x * y;
					self.stack.push(result.to_bits() as u64);
				}
				F64Sub => {
					let y: f64 = f64::from_bits(self.stack.pop().unwrap());
					let x: f64 = f64::from_bits(self.stack.pop().unwrap());
					let result = x - y;
					self.stack.push(result.to_bits() as u64);
				}
				F64Eq => {
					let y: f64 = f64::from_bits(self.stack.pop().unwrap());
					let x: f64 = f64::from_bits(self.stack.pop().unwrap());
					let result = x == y;
					self.stack.push(result as u64);
				}
				F64Ge => {
					let y: f64 = f64::from_bits(self.stack.pop().unwrap());
					let x: f64 = f64::from_bits(self.stack.pop().unwrap());
					let result = x >= y;
					self.stack.push(result as u64);
				}
				F64Gt => {
					let y: f64 = f64::from_bits(self.stack.pop().unwrap());
					let x: f64 = f64::from_bits(self.stack.pop().unwrap());
					let result = x > y;
					self.stack.push(result as u64);
				}
				F64Le => {
					let y: f64 = f64::from_bits(self.stack.pop().unwrap());
					let x: f64 = f64::from_bits(self.stack.pop().unwrap());
					let result = x <= y;
					self.stack.push(result as u64);
				}
				F64Lt => {
					let y: f64 = f64::from_bits(self.stack.pop().unwrap());
					let x: f64 = f64::from_bits(self.stack.pop().unwrap());
					let result = x < y;
					self.stack.push(result as u64);
				}
				F64Ne => {
					let y: f64 = f64::from_bits(self.stack.pop().unwrap());
					let x: f64 = f64::from_bits(self.stack.pop().unwrap());
					let result = x != y;
					self.stack.push(result as u64);
				}
				I64Const(value) => {
					let result = *value;
					self.stack.push(result as u64);
				}
				I64Load(offset) => {
					let i: usize = self.stack.pop().unwrap() as usize + offset;
					let result = i64::from_le_bytes(memory[i..i + 8].try_into().unwrap());
					self.stack.push(result as u64);
				}
				I64Store(offset) => {
					let x: i64 = self.stack.pop().unwrap() as i64;
					let i: usize = self.stack.pop().unwrap() as usize + offset;
					memory[i..i + 8].copy_from_slice(&x.to_le_bytes());
				}
				I64And => {
					let y: i64 = self.stack.pop().unwrap() as i64;
					let x: i64 = self.stack.pop().unwrap() as i64;
					let result = x & y;
					self.stack.push(result as u64);
				}
				I64Or => {
					let y: i64 = self.stack.pop().unwrap() as i64;
					let x: i64 = self.stack.pop().unwrap() as i64;
					let result = x | y;
					self.stack.push(result as u64);
				}
				I64Xor => {
					let y: i64 = self.stack.pop().unwrap() as i64;
					let x: i64 = self.stack.pop().unwrap() as i64;
					let result = x ^ y;
					self.stack.push(result as u64);
				}
				I64Add(location) => {
					let y: i64 = self.stack.pop().unwrap() as i64;
					let x: i64 = self.stack.pop().unwrap() as i64;
					let result = x.checked_add(y).unwrap();
					self.stack.push(result as u64);
				}
				I64Div(location) => {
					let y: i64 = self.stack.pop().unwrap() as i64;
					let x: i64 = self.stack.pop().unwrap() as i64;
					let result = x.checked_div(y).unwrap();
					self.stack.push(result as u64);
				}
				I64Mul(location) => {
					let y: i64 = self.stack.pop().unwrap() as i64;
					let x: i64 = self.stack.pop().unwrap() as i64;
					let result = x.checked_mul(y).unwrap();
					self.stack.push(result as u64);
				}
				I64Rem(location) => {
					let y: i64 = self.stack.pop().unwrap() as i64;
					let x: i64 = self.stack.pop().unwrap() as i64;
					let result = x.checked_rem(y).unwrap();
					self.stack.push(result as u64);
				}
				I64Rotl => {
					let y: u32 = self.stack.pop().unwrap() as u32;
					let x: i64 = self.stack.pop().unwrap() as i64;
					let result = x.rotate_left(y);
					self.stack.push(result as u64);
				}
				I64Rotr => {
					let y: u32 = self.stack.pop().unwrap() as u32;
					let x: i64 = self.stack.pop().unwrap() as i64;
					let result = x.rotate_right(y);
					self.stack.push(result as u64);
				}
				I64Shl(location) => {
					let y: u32 = self.stack.pop().unwrap() as u32;
					let x: i64 = self.stack.pop().unwrap() as i64;
					let result = x.checked_shl(y).unwrap();
					self.stack.push(result as u64);
				}
				I64Shr(location) => {
					let y: u32 = self.stack.pop().unwrap() as u32;
					let x: i64 = self.stack.pop().unwrap() as i64;
					let result = x.checked_shr(y).unwrap();
					self.stack.push(result as u64);
				}
				I64Sub(location) => {
					let y: i64 = self.stack.pop().unwrap() as i64;
					let x: i64 = self.stack.pop().unwrap() as i64;
					let result = x.checked_sub(y).unwrap();
					self.stack.push(result as u64);
				}
				I64Eqz => {
					let x: i64 = self.stack.pop().unwrap() as i64;
					let result = x == 0;
					self.stack.push(result as u64);
				}
				I64Eq => {
					let y: i64 = self.stack.pop().unwrap() as i64;
					let x: i64 = self.stack.pop().unwrap() as i64;
					let result = x != y;
					self.stack.push(result as u64);
				}
				I64Ge => {
					let y: i64 = self.stack.pop().unwrap() as i64;
					let x: i64 = self.stack.pop().unwrap() as i64;
					let result = x >= y;
					self.stack.push(result as u64);
				}
				I64Gt => {
					let y: i64 = self.stack.pop().unwrap() as i64;
					let x: i64 = self.stack.pop().unwrap() as i64;
					let result = x > y;
					self.stack.push(result as u64);
				}
				I64Le => {
					let y: i64 = self.stack.pop().unwrap() as i64;
					let x: i64 = self.stack.pop().unwrap() as i64;
					let result = x <= y;
					self.stack.push(result as u64);
				}
				I64Lt => {
					let y: i64 = self.stack.pop().unwrap() as i64;
					let x: i64 = self.stack.pop().unwrap() as i64;
					let result = x < y;
					self.stack.push(result as u64);
				}
				I64Ne => {
					let y: i64 = self.stack.pop().unwrap() as i64;
					let x: i64 = self.stack.pop().unwrap() as i64;
					let result = x != y;
					self.stack.push(result as u64);
				}
				U64Const(value) => {
					let result = *value;
					self.stack.push(result);
				}
				U64Load(offset) => {
					let i: usize = self.stack.pop().unwrap() as usize + offset;
					let result = u64::from_le_bytes(memory[i..i + 8].try_into().unwrap());
					self.stack.push(result);
				}
				U64Store(offset) => {
					let x: u64 = self.stack.pop().unwrap();
					let i: usize = self.stack.pop().unwrap() as usize + offset;
					memory[i..i + 8].copy_from_slice(&x.to_le_bytes());
				}
				U64And => {
					let y: u64 = self.stack.pop().unwrap();
					let x: u64 = self.stack.pop().unwrap();
					let result = x & y;
					self.stack.push(result);
				}
				U64Or => {
					let y: u64 = self.stack.pop().unwrap();
					let x: u64 = self.stack.pop().unwrap();
					let result = x | y;
					self.stack.push(result);
				}
				U64Xor => {
					let y: u64 = self.stack.pop().unwrap();
					let x: u64 = self.stack.pop().unwrap();
					let result = x ^ y;
					self.stack.push(result);
				}
				U64Add(location) => {
					let y: u64 = self.stack.pop().unwrap();
					let x: u64 = self.stack.pop().unwrap();
					let result = x.checked_add(y).unwrap();
					self.stack.push(result);
				}
				U64Div(location) => {
					let y: u64 = self.stack.pop().unwrap();
					let x: u64 = self.stack.pop().unwrap();
					let result = x.checked_div(y).unwrap();
					self.stack.push(result);
				}
				U64Mul(location) => {
					let y: u64 = self.stack.pop().unwrap();
					let x: u64 = self.stack.pop().unwrap();
					let result = x.checked_mul(y).unwrap();
					self.stack.push(result);
				}
				U64Rem(location) => {
					let y: u64 = self.stack.pop().unwrap();
					let x: u64 = self.stack.pop().unwrap();
					let result = x.checked_rem(y).unwrap();
					self.stack.push(result);
				}
				U64Rotl => {
					let y: u32 = self.stack.pop().unwrap() as u32;
					let x: u64 = self.stack.pop().unwrap();
					let result = x.rotate_left(y);
					self.stack.push(result);
				}
				U64Rotr => {
					let y: u32 = self.stack.pop().unwrap() as u32;
					let x: u64 = self.stack.pop().unwrap();
					let result = x.rotate_right(y);
					self.stack.push(result);
				}
				U64Shl(location) => {
					let y: u32 = self.stack.pop().unwrap() as u32;
					let x: u64 = self.stack.pop().unwrap();
					let result = x.checked_shl(y).unwrap();
					self.stack.push(result);
				}
				U64Shr(location) => {
					let y: u32 = self.stack.pop().unwrap() as u32;
					let x: u64 = self.stack.pop().unwrap();
					let result = x.checked_shr(y).unwrap();
					self.stack.push(result);
				}
				U64Sub(location) => {
					let y: u64 = self.stack.pop().unwrap();
					let x: u64 = self.stack.pop().unwrap();
					let result = x.checked_sub(y).unwrap();
					self.stack.push(result);
				}
				U64Eqz => {
					let x: u64 = self.stack.pop().unwrap();
					let result = x == 0;
					self.stack.push(result as u64);
				}
				U64Eq => {
					let y: u64 = self.stack.pop().unwrap();
					let x: u64 = self.stack.pop().unwrap();
					let result = x != y;
					self.stack.push(result as u64);
				}
				U64Ge => {
					let y: u64 = self.stack.pop().unwrap();
					let x: u64 = self.stack.pop().unwrap();
					let result = x >= y;
					self.stack.push(result as u64);
				}
				U64Gt => {
					let y: u64 = self.stack.pop().unwrap();
					let x: u64 = self.stack.pop().unwrap();
					let result = x > y;
					self.stack.push(result as u64);
				}
				U64Le => {
					let y: u64 = self.stack.pop().unwrap();
					let x: u64 = self.stack.pop().unwrap();
					let result = x <= y;
					self.stack.push(result as u64);
				}
				U64Lt => {
					let y: u64 = self.stack.pop().unwrap();
					let x: u64 = self.stack.pop().unwrap();
					let result = x < y;
					self.stack.push(result as u64);
				}
				U64Ne => {
					let y: u64 = self.stack.pop().unwrap();
					let x: u64 = self.stack.pop().unwrap();
					let result = x != y;
					self.stack.push(result as u64);
				}
			}
			ip += 1;
		}
	}
}
