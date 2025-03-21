#![allow(unused)]

use std::{
	collections::{HashMap, VecDeque},
	num::{NonZeroU16, NonZeroUsize},
	rc::Rc,
};
use indexmap::{IndexMap, IndexSet};

use super::hir::*;

#[derive(Debug, Clone, PartialEq)]
pub struct Bytecode {
	pub types: IndexSet<FuncType>,
	pub imports: IndexSet<Import>,
	pub sources: HashMap<Str, Str>,
	pub globals: IndexMap<Str, GlobalVariable>,
	pub functions: IndexMap<Str, Func>,
	pub data: Vec<u8>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Import {
	pub signature: FuncType,
	pub namespace: Str,
	pub func_name: Str,
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Func {
	pub signature: FuncType,
	pub locals: Vec<ValueType>,
	pub code: Vec<Op>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub struct FuncType {
	pub parameters: Vec<ValueType>,
	pub results: Vec<ValueType>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GlobalVariable {
	pub typ: ValueType,
	pub mutable: bool,
	pub value: u64,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum BlockType {
	ValueType(ValueType),
	TypeIndex(usize),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
pub enum ValueType {
	I32,
	I64,
	F32,
	F64,
	// TODO: FuncRef,
	ExternRef,
}

type Str = Rc<str>;

impl Program {
	pub fn to_lir(&self) -> Bytecode {
		ProgramCompiler::new(self).generate_lir()
	}
}

struct ProgramCompiler<'a> {
	program: &'a Program,
	lir: Bytecode,
	queue: VecDeque<Str>,
	globals: Variables,
}

#[derive(Default)]
struct Variables {
	valtypes: Vec<ValueType>,
	locations: HashMap<Str, Vec<usize>>,
	shadowed: Vec<(Str, Vec<usize>)>,
	vacant_i32: Vec<usize>,
	vacant_f32: Vec<usize>,
	vacant_i64: Vec<usize>,
	vacant_f64: Vec<usize>,
}

impl<'a> ProgramCompiler<'a> {
	fn new(program: &'a Program) -> Self {
		assert!(!program.is_poisoned());
		let main = program.main.to_string() + ".main";
		let queue = VecDeque::from([main.into()]);
		let lir = Bytecode {
			types: IndexSet::new(),
			imports: IndexSet::new(),
			sources: program.sources.clone(),
			globals: IndexMap::new(),
			functions: IndexMap::new(),
			data: Vec::new(),
		};
		// program.functions.iter().for_each(|(f, _)| eprintln!("{}", f));
		ProgramCompiler { program, lir, queue, globals: Variables::default() }
	}

	fn generate_lir(mut self) -> Bytecode {
		while let Some(name) = self.queue.pop_front() {
			eprintln!("> Compiling {}", name);
			self.process_function(name);
		}
		self.lir
	}

	fn process_function(&mut self, fname: Str) {
		let function = &self.program.functions[&fname];
		let bytecode = FunctionCompiler::new(self, function).compile();
		self.lir.functions.insert(fname, bytecode);
	}

	fn queue_function(&mut self, function: &str) -> usize {
		if let Some(idx) = self.lir.functions.get_index_of(function) {
			return idx;
		}
		match function {
			_ => {
				let function = Str::from(function);
				self.queue.push_back(function.clone());
				let (idx, _) = self.lir.functions.insert_full(function, Func::default());
				idx
			}
		}
	}

	fn queue_global(&mut self, global: &str) -> Vec<usize> {
		if let Some(layout) = self.globals.find(global) {
			return layout;
		}
		let layout = self.process_type(&self.program.globals[global].0);
		self.globals.insert(global.into(), layout)
	}

	fn process_type(&mut self, typ: &Type) -> Vec<ValueType> {
		match typ {
			Type::Tuple(items) => {
				let mut flattened = vec![];
				for item in items {
					flattened.extend(self.process_type(&item));
				}
				flattened
			}
			Type::Array(_) => vec![ValueType::I32, ValueType::I32, ValueType::I32],
			Type::Name(typename) => match &typename[..] {
				"str" => vec![ValueType::I32, ValueType::I32],
				"i32" | "u32" | "i16" | "u16" | "i8" | "u8" => vec![ValueType::I32],
				"i64" | "u64" => vec![ValueType::I64],
				"f32" => vec![ValueType::F32],
				"f64" => vec![ValueType::F64],
				_ => todo!("Type {}", typename),
			},
			Type::Void => vec![],
		}
	}
}

impl Variables {
	fn insert(&mut self, name: Str, layout: Vec<ValueType>) -> Vec<usize> {
		use ValueType::*;
		let old_layout = self.locations.remove(&name).unwrap_or(Vec::new());
		self.shadowed.push((name.clone(), old_layout));
		let mut new = |valtype| {
			let position = self.valtypes.len();
			self.valtypes.push(valtype);
			position
		};
		let mut slots = Vec::with_capacity(layout.len());
		for typ in layout {
			slots.push(match &typ {
				I32 => self.vacant_i32.pop().unwrap_or_else(|| new(typ)),
				I64 => self.vacant_i64.pop().unwrap_or_else(|| new(typ)),
				F32 => self.vacant_f32.pop().unwrap_or_else(|| new(typ)),
				F64 => self.vacant_f64.pop().unwrap_or_else(|| new(typ)),
				ExternRef => todo!(),
			});
		}
		self.locations.insert(name.clone(), slots.clone());
		slots
	}

	fn find<'a>(&'a self, name: &str) -> Option<Vec<usize>> {
		self.locations.get(name).cloned()
	}
}

struct FunctionCompiler<'c, 'p> {
	function: &'p Function,
	program: &'p Program,
	ctx: &'c mut ProgramCompiler<'p>,
	locals: Vec<ValueType>,
	named_locals: Vec<(Str, usize)>,
	results: Vec<ValueType>,
}

impl<'a, 'p> FunctionCompiler<'a, 'p> {
	fn new(ctx: &'a mut ProgramCompiler<'p>, function: &'p Function) -> Self {
		let results = ctx.process_type(&function.result);
		Self { program: ctx.program, ctx, locals: Vec::new(), named_locals: Vec::new(), results, function }
	}

	fn compile(mut self) -> Func {
		let mut parameters = vec![];
		for (name, typ) in &self.function.parameters {
			let typ = self.ctx.process_type(typ);
			parameters.extend(&typ);
			self.define_local(name.clone(), typ);
		}
		let mut code = vec![];
		self.compile_instr(&self.function.body.value, &mut code);
		let locals = self.locals;
		let signature = FuncType { parameters, results: self.results };
		Func { signature, locals, code }
	}

	fn compile_instr(&mut self, instr: &Instr, code: &mut Vec<Op>) {
		use InstrKind::*;
		match &instr.kind {
			String(string) => {
				let string = string.as_bytes();
				let ptr = self.ctx.lir.data.len() as u32;
				let length = string.len() as u32;
				self.ctx.lir.data.extend(string);
				let clone = self.ctx.queue_function("std.clone_str");
				code.extend([Op::U32Const(ptr), Op::U32Const(length), Op::CallFunc(clone)]);
			}
			Integer(source) => code.push(match self.function.body.types.realize(instr.typ) {
				Type::Name(name) => match &name[..] {
					"u8" | "u16" | "u32" => Op::U32Const(source.parse().unwrap()),
					"i8" | "i16" | "i32" => Op::I32Const(source.parse().unwrap()),
					"u64" => Op::U64Const(source.parse().unwrap()),
					"i64" => Op::I64Const(source.parse().unwrap()),
					_ => unreachable!(),
				},
				_ => unreachable!(),
			}),
			GetLocal(_) => todo!(),
			Tuple(fields) => fields.iter().for_each(|x| self.compile_instr(x, code)),
			Assignment(name, instr) => {
				self.compile_instr(instr, code);
				if let Some((pos, len)) = self.find_local(name) {
					(pos..(pos + len)).rev().for_each(|x| code.push(Op::LocalSet(x)));
				} else {
					let layout = self.ctx.globals.find(name).unwrap();
					layout.iter().rev().for_each(|&x| code.push(Op::GlobalSet(x)));
				};
			}
			Identifier(name) => {
				if let Some((pos, size)) = self.find_local(name) {
					(pos..self.locals.len()).for_each(|x| code.push(Op::LocalGet(x)));
				} else {
					let layout = self.ctx.queue_global(name);
					layout.iter().for_each(|&x| code.push(Op::GlobalGet(x)));
				}
			}
			Block(block) => {
				for (target, instr) in &block.stmts {
					self.compile_instr(instr, code);
					let typ = self.function.body.types.realize(instr.typ);
					if let Some((name, mutable)) = target {
						let position = self.locals.len();
						self.locals.extend(self.ctx.process_type(&typ));
						self.named_locals.push((name.clone(), position));
						for target in (position..self.locals.len()).rev() {
							code.push(Op::LocalSet(target));
						}
					} else {
						self.compile_drop(&typ, code);
					}
				}
				self.compile_instr(&block.result, code);
			}
			While(instr, instr1) => todo!(),
			If(instr, instr1, instr2) => todo!(),
			MethodCall(receiver, method, args) => {
				let src = instr.location as usize;
				let typ = self.function.body.types.realize(instr.typ);
				self.compile_instr(&receiver, code);
				args.iter().for_each(|x| self.compile_instr(x, code));
				match &typ {
					Type::Tuple(_) => unreachable!(),
					Type::Array(_) => unreachable!(),
					Type::Name(receiver) => match (&receiver[..], &method[..]) {
						("i32", "add") => code.push(Op::I32Add(src)),
						("i32", "sub") => code.push(Op::I32Sub(src)),
						("i32", "mul") => code.push(Op::I32Mul(src)),
						("i32", "div") => code.push(Op::I32Div(src)),
						("i32", "rem") => code.push(Op::I32Rem(src)),
						("i32", "shl") => code.push(Op::I32Shl(src)),
						("i32", "shr") => code.push(Op::I32Shr(src)),
						("u32", "add") => code.push(Op::U32Add(src)),
						("u32", "sub") => code.push(Op::U32Sub(src)),
						("u32", "mul") => code.push(Op::U32Mul(src)),
						("u32", "div") => code.push(Op::U32Div(src)),
						("u32", "rem") => code.push(Op::U32Rem(src)),
						("u32", "shl") => code.push(Op::U32Shl(src)),
						("u32", "shr") => code.push(Op::U32Shr(src)),
						x => todo!("{:?}", x),
					},
					Type::Void => unreachable!(),
				}
			}
			FnCall(name, args) => match &name[..] {
				"memory_copy" => {
					args.iter().for_each(|arg| self.compile_instr(arg, code));
					code.push(Op::MemoryCopy);
				}
				"println" => {
					let mut string_in_stack = false;
					for instr in args {
						let typ = self.function.body.types.realize(instr.typ);
						self.compile_instr(instr, code);
						self.compile_stringify(&typ, code);
						if string_in_stack {
							code.push(Op::CallFunc(self.ctx.queue_function("std.str_add")));
						} else {
							string_in_stack = true;
						}
					}
					if !string_in_stack {
						self.compile_string("", code);
					}
					code.push(Op::CallFunc(self.ctx.queue_function("std.println")));
				}
				_ => {
					args.iter().for_each(|arg| self.compile_instr(arg, code));
					code.push(Op::CallFunc(self.ctx.queue_function(name)))
				}
			},
			Return(value) => {
				self.compile_instr(value, code);
				code.push(Op::Return);
			}
			Unreachable => code.push(Op::Unreachable),
			NoOp => {}
		}
	}

	fn compile_stringify(&mut self, typ: &Type, code: &mut Vec<Op>) {
		match typ {
			Type::Tuple(_) => unreachable!(),
			Type::Array(_) => unreachable!(),
			Type::Name(typ) => match &typ[..] {
				"str" => {}
				_ => todo!("str({})", typ),
			},
			Type::Void => {}
		}
	}

	fn compile_drop(&mut self, typ: &Type, code: &mut Vec<Op>) {
		match typ {
			Type::Tuple(fields) => fields.iter().for_each(|typ| self.compile_drop(typ, code)),
			Type::Array(_) => todo!(),
			Type::Name(typ) => match &typ[..] {
				"i8" | "u8" | "i16" | "u16" | "i32" | "u32" | "f32" | "i64" | "u64" | "f64" => code.push(Op::Drop),
				"str" => code.push(Op::CallFunc(self.ctx.queue_function("std.free"))),
				_ => todo!(),
			},
			Type::Void => {}
		}
	}

	fn compile_string(&mut self, string: &str, code: &mut Vec<Op>) {
		let string = string.as_bytes();
		let ptr = self.ctx.lir.data.len() as u32;
		let length = string.len() as u32;
		self.ctx.lir.data.extend(string);
		let clone = self.ctx.queue_function("std.clone_str");
		code.extend([Op::U32Const(ptr), Op::U32Const(length), Op::CallFunc(clone)]);
	}

	fn find_local(&self, name: &str) -> Option<(usize, usize)> {
		let mut last_pos = self.locals.len();
		for (local, position) in self.named_locals.iter().rev() {
			if name == &local[..] {
				return Some((*position, last_pos - position));
			} else {
				last_pos = *position;
			}
		}
		None
	}

	fn find_global(&self, name: &str) -> Option<(usize, usize)> {
		let mut last_pos = self.locals.len();
		for (local, position) in self.named_locals.iter().rev() {
			if name == &local[..] {
				return Some((*position, last_pos - position));
			} else {
				last_pos = *position;
			}
		}
		None
	}

	fn define_local(&mut self, name: Str, typ: Vec<ValueType>) {
		let position = self.locals.len();
		self.locals.extend(typ);
		self.named_locals.push((name, position));
	}
}

#[derive(Debug, Clone, PartialEq)]
pub enum Op {
	Unreachable,
	Nop,
	Block(BlockType, usize),
	Loop(BlockType, usize),
	IfThen(BlockType, usize),
	Else(usize),
	End,
	Br(usize),
	BrIf(usize),
	JumpInto(Vec<usize>),
	Return,
	CallFunc(usize),
	CallImport(usize),
	CallIndirect(usize),
	Drop,
	Select(ValueType),
	GlobalGet(usize),
	GlobalSet(usize),
	LocalGet(usize),
	LocalSet(usize),
	LocalTee(usize),
	MemorySize,
	MemoryGrow,
	MemoryCopy,
	F32Const(f32),
	F32Load(usize),
	F32Store(usize),
	F32Abs,
	F32Ceil,
	F32Floor,
	F32Nearest,
	F32Neg,
	F32Sqrt,
	F32Trunc,
	F32Copysign,
	F32Add,
	F32Div,
	F32Max,
	F32Min,
	F32Mul,
	F32Sub,
	F32Eq,
	F32Ge,
	F32Gt,
	F32Le,
	F32Lt,
	F32Ne,
	I32Const(i32),
	I32Load(usize),
	I32Store(usize),
	I32And,
	I32Or,
	I32Xor,
	I32Add(usize),
	I32Div(usize),
	I32Mul(usize),
	I32Rem(usize),
	I32Rotl,
	I32Rotr,
	I32Shl(usize),
	I32Shr(usize),
	I32Sub(usize),
	I32Eqz,
	I32Eq,
	I32Ge,
	I32Gt,
	I32Le,
	I32Lt,
	I32Ne,
	U32Const(u32),
	U32Load(usize),
	U32Store(usize),
	U32And,
	U32Or,
	U32Xor,
	U32Add(usize),
	U32Div(usize),
	U32Mul(usize),
	U32Rem(usize),
	U32Rotl,
	U32Rotr,
	U32Shl(usize),
	U32Shr(usize),
	U32Sub(usize),
	U32Eqz,
	U32Eq,
	U32Ge,
	U32Gt,
	U32Le,
	U32Lt,
	U32Ne,
	F64Const(f64),
	F64Load(usize),
	F64Store(usize),
	F64Abs,
	F64Ceil,
	F64Floor,
	F64Nearest,
	F64Neg,
	F64Sqrt,
	F64Trunc,
	F64Copysign,
	F64Add,
	F64Div,
	F64Max,
	F64Min,
	F64Mul,
	F64Sub,
	F64Eq,
	F64Ge,
	F64Gt,
	F64Le,
	F64Lt,
	F64Ne,
	I64Const(i64),
	I64Load(usize),
	I64Store(usize),
	I64And,
	I64Or,
	I64Xor,
	I64Add(usize),
	I64Div(usize),
	I64Mul(usize),
	I64Rem(usize),
	I64Rotl,
	I64Rotr,
	I64Shl(usize),
	I64Shr(usize),
	I64Sub(usize),
	I64Eqz,
	I64Eq,
	I64Ge,
	I64Gt,
	I64Le,
	I64Lt,
	I64Ne,
	U64Const(u64),
	U64Load(usize),
	U64Store(usize),
	U64And,
	U64Or,
	U64Xor,
	U64Add(usize),
	U64Div(usize),
	U64Mul(usize),
	U64Rem(usize),
	U64Rotl,
	U64Rotr,
	U64Shl(usize),
	U64Shr(usize),
	U64Sub(usize),
	U64Eqz,
	U64Eq,
	U64Ge,
	U64Gt,
	U64Le,
	U64Lt,
	U64Ne,
}
