#![allow(unused)]
use indexmap::IndexSet;

use crate::utils::stringify_lossy;

use super::mir::*;

impl<'a> Program<'a> {
	pub fn compile_to_wasm(&self) -> Vec<u8> {
		Compiler::new(self).compile()
	}
}

enum Opcode {
	Block = 0x02,
	Loop = 0x03,
	End = 0x0B,
	Br = 0x0C,
	Return = 0x0F,
	BrIf = 0x0D,
	Call = 0x10,
	Drop = 0x1A,
	LocalGet = 0x20,
	LocalSet = 0x21,
	I32Const = 0x41,
	I32Eq = 0x46,
}

struct Compiler<'a, 'b> {
	program: &'b Program<'a>,
	types: IndexSet<WasmFnType>,
	signatures: Vec<usize>,
}

const HOSTLANG_DEPS: [(&'static str, &'static str); 1] = [("console", "log")];

impl<'a, 'b> Compiler<'a, 'b> {
	fn new(program: &'b Program<'a>) -> Self {
		let x = 134;
		Self { program, types: IndexSet::new(), signatures: vec![] }
	}

	fn compile(mut self) -> Vec<u8> {
		let mut binary = b"\x00asm".to_vec();
		binary.extend(1u32.to_le_bytes());
		let mut code = vec![];
		for f in &self.program.functions {
			let (signature, body) = self.compile_fn(&f);
			self.signatures.push(signature);
			code.push(body);
		}
		println!("{}", stringify_lossy(&binary));
		self.pack_types(&mut binary);
		// TODO: pack imports
		println!("{}", stringify_lossy(&binary));
		self.pack_signatures(&mut binary);
		println!("{}", stringify_lossy(&binary));
		self.pack_code(&mut binary, code);
		binary
	}

	fn pack_types(&self, binary: &mut Vec<u8>) {
		const TYPE_SECTION_ID: u8 = 0x01;
		let mut section = vec![];
		section.pack_usize(self.types.len());
		for typ in &self.types {
			section.pack_usize(typ.params.len());
			for param in &typ.params {
				section.push(param.compile());
			}
			section.pack_usize(typ.results.len());
			for param in &typ.results {
				section.push(param.compile());
			}
		}
		binary.pack_section(TYPE_SECTION_ID, &section);
	}

	fn pack_code(&self, binary: &mut Vec<u8>, code: Vec<Vec<u8>>) {
		const CODE_SECTION_ID: u8 = 0x0A;
		let mut section = vec![];
		section.pack_usize(code.len());
		for body in &code {
			section.pack_usize(body.len());
			section.extend(body);
		}
		binary.pack_section(CODE_SECTION_ID, &section);
	}

	fn pack_signatures(&self, binary: &mut Vec<u8>) {
		const FUNCTION_SECTION_ID: u8 = 0x03;
		let mut section = vec![];
		section.pack_usize(self.signatures.len());
		for &f in &self.signatures {
			section.pack_usize(f);
		}
		binary.pack_section(FUNCTION_SECTION_ID, &section);
	}

	fn compile_fn(&mut self, f: &Function<'a, ()>) -> (usize, Vec<u8>) {
		let params = f.params.iter().map(|(_, typ)| typ.clone()).filter(|t| *t != Type::Unit).collect::<Vec<_>>();
		let results = [f.result.clone()].iter().map(|typ| typ.clone()).filter(|t| *t != Type::Unit).collect::<Vec<_>>();
		let type_id = self.types.insert_full(WasmFnType::new(params, results)).0;

		let mut body = vec![];
		body.pack_usize(f.locals.len());
		for local in &f.locals {
			body.pack_u32(1);
			body.push(local.1.compile());
		}
		for stmt in &f.body {
			body.extend(self.compile_stmt(stmt));
		}
		body.push(Opcode::End as u8);
		(type_id, body)
	}

	fn compile_instr(&mut self, instr: &Instr) -> Vec<u8> {
		use InstrKind::*;
		match &instr.kind {
			Cntrl(instr) => self.compile_stmt(instr),
			Bool(instr) => self.compile_bool(instr),
			I64(instr) => self.compile_i64(instr),
			U64(instr) => self.compile_u64(instr),
			F64(instr) => self.compile_f64(instr),
			Str(instr) => self.compile_str(instr),
			Vec(instr, _) => self.compile_vec(instr),
		}
	}

	fn compile_stmt(&mut self, stmt: &InstrCntrl) -> Vec<u8> {
		let mut code = vec![];
		use InstrKindCntrl::*;
		match &*stmt.kind {
			Call(idx, vec) => {
				for argument in vec {
					code.extend(self.compile_instr(argument));
				}
				code.push(Opcode::Call as u8);
				code.pack_usize(*idx + HOSTLANG_DEPS.len());
			}
			DefI64(idx, instr) | SetI64(idx, instr) => {
				code.extend(self.compile_i64(instr));
				code.push(Opcode::LocalSet as u8);
				code.pack_usize(*idx);
			}
			DefU64(idx, instr) | SetU64(idx, instr) => {
				code.extend(self.compile_u64(instr));
				code.push(Opcode::LocalSet as u8);
				code.pack_usize(*idx);
			}
			DefF64(idx, instr) | SetF64(idx, instr) => {
				code.extend(self.compile_f64(instr));
				code.push(Opcode::LocalSet as u8);
				code.pack_usize(*idx);
			}
			DefStr(idx, instr) | SetStr(idx, instr) => {
				code.extend(self.compile_str(instr));
				code.push(Opcode::LocalSet as u8);
				code.pack_usize(*idx);
			}
			DefBool(idx, instr) | SetBool(idx, instr) => {
				code.extend(self.compile_bool(instr));
				code.push(Opcode::LocalSet as u8);
				code.pack_usize(*idx);
			}
			DefVec(idx, instr, _) => {
				code.extend(self.compile_vec(instr));
				code.push(Opcode::LocalSet as u8);
				code.pack_usize(*idx);
			}
			SetVec(_) => todo!(), // TODO: why no instruction in that tuple?
			PrintlnStr(instr) | PrintStr(instr) => {
				code.extend(self.compile_str(instr));
				code.push(Opcode::Call as u8);
				code.pack_usize(0);
			}
			While(condition, body) => {
				code.push(Opcode::Block as u8);
				code.push(Opcode::Loop as u8);
				code.extend(self.compile_bool(condition));
				code.push(Opcode::I32Const as u8);
				code.pack_u32(0);
				code.push(Opcode::I32Eq as u8);
				code.push(Opcode::BrIf as u8);
				code.pack_usize(1);
				for instr in body {
					code.extend(self.compile_stmt(instr));
				}
				code.push(Opcode::Br as u8);
				code.pack_usize(0);
				code.push(Opcode::End as u8);
				code.push(Opcode::End as u8);
			}
			Block(body) => {
				for instr in body {
					code.extend(self.compile_stmt(instr));
				}
			}
			Return(value) => {
				code.extend(self.compile_instr(value));
				code.push(Opcode::Return as u8);
			}
			Drop(instr) => {
				code.extend(self.compile_instr(instr));
				code.push(Opcode::Drop as u8);
			}
			Push(_, instr) => todo!(),
		}
		code
	}

	fn compile_i64(&mut self, instr: &InstrI64) -> Vec<u8> {
		let mut code = vec![];
		use InstrKindI64::*;
		match &*instr.kind {
			Add(instr_i64, instr_i65) => todo!(),
			Sub(instr_i64, instr_i65) => todo!(),
			Mult(instr_i64, instr_i65) => todo!(),
			Div(instr_i64, instr_i65) => todo!(),
			Rem(instr_i64, instr_i65) => todo!(),
			And(instr_i64, instr_i65) => todo!(),
			Xor(instr_i64, instr_i65) => todo!(),
			Or(instr_i64, instr_i65) => todo!(),
			Return(instr) => todo!(),
			Variable(_) => todo!(),
			Call(_) => todo!(),
			Value(_) => todo!(),
		}
		code
	}

	fn compile_u64(&mut self, instr: &InstrU64) -> Vec<u8> {
		let mut code = vec![];
		use InstrKindU64::*;
		match &*instr.kind {
			Add(instr_u64, instr_u65) => todo!(),
			Sub(instr_u64, instr_u65) => todo!(),
			Mult(instr_u64, instr_u65) => todo!(),
			Div(instr_u64, instr_u65) => todo!(),
			Rem(instr_u64, instr_u65) => todo!(),
			And(instr_u64, instr_u65) => todo!(),
			Xor(instr_u64, instr_u65) => todo!(),
			Or(instr_u64, instr_u65) => todo!(),
			Return(instr) => todo!(),
			Variable(_) => todo!(),
			Call(_) => todo!(),
			Value(_) => todo!(),
		}
		code
	}

	fn compile_f64(&mut self, instr: &InstrF64) -> Vec<u8> {
		let mut code = vec![];
		use InstrKindF64::*;
		match &*instr.kind {
			Add(instr_f64, instr_f65) => todo!(),
			Sub(instr_f64, instr_f65) => todo!(),
			Mult(instr_f64, instr_f65) => todo!(),
			Div(instr_f64, instr_f65) => todo!(),
			Return(instr) => todo!(),
			Variable(_) => todo!(),
			Call(_) => todo!(),
			Value(_) => todo!(),
		}
		code
	}

	fn compile_str(&mut self, instr: &InstrStr) -> Vec<u8> {
		let mut code = vec![];
		use InstrKindStr::*;
		match &*instr.kind {
			Add(instr_str, instr_str1) => todo!(),
			Return(instr) => todo!(),
			Variable(_) => todo!(),
			Value(_) => todo!(),
			Call(_) => todo!(),
			CastI64(instr_i64) => todo!(),
			CastF64(instr_f64) => todo!(),
			CastVec(instr_vec, _) => todo!(),
		}
		code
	}

	fn compile_bool(&mut self, instr: &InstrBool) -> Vec<u8> {
		let mut code = vec![];
		use InstrKindBool::*;
		match &*instr.kind {
			LessI64(instr_i64, instr_i65) => todo!(),
			Not(instr_bool) => todo!(),
			And(instr_bool, instr_str) => todo!(),
			Return(instr) => todo!(),
			Variable(_) => todo!(),
			Value(_) => todo!(),
			Call(_) => todo!(),
		}
		code
	}

	fn compile_vec(&mut self, instr: &InstrVec) -> Vec<u8> {
		let mut code = vec![];
		use InstrKindVec::*;
		match &*instr.kind {
			Return(instr) => todo!(),
			Variable(_) => todo!(),
			Value(_) => todo!(),
			Call(_) => todo!(),
			Empty => todo!(),
		}
		code
	}
}

struct FunctionCompiler<'a, 'b, 'c> {
	compiler: &'b Compiler<'a, 'c>,
	f: &'b Function<'a, ()>,
}

impl<'a, 'b, 'c> FunctionCompiler<'a, 'b, 'c> {
	fn new(compiler: &'b Compiler<'a, 'c>, function: &'b Function<'a, ()>) -> Self {
		Self { compiler, f: function }
	}

	fn compile(self) {}
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct WasmFnType {
	params: Vec<Type>,
	results: Vec<Type>,
}

impl WasmFnType {
	fn new(params: Vec<Type>, results: Vec<Type>) -> Self {
		Self { params, results }
	}
}

impl Type {
	fn compile(&self) -> u8 {
		match self {
			Type::Unit => unreachable!(),
			Type::Bool => 0x7f,
			Type::I64 => 0x7e,
			Type::U64 => 0x7e,
			Type::F64 => 0x7c,
			Type::Str => 0x7e,
			Type::Vec(_) => 0x7e,
		}
	}
}

trait Leb128 {
	fn pack_u32(&mut self, value: u32);
	fn pack_usize(&mut self, value: usize);
	fn pack_section(&mut self, typ: u8, section: &[u8]);
	fn push_op(&mut self, opcode: Opcode);
}

impl Leb128 for Vec<u8> {
	fn pack_u32(&mut self, mut value: u32) {
		loop {
			self.push(0x7f & value as u8 + (value > 0x7f) as u8);
			value >>= 7;
			if value == 0 {
				break;
			}
		}
	}

	fn pack_usize(&mut self, mut value: usize) {
		loop {
			self.push(0x7f & value as u8 + (value > 0x7f) as u8);
			value >>= 7;
			if value == 0 {
				break;
			}
		}
	}

	fn pack_section(&mut self, typ: u8, section: &[u8]) {
		self.push(typ);
		self.pack_u32(section.len() as u32);
		self.extend(section);
	}

	fn push_op(&mut self, opcode: Opcode) {
		self.push(opcode as u8);
	}
}
