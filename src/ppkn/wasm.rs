#![allow(dead_code)]
use std::{borrow::Borrow, rc::Rc};

use indexmap::{IndexMap, IndexSet};
use ordered_float::NotNan;
use crate::utils::Leb128;

#[derive(Debug, Clone, PartialEq, Eq)]
struct WebAssemblyModule {
	funcs: IndexMap<Rc<str>, Func>,
	mems: Vec<MemoryLimit>,
	globals: Vec<GlobalVariable>,
	datas: Vec<Data>,
	start: Option<u32>,
	imports: IndexSet<Import>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Func {
	signature: FuncType,
	locals: Vec<ValueType>,
	body: Vec<Instr>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct MemoryLimit {
	min: u32,
	max: u32,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct GlobalVariable {
	typ: ValueType,
	mutable: bool,
	value: u64,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Data {
	bytes: Vec<u8>,
	offset: u32,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Import {
	signature: FuncType,
	module: Rc<str>,
	name: Rc<str>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct FuncType {
	parameters: Vec<ValueType>,
	result: Vec<ValueType>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum ValueType {
	I32,
	I64,
	F32,
	F64,
	FuncRef,
	ExternRef,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Instr {
	Block(Vec<ValueType>, Vec<Instr>),
	Loop(Vec<ValueType>, Vec<Instr>),
	If(Vec<ValueType>, Vec<Instr>, Vec<Instr>),
	CallImportedFn(u32),
	CallFunction(u32),
	Unreachable, Nop, Br { label: u32 }, BrIf { label: u32 }, BrTable { labels: Vec<u32> }, Return,
	CallIndirect { table: u32, func_type: u32 }, Drop, LocalGet, LocalSet, LocalTee, GlobalGet,
	GlobalSet, TableGet, TableSet, I32Load { offset: u32, align: u32 },
	I64Load { offset: u32, align: u32 }, F32Load { offset: u32, align: u32 },
	F64Load { offset: u32, align: u32 }, I32Load8S { offset: u32, align: u32 },
	I32Load8U { offset: u32, align: u32 }, I32Load16S { offset: u32, align: u32 },
	I32Load16U { offset: u32, align: u32 }, I64Load8S { offset: u32, align: u32 },
	I64Load8U { offset: u32, align: u32 }, I64Load16S { offset: u32, align: u32 },
	I64Load16U { offset: u32, align: u32 }, I64Load32S { offset: u32, align: u32 },
	I64Load32U { offset: u32, align: u32 }, I32Store { offset: u32, align: u32 },
	I64Store { offset: u32, align: u32 }, F32Store { offset: u32, align: u32 },
	F64Store { offset: u32, align: u32 }, I32Store8 { offset: u32, align: u32 },
	I32Store16 { offset: u32, align: u32 }, I64Store8 { offset: u32, align: u32 },
	I64Store16 { offset: u32, align: u32 }, I64Store32 { offset: u32, align: u32 }, MemorySize,
	MemoryGrow, I32Const(i32), I64Const(i64), F32Const(NotNan<f32>), F64Const(NotNan<f64>), I32Eqz, I32Eq, I32Ne,
	I32LtS, I32LtU, I32GtS, I32GtU, I32LeU, I32LeS, I32GeS, I32GeU, I64Eqz, I64Eq, I64Ne, I64LtS,
	I64LtU, I64GtS, I64GtU, I64LeU, I64LeS, I64GeS, I64GeU, F32Eq, F32Ne, F32Lt, F32Gt, F32Le, F32Ge,
	F64Eq, F64Ne, F64Lt, F64Gt, F64Le, F64Ge, I32Clz, I32Ctz, I32Popcnt, I32Add, I32Sub, I32Mul,
	I32DivS, I32DivU, I32RemS, I32RemU, I32And, I32Or, I32Xor, I32Shl, I32ShrS, I32ShrU, I32Rotl,
	I32Rotr, I64Clz, I64Ctz, I64Popcnt, I64Add, I64Sub, I64Mul, I64DivS, I64DivU, I64RemS, I64RemU,
	I64And, I64Or, I64Xor, I64Shl, I64ShrS, I64ShrU, I64Rotl, I64Rotr, F32Abs, F32Neg, F32Ceil,
	F32Floor, F32Trunc, F32Nearest, F32Sqrt, F32Add, F32Sub, F32Mul, F32Div, F32Min, F32Max,
	F32Copysign, F64Abs, F64Neg, F64Ceil, F64Floor, F64Trunc, F64Nearest, F64Sqrt, F64Add, F64Sub,
	F64Mul, F64Div, F64Min, F64Max, F64Copysign, I32WrapI64, I32TruncF32S, I32TruncF32U, I32TruncF64S,
	I32TruncF64U, I64ExtendI32S, I64ExtendI32U, I64TruncF32S, I64TruncF32U, I64TruncF64S, I64TruncF64U,
	F32ConvertI32S, F32ConvertI32U, F32ConvertI64S, F32ConvertI64U, F32DemoteF64, F64ConvertI32S,
	F64ConvertI32U, F64ConvertI64S, F64ConvertI64U, F64PromoteF32, I32ReinterpretF32,
	I64ReinterpretF64, F32ReinterpretI32, F64ReinterpretI64, I32Extend8S, I32Extend16S, I64Extend8S,
	I64Extend16S, I64Extend32S,
}

impl WebAssemblyModule {
	fn to_wasm(self) -> Vec<u8> {
		// TODO: self -> &self
		let mut wasm = b"\x00asm\x01\x00\x00\x00".to_vec();
		let mut types = IndexSet::new();
		let mut funcs = Vec::new();
		let mut ctx = CompilationContext { types: &mut types, funcs: &mut funcs, imports: self.imports.len() };
		let imports: Vec<_> = self.imports.into_iter().map(|import| import.to_wasm(&mut ctx)).collect();
		let functions: Vec<_> = self.funcs.into_iter().map(|(_, func)| func.to_wasm(&mut ctx)).collect();
		let types: Vec<_> = types.into_iter().map(|t| t.to_wasm()).collect();

		// TODO: guard each section behind check if it is needed
		let mut type_section = Vec::new();
		type_section.pack(types);
		wasm.push(0x01);
		wasm.pack(type_section.len());
		wasm.extend(type_section);

		let mut import_section = Vec::new();
		import_section.pack(imports);
		wasm.push(0x02);
		wasm.pack(import_section.len());
		wasm.extend(import_section);

		let mut function_section = Vec::with_capacity(funcs.len());
		function_section.pack(funcs.len());
		for func_signature in funcs {
			function_section.pack(func_signature);
		}
		wasm.push(0x03);
		wasm.pack(function_section.len());
		wasm.extend(function_section);

		// let table_section_id = 4;

		let mut memory_section = Vec::new();
		memory_section.pack(self.mems.iter().map(|m| m.to_wasm()).collect::<Vec<_>>());
		wasm.push(0x05);
		wasm.pack(memory_section.len());
		wasm.extend(memory_section);

		let mut global_section = Vec::new();
		global_section.pack(self.globals.iter().map(|x| x.to_wasm()).collect::<Vec<_>>());
		wasm.push(0x06);
		wasm.pack(global_section.len());
		wasm.extend(global_section);

		// let export_section_id = 7;

		if let Some(start) = self.start {
			let mut start_section = Vec::new();
			start_section.pack(start);
			wasm.push(0x08);
			wasm.pack(start_section.len());
			wasm.extend(start_section);
		}

		// let element_section_id = 9;

		let code_section_id = 10;
		let mut code_section = Vec::new();
		code_section.pack(functions);
		wasm.push(code_section_id);
		wasm.pack(code_section.len());
		wasm.extend(code_section);

		let data_section_id = 11;
		let mut data_section = Vec::new();
		data_section.pack(self.datas.iter().map(|x| x.to_wasm()).collect::<Vec<_>>());
		wasm.push(data_section_id);
		wasm.pack(data_section.len());
		wasm.extend(data_section);

		wasm
	}
}

struct CompilationContext<'t> {
	types: &'t mut IndexSet<FuncType>,
	funcs: &'t mut Vec<u32>,
	imports: usize,
}

impl Import {
	fn to_wasm(self, ctx: &mut CompilationContext) -> Vec<u8> {
		let (type_idx, _) = ctx.types.insert_full(self.signature);
		let mut wasm = Vec::with_capacity(4 + self.module.len() + self.name.len());
		wasm.pack(self.module);
		wasm.pack(self.name);
		wasm.push(0x00);
		wasm.pack(type_idx);
		wasm
	}
}

impl Func {
	fn to_wasm(&self, ctx: &mut CompilationContext) -> Vec<u8> {
		let mut locals: Vec<(ValueType, u32)> = vec![];
		for local in &self.locals {
			if locals.last().is_some_and(|(t, _)| local == t) {
				locals.last_mut().unwrap().1 += 1;
			} else {
				locals.push((local.clone(), 1));
			}
		}

		let mut code: Vec<u8> = vec![];
		code.pack(locals.len());
		for (typ, count) in locals {
			code.pack(count);
			code.push(typ.to_wasm());
		}

		self.body.iter().for_each(|instr| instr.to_wasm(ctx, &mut code));
		code.push(0x0B);

		let mut wasm = vec![];
		wasm.pack(code.len());
		wasm.extend(code);
		wasm
	}
}

impl FuncType {
	fn to_wasm(self) -> Vec<u8> {
		let mut wasm = Vec::with_capacity(3 + self.parameters.len() + self.result.len());
		wasm.push(0x60);
		wasm.pack(self.parameters.len());
		wasm.extend(self.parameters.iter().map(|typ| typ.to_wasm()));
		wasm.pack(self.result.len());
		wasm.extend(self.result.iter().map(|typ| typ.to_wasm()));
		wasm
	}
}

impl ValueType {
	fn to_wasm(&self) -> u8 {
		match self {
			ValueType::I32 => 0x7F,
			ValueType::I64 => 0x7E,
			ValueType::F32 => 0x7D,
			ValueType::F64 => 0x7C,
			ValueType::FuncRef => 0x70,
			ValueType::ExternRef => 0x6F,
		}
	}
}

impl MemoryLimit {
	fn to_wasm(&self) -> Vec<u8> {
		let mut wasm = vec![(self.max != u32::MAX) as u8];
		wasm.pack(self.min);
		if self.max != u32::MAX {
			wasm.pack(self.max);
		}
		wasm
	}
}

impl GlobalVariable {
	fn to_wasm(&self) -> Vec<u8> {
		let mut wasm = vec![self.typ.to_wasm()];
		wasm.push(self.mutable as u8);
		match self.typ {
			ValueType::I32 => {
				wasm.push(0x41);
				wasm.pack(self.value as i32);
			}
			ValueType::I64 => {
				wasm.push(0x42);
				wasm.pack(self.value as i64);
			}
			ValueType::F32 => {
				wasm.push(0x43);
				wasm.extend((self.value as u32).to_ne_bytes());
			}
			ValueType::F64 => {
				wasm.push(0x44);
				wasm.extend(self.value.to_ne_bytes());
			}
			ValueType::FuncRef | ValueType::ExternRef => {
				assert!(self.value == 0);
				wasm.push(0xD0);
			}
		}
		wasm.push(0x0B);
		wasm
	}
}

impl Data {
	fn to_wasm(&self) -> Vec<u8> {
		let mut wasm;
		if self.offset != 0 {
			wasm = vec![0x00, 0x41];
			wasm.pack(self.offset);
			wasm.push(0x0B);
		} else {
			wasm = vec![0x01];
		}
		wasm.pack(&*self.bytes);
		wasm
	}
}

trait ToWat<S: Borrow<str>> {
	fn to_wat(&self) -> S;
}

impl ToWat<String> for WebAssemblyModule {
	fn to_wat(&self) -> String {
		let mut wat = String::new();
		wat += "(module\n";
		for import in &self.imports {
			wat += &import.to_wat().indent();
		}
		for func in self.funcs.values() {
			wat += &func.to_wat(self).indent();
		}
		// TODO: what about other sections?
		wat += ")";
		wat
	}
}

impl Func {
	fn to_wat(&self, ctx: &WebAssemblyModule) -> String {
		let mut wat = String::new();
		wat += &format!("(func {}\n", self.signature.to_wat());
		for local in &self.locals {
			wat += &format!("(local {})", local.to_wat()).indent();
		}
		for instr in &self.body {
			wat += &instr.to_wat(ctx).indent();
		}
		wat += ")";
		wat
	}
}

impl Import {
	fn to_wat(&self) -> String {
		format!("(func (import {} {}){})", self.module, self.name, self.signature.to_wat())
	}
}

impl FuncType {
	fn to_wat(&self) -> String {
		let mut wat = String::new();
		if !self.parameters.is_empty() {
			wat += " (param";
			for param in &self.parameters {
				wat += param.to_wat();
			}
			wat += ")";
		}
		if !self.result.is_empty() {
			wat += " (result";
			for result in &self.result {
				wat += result.to_wat();
			}
			wat += ")";
		}
		wat
	}
}

impl ValueType {
	fn to_wat(&self) -> &'static str {
		match self {
			ValueType::I32 => "i32",
			ValueType::I64 => "i64",
			ValueType::F32 => "f32",
			ValueType::F64 => "f64",
			ValueType::FuncRef => "funcref",
			ValueType::ExternRef => "externref",
		}
	}
}

trait Indentable {
	fn indent(self) -> String;
}

impl<S: Borrow<str>> Indentable for S {
	fn indent(self) -> String {
		let lines: Vec<_> = self.borrow().split_terminator("\n").map(|line| format!("  {}\n", line)).collect();
		lines.join("\n")
	}
}

impl Instr {
	fn to_wat(&self, ctx: &WebAssemblyModule) -> String {
		use super::super::utils::map;

		match self {
			Instr::Block(typ, body) => {
				let mut wat = "(block".to_string();
				wat += &map(typ, |t| format!(" (result {})", t.to_wat())).join("");
				wat += "\n";
				wat += &map(body, |x| x.to_wat(ctx).indent()).join("");
				wat += ")";
				wat
			}
			Instr::Loop(typ, body) => {
				let mut wat = "(loop".to_string();
				wat += &map(typ, |t| format!(" (result {})", t.to_wat())).join("");
				wat += "\n";
				wat += &map(body, |x| x.to_wat(ctx).indent()).join("");
				wat += ")";
				wat
			}
			Instr::If(typ, then, otherwise) => {
				let mut wat = "(if".to_string();
				wat += &map(typ, |t| format!(" (result {})", t.to_wat())).join("");
				wat += " (then\n";
				wat += &map(then, |x| x.to_wat(ctx).indent()).join("");
				if !otherwise.is_empty() {
					wat += ") (else\n";
					wat += &map(otherwise, |x| x.to_wat(ctx).indent()).join("");
				}
				wat += "))";
				wat
			}
			Instr::CallImportedFn(func) => format!("call {}", func),
			Instr::CallFunction(func) => format!("call {}", *func as usize + ctx.imports.len()),
			Instr::Unreachable => "unreachable".into(),
			Instr::Nop => "nop".into(),
			Instr::Br { label } => format!("br {}", label),
			Instr::BrIf { label } => format!("br_if {}", label),
			Instr::BrTable { labels } => {
				format!("br_table {}", labels.iter().map(|label| label.to_string()).collect::<Vec<_>>().join(" "))
			}
			Instr::Return => "return".into(),
			Instr::CallIndirect { table, func_type } => format!("call_indirect {} {}", table, func_type),
			Instr::Drop => "drop".into(),
			Instr::LocalGet => "local.get".into(),
			Instr::LocalSet => "local.set".into(),
			Instr::LocalTee => "local.tee".into(),
			Instr::GlobalGet => "global.get".into(),
			Instr::GlobalSet => "global.set".into(),
			Instr::TableGet => "table.get".into(),
			Instr::TableSet => "table.set".into(),
			Instr::I32Load { offset, align } => format!("i32.load {} {}", offset, align),
			Instr::I64Load { offset, align } => format!("i64.load {} {}", offset, align),
			Instr::F32Load { offset, align } => format!("f32.load {} {}", offset, align),
			Instr::F64Load { offset, align } => format!("f64.load {} {}", offset, align),
			Instr::I32Load8S { offset, align } => format!("i32.load8_s {} {}", offset, align),
			Instr::I32Load8U { offset, align } => format!("i32.load8_u {} {}", offset, align),
			Instr::I32Load16S { offset, align } => format!("i32.load16_s {} {}", offset, align),
			Instr::I32Load16U { offset, align } => format!("i32.load16_u {} {}", offset, align),
			Instr::I64Load8S { offset, align } => format!("i64.load8_s {} {}", offset, align),
			Instr::I64Load8U { offset, align } => format!("i64.load8_u {} {}", offset, align),
			Instr::I64Load16S { offset, align } => format!("i64.load16_s {} {}", offset, align),
			Instr::I64Load16U { offset, align } => format!("i64.load16_u {} {}", offset, align),
			Instr::I64Load32S { offset, align } => format!("i64.load32_s {} {}", offset, align),
			Instr::I64Load32U { offset, align } => format!("i64.load32_u {} {}", offset, align),
			Instr::I32Store { offset, align } => format!("i32.store {} {}", offset, align),
			Instr::I64Store { offset, align } => format!("i64.store {} {}", offset, align),
			Instr::F32Store { offset, align } => format!("f32.store {} {}", offset, align),
			Instr::F64Store { offset, align } => format!("f64.store {} {}", offset, align),
			Instr::I32Store8 { offset, align } => format!("i32.store8 {} {}", offset, align),
			Instr::I32Store16 { offset, align } => format!("i32.store16 {} {}", offset, align),
			Instr::I64Store8 { offset, align } => format!("i64.store8 {} {}", offset, align),
			Instr::I64Store16 { offset, align } => format!("i64.store16 {} {}", offset, align),
			Instr::I64Store32 { offset, align } => format!("i64.store32 {} {}", offset, align),
			Instr::MemorySize => "memory.size".into(),
			Instr::MemoryGrow => "memory.grow".into(),
			Instr::I32Const(value) => format!("i32.const {}", value),
			Instr::I64Const(value) => format!("i64.const {}", value),
			Instr::F32Const(value) => format!("f32.const {}", value),
			Instr::F64Const(value) => format!("f64.const {}", value),
			Instr::I32Eqz => "i32.eqz".into(),
			Instr::I32Eq => "i32.eq".into(),
			Instr::I32Ne => "i32.ne".into(),
			Instr::I32LtS => "i32.lt_s".into(),
			Instr::I32LtU => "i32.lt_u".into(),
			Instr::I32GtS => "i32.gt_s".into(),
			Instr::I32GtU => "i32.gt_u".into(),
			Instr::I32LeU => "i32.le_u".into(),
			Instr::I32LeS => "i32.le_s".into(),
			Instr::I32GeS => "i32.ge_s".into(),
			Instr::I32GeU => "i32.ge_u".into(),
			Instr::I64Eqz => "i64.eqz".into(),
			Instr::I64Eq => "i64.eq".into(),
			Instr::I64Ne => "i64.ne".into(),
			Instr::I64LtS => "i64.lt_s".into(),
			Instr::I64LtU => "i64.lt_u".into(),
			Instr::I64GtS => "i64.gt_s".into(),
			Instr::I64GtU => "i64.gt_u".into(),
			Instr::I64LeU => "i64.le_u".into(),
			Instr::I64LeS => "i64.le_s".into(),
			Instr::I64GeS => "i64.ge_s".into(),
			Instr::I64GeU => "i64.ge_u".into(),
			Instr::F32Eq => "f32.eq".into(),
			Instr::F32Ne => "f32.ne".into(),
			Instr::F32Lt => "f32.lt".into(),
			Instr::F32Gt => "f32.gt".into(),
			Instr::F32Le => "f32.le".into(),
			Instr::F32Ge => "f32.ge".into(),
			Instr::F64Eq => "f64.eq".into(),
			Instr::F64Ne => "f64.ne".into(),
			Instr::F64Lt => "f64.lt".into(),
			Instr::F64Gt => "f64.gt".into(),
			Instr::F64Le => "f64.le".into(),
			Instr::F64Ge => "f64.ge".into(),
			Instr::I32Clz => "i32.clz".into(),
			Instr::I32Ctz => "i32.ctz".into(),
			Instr::I32Popcnt => "i32.popcnt".into(),
			Instr::I32Add => "i32.add".into(),
			Instr::I32Sub => "i32.sub".into(),
			Instr::I32Mul => "i32.mul".into(),
			Instr::I32DivS => "i32.div_s".into(),
			Instr::I32DivU => "i32.div_u".into(),
			Instr::I32RemS => "i32.rem_s".into(),
			Instr::I32RemU => "i32.rem_u".into(),
			Instr::I32And => "i32.and".into(),
			Instr::I32Or => "i32.or".into(),
			Instr::I32Xor => "i32.xor".into(),
			Instr::I32Shl => "i32.shl".into(),
			Instr::I32ShrS => "i32.shr_s".into(),
			Instr::I32ShrU => "i32.shr_u".into(),
			Instr::I32Rotl => "i32.rotl".into(),
			Instr::I32Rotr => "i32.rotr".into(),
			Instr::I64Clz => "i64.clz".into(),
			Instr::I64Ctz => "i64.ctz".into(),
			Instr::I64Popcnt => "i64.popcnt".into(),
			Instr::I64Add => "i64.add".into(),
			Instr::I64Sub => "i64.sub".into(),
			Instr::I64Mul => "i64.mul".into(),
			Instr::I64DivS => "i64.div_s".into(),
			Instr::I64DivU => "i64.div_u".into(),
			Instr::I64RemS => "i64.rem_s".into(),
			Instr::I64RemU => "i64.rem_u".into(),
			Instr::I64And => "i64.and".into(),
			Instr::I64Or => "i64.or".into(),
			Instr::I64Xor => "i64.xor".into(),
			Instr::I64Shl => "i64.shl".into(),
			Instr::I64ShrS => "i64.shr_s".into(),
			Instr::I64ShrU => "i64.shr_u".into(),
			Instr::I64Rotl => "i64.rotl".into(),
			Instr::I64Rotr => "i64.rotr".into(),
			Instr::F32Abs => "f32.abs".into(),
			Instr::F32Neg => "f32.neg".into(),
			Instr::F32Ceil => "f32.ceil".into(),
			Instr::F32Floor => "f32.floor".into(),
			Instr::F32Trunc => "f32.trunc".into(),
			Instr::F32Nearest => "f32.nearest".into(),
			Instr::F32Sqrt => "f32.sqrt".into(),
			Instr::F32Add => "f32.add".into(),
			Instr::F32Sub => "f32.sub".into(),
			Instr::F32Mul => "f32.mul".into(),
			Instr::F32Div => "f32.div".into(),
			Instr::F32Min => "f32.min".into(),
			Instr::F32Max => "f32.max".into(),
			Instr::F32Copysign => "f32.copysign".into(),
			Instr::F64Abs => "f64.abs".into(),
			Instr::F64Neg => "f64.neg".into(),
			Instr::F64Ceil => "f64.ceil".into(),
			Instr::F64Floor => "f64.floor".into(),
			Instr::F64Trunc => "f64.trunc".into(),
			Instr::F64Nearest => "f64.nearest".into(),
			Instr::F64Sqrt => "f64.sqrt".into(),
			Instr::F64Add => "f64.add".into(),
			Instr::F64Sub => "f64.sub".into(),
			Instr::F64Mul => "f64.mul".into(),
			Instr::F64Div => "f64.div".into(),
			Instr::F64Min => "f64.min".into(),
			Instr::F64Max => "f64.max".into(),
			Instr::F64Copysign => "f64.copysign".into(),
			Instr::I32WrapI64 => "i32.wrap_i64".into(),
			Instr::I32TruncF32S => "i32.trunc_f32_s".into(),
			Instr::I32TruncF32U => "i32.trunc_f32_u".into(),
			Instr::I32TruncF64S => "i32.trunc_f64_s".into(),
			Instr::I32TruncF64U => "i32.trunc_f64_u".into(),
			Instr::I64ExtendI32S => "i64.extend_i32_s".into(),
			Instr::I64ExtendI32U => "i64.extend_i32_u".into(),
			Instr::I64TruncF32S => "i64.trunc_f32_s".into(),
			Instr::I64TruncF32U => "i64.trunc_f32_u".into(),
			Instr::I64TruncF64S => "i64.trunc_f64_s".into(),
			Instr::I64TruncF64U => "i64.trunc_f64_u".into(),
			Instr::F32ConvertI32S => "f32.convert_i32_s".into(),
			Instr::F32ConvertI32U => "f32.convert_i32_u".into(),
			Instr::F32ConvertI64S => "f32.convert_i64_s".into(),
			Instr::F32ConvertI64U => "f32.convert_i64_u".into(),
			Instr::F32DemoteF64 => "f32.demote_f64".into(),
			Instr::F64ConvertI32S => "f64.convert_i32_s".into(),
			Instr::F64ConvertI32U => "f64.convert_i32_u".into(),
			Instr::F64ConvertI64S => "f64.convert_i64_s".into(),
			Instr::F64ConvertI64U => "f64.convert_i64_u".into(),
			Instr::F64PromoteF32 => "f64.promote_f32".into(),
			Instr::I32ReinterpretF32 => "i32.reinterpret_f32".into(),
			Instr::I64ReinterpretF64 => "i64.reinterpret_f64".into(),
			Instr::F32ReinterpretI32 => "f32.reinterpret_i32".into(),
			Instr::F64ReinterpretI64 => "f64.reinterpret_i64".into(),
			Instr::I32Extend8S => "i32.extend8_s".into(),
			Instr::I32Extend16S => "i32.extend16_s".into(),
			Instr::I64Extend8S => "i64.extend8_s".into(),
			Instr::I64Extend16S => "i64.extend16_s".into(),
			Instr::I64Extend32S => "i64.extend32_s".into(),
		}
	}

	fn to_wasm(&self, ctx: &mut CompilationContext, wasm: &mut Vec<u8>) {
		match self {
			Instr::Block(typ, body) => {
				wasm.push(0x02);
				match &typ[..] {
					[] => wasm.push(0x40),
					[typ] => wasm.push(typ.to_wasm()),
					_ => {
						let typ = FuncType { parameters: vec![], result: typ.clone() };
						wasm.pack(ctx.types.insert_full(typ).0 as i32);
					}
				}
				body.iter().for_each(|instr| instr.to_wasm(ctx, wasm));
				wasm.push(0x0B);
			}
			Instr::Loop(typ, body) => {
				wasm.push(0x03);
				match &typ[..] {
					[] => wasm.push(0x40),
					[typ] => wasm.push(typ.to_wasm()),
					_ => {
						let typ = FuncType { parameters: vec![], result: typ.clone() };
						wasm.pack(ctx.types.insert_full(typ).0 as i32);
					}
				}
				body.iter().for_each(|instr| instr.to_wasm(ctx, wasm));
				wasm.push(0x0B);
			}
			Instr::If(typ, then, otherwise) => {
				wasm.push(0x04);
				match &typ[..] {
					[] => wasm.push(0x40),
					[typ] => wasm.push(typ.to_wasm()),
					_ => {
						let typ = FuncType { parameters: vec![], result: typ.clone() };
						wasm.pack(ctx.types.insert_full(typ).0 as i32);
					}
				}
				then.iter().for_each(|instr| instr.to_wasm(ctx, wasm));
				if !otherwise.is_empty() {
					wasm.push(0x05);
					otherwise.iter().for_each(|instr| instr.to_wasm(ctx, wasm));
				}
				wasm.push(0x0B);
			}
			Instr::CallImportedFn(func) => {
				let mut wasm = vec![0x10];
				wasm.pack(*func);
			}
			Instr::CallFunction(func) => {
				let mut wasm = vec![0x10];
				wasm.pack(func + ctx.imports as u32);
			}
			// _ => todo!(),
			Instr::Unreachable => wasm.push(0x00),
			Instr::Nop => wasm.push(0x01),
			Instr::Br { label } => {
				wasm.push(0x0C);
				wasm.pack(*label);
			}
			Instr::BrIf { label } => {
				wasm.push(0x0D);
				wasm.pack(*label);
			}
			Instr::BrTable { labels } => {
				wasm.push(0x0E);
				wasm.pack(labels.len() - 1);
				labels.iter().for_each(|idx| wasm.pack(*idx));
			}
			Instr::Return => wasm.push(0x0F),
			Instr::CallIndirect { table, func_type } => {
				wasm.push(0x11);
				wasm.pack(*table);
				wasm.pack(*func_type);
			}
			Instr::Drop => wasm.push(0x1A),
			Instr::LocalGet => wasm.push(0x20),
			Instr::LocalSet => wasm.push(0x21),
			Instr::LocalTee => wasm.push(0x22),
			Instr::GlobalGet => wasm.push(0x23),
			Instr::GlobalSet => wasm.push(0x24),
			Instr::TableGet => wasm.push(0x25),
			Instr::TableSet => wasm.push(0x26),
			Instr::I32Load { offset, align } => {
				wasm.push(0x28);
				wasm.pack(*offset);
				wasm.pack(*align);
			}
			Instr::I64Load { offset, align } => {
				wasm.push(0x29);
				wasm.pack(*offset);
				wasm.pack(*align);
			}
			Instr::F32Load { offset, align } => {
				wasm.push(0x2A);
				wasm.pack(*offset);
				wasm.pack(*align);
			}
			Instr::F64Load { offset, align } => {
				wasm.push(0x2B);
				wasm.pack(*offset);
				wasm.pack(*align);
			}
			Instr::I32Load8S { offset, align } => {
				wasm.push(0x2C);
				wasm.pack(*offset);
				wasm.pack(*align);
			}
			Instr::I32Load8U { offset, align } => {
				wasm.push(0x2D);
				wasm.pack(*offset);
				wasm.pack(*align);
			}
			Instr::I32Load16S { offset, align } => {
				wasm.push(0x2E);
				wasm.pack(*offset);
				wasm.pack(*align);
			}
			Instr::I32Load16U { offset, align } => {
				wasm.push(0x2F);
				wasm.pack(*offset);
				wasm.pack(*align);
			}
			Instr::I64Load8S { offset, align } => {
				wasm.push(0x30);
				wasm.pack(*offset);
				wasm.pack(*align);
			}
			Instr::I64Load8U { offset, align } => {
				wasm.push(0x31);
				wasm.pack(*offset);
				wasm.pack(*align);
			}
			Instr::I64Load16S { offset, align } => {
				wasm.push(0x32);
				wasm.pack(*offset);
				wasm.pack(*align);
			}
			Instr::I64Load16U { offset, align } => {
				wasm.push(0x33);
				wasm.pack(*offset);
				wasm.pack(*align);
			}
			Instr::I64Load32S { offset, align } => {
				wasm.push(0x34);
				wasm.pack(*offset);
				wasm.pack(*align);
			}
			Instr::I64Load32U { offset, align } => {
				wasm.push(0x35);
				wasm.pack(*offset);
				wasm.pack(*align);
			}
			Instr::I32Store { offset, align } => {
				wasm.push(0x36);
				wasm.pack(*offset);
				wasm.pack(*align);
			}
			Instr::I64Store { offset, align } => {
				wasm.push(0x37);
				wasm.pack(*offset);
				wasm.pack(*align);
			}
			Instr::F32Store { offset, align } => {
				wasm.push(0x38);
				wasm.pack(*offset);
				wasm.pack(*align);
			}
			Instr::F64Store { offset, align } => {
				wasm.push(0x39);
				wasm.pack(*offset);
				wasm.pack(*align);
			}
			Instr::I32Store8 { offset, align } => {
				wasm.push(0x3A);
				wasm.pack(*offset);
				wasm.pack(*align);
			}
			Instr::I32Store16 { offset, align } => {
				wasm.push(0x3B);
				wasm.pack(*offset);
				wasm.pack(*align);
			}
			Instr::I64Store8 { offset, align } => {
				wasm.push(0x3C);
				wasm.pack(*offset);
				wasm.pack(*align);
			}
			Instr::I64Store16 { offset, align } => {
				wasm.push(0x3D);
				wasm.pack(*offset);
				wasm.pack(*align);
			}
			Instr::I64Store32 { offset, align } => {
				wasm.push(0x3E);
				wasm.pack(*offset);
				wasm.pack(*align);
			}
			Instr::MemorySize => wasm.push(0x3F),
			Instr::MemoryGrow => wasm.push(0x40),
			Instr::I32Const(value) => {
				wasm.push(0x41);
				wasm.pack(*value);
			}
			Instr::I64Const(value) => {
				wasm.push(0x42);
				wasm.pack(*value);
			}
			Instr::F32Const(value) => {
				wasm.push(0x43);
				wasm.extend(value.to_bits().to_ne_bytes());
			}
			Instr::F64Const(value) => {
				wasm.push(0x44);
				wasm.extend(value.to_bits().to_ne_bytes());
			}
			Instr::I32Eqz => wasm.push(0x45), Instr::I32Eq => wasm.push(0x46), Instr::I32Ne => wasm.push(0x47),
			Instr::I32LtS => wasm.push(0x48), Instr::I32LtU => wasm.push(0x49), Instr::I32GtS => wasm.push(0x4A),
			Instr::I32GtU => wasm.push(0x4B), Instr::I32LeU => wasm.push(0x4C), Instr::I32LeS => wasm.push(0x4D),
			Instr::I32GeS => wasm.push(0x4E), Instr::I32GeU => wasm.push(0x4F), Instr::I64Eqz => wasm.push(0x50),
			Instr::I64Eq => wasm.push(0x51), Instr::I64Ne => wasm.push(0x52), Instr::I64LtS => wasm.push(0x53),
			Instr::I64LtU => wasm.push(0x54), Instr::I64GtS => wasm.push(0x55), Instr::I64GtU => wasm.push(0x56),
			Instr::I64LeU => wasm.push(0x57), Instr::I64LeS => wasm.push(0x58), Instr::I64GeS => wasm.push(0x59),
			Instr::I64GeU => wasm.push(0x5A), Instr::F32Eq => wasm.push(0x5B), Instr::F32Ne => wasm.push(0x5C),
			Instr::F32Lt => wasm.push(0x5D), Instr::F32Gt => wasm.push(0x5E), Instr::F32Le => wasm.push(0x5F),
			Instr::F32Ge => wasm.push(0x60), Instr::F64Eq => wasm.push(0x61), Instr::F64Ne => wasm.push(0x62),
			Instr::F64Lt => wasm.push(0x63), Instr::F64Gt => wasm.push(0x64), Instr::F64Le => wasm.push(0x65),
			Instr::F64Ge => wasm.push(0x66), Instr::I32Clz => wasm.push(0x67), Instr::I32Ctz => wasm.push(0x68),
			Instr::I32Popcnt => wasm.push(0x69), Instr::I32Add => wasm.push(0x6A), Instr::I32Sub => wasm.push(0x6B),
			Instr::I32Mul => wasm.push(0x6C), Instr::I32DivS => wasm.push(0x6D), Instr::I32DivU => wasm.push(0x6E),
			Instr::I32RemS => wasm.push(0x6F), Instr::I32RemU => wasm.push(0x70), Instr::I32And => wasm.push(0x71),
			Instr::I32Or => wasm.push(0x72), Instr::I32Xor => wasm.push(0x73), Instr::I32Shl => wasm.push(0x74),
			Instr::I32ShrS => wasm.push(0x75), Instr::I32ShrU => wasm.push(0x76), Instr::I32Rotl => wasm.push(0x77),
			Instr::I32Rotr => wasm.push(0x78), Instr::I64Clz => wasm.push(0x79), Instr::I64Ctz => wasm.push(0x7A),
			Instr::I64Popcnt => wasm.push(0x7B), Instr::I64Add => wasm.push(0x7C), Instr::I64Sub => wasm.push(0x7D),
			Instr::I64Mul => wasm.push(0x7E), Instr::I64DivS => wasm.push(0x7F), Instr::I64DivU => wasm.push(0x80),
			Instr::I64RemS => wasm.push(0x81), Instr::I64RemU => wasm.push(0x82), Instr::I64And => wasm.push(0x83),
			Instr::I64Or => wasm.push(0x84), Instr::I64Xor => wasm.push(0x85), Instr::I64Shl => wasm.push(0x86),
			Instr::I64ShrS => wasm.push(0x87), Instr::I64ShrU => wasm.push(0x88), Instr::I64Rotl => wasm.push(0x89),
			Instr::I64Rotr => wasm.push(0x8A), Instr::F32Abs => wasm.push(0x8B), Instr::F32Neg => wasm.push(0x8C),
			Instr::F32Ceil => wasm.push(0x8D), Instr::F32Floor => wasm.push(0x8E), Instr::F32Trunc => wasm.push(0x8F),
			Instr::F32Nearest => wasm.push(0x90), Instr::F32Sqrt => wasm.push(0x91), Instr::F32Add => wasm.push(0x92),
			Instr::F32Sub => wasm.push(0x93), Instr::F32Mul => wasm.push(0x94), Instr::F32Div => wasm.push(0x95),
			Instr::F32Min => wasm.push(0x96), Instr::F32Max => wasm.push(0x97), Instr::F32Copysign => wasm.push(0x98),
			Instr::F64Abs => wasm.push(0x99), Instr::F64Neg => wasm.push(0x9A), Instr::F64Ceil => wasm.push(0x9B),
			Instr::F64Floor => wasm.push(0x9C), Instr::F64Trunc => wasm.push(0x9D), Instr::F64Nearest => wasm.push(0x9E),
			Instr::F64Sqrt => wasm.push(0x9F), Instr::F64Add => wasm.push(0xA0), Instr::F64Sub => wasm.push(0xA1),
			Instr::F64Mul => wasm.push(0xA2), Instr::F64Div => wasm.push(0xA3), Instr::F64Min => wasm.push(0xA4),
			Instr::F64Max => wasm.push(0xA5), Instr::F64Copysign => wasm.push(0xA6), Instr::I32WrapI64 => wasm.push(0xA7),
			Instr::I32TruncF32S => wasm.push(0xA8), Instr::I32TruncF32U => wasm.push(0xA9),
			Instr::I32TruncF64S => wasm.push(0xAA), Instr::I32TruncF64U => wasm.push(0xAB),
			Instr::I64ExtendI32S => wasm.push(0xAC), Instr::I64ExtendI32U => wasm.push(0xAD),
			Instr::I64TruncF32S => wasm.push(0xAE), Instr::I64TruncF32U => wasm.push(0xAF),
			Instr::I64TruncF64S => wasm.push(0xB0), Instr::I64TruncF64U => wasm.push(0xB1),
			Instr::F32ConvertI32S => wasm.push(0xB2), Instr::F32ConvertI32U => wasm.push(0xB3),
			Instr::F32ConvertI64S => wasm.push(0xB4), Instr::F32ConvertI64U => wasm.push(0xB5),
			Instr::F32DemoteF64 => wasm.push(0xB6), Instr::F64ConvertI32S => wasm.push(0xB7),
			Instr::F64ConvertI32U => wasm.push(0xB8), Instr::F64ConvertI64S => wasm.push(0xB9),
			Instr::F64ConvertI64U => wasm.push(0xBA), Instr::F64PromoteF32 => wasm.push(0xBB),
			Instr::I32ReinterpretF32 => wasm.push(0xBC), Instr::I64ReinterpretF64 => wasm.push(0xBD),
			Instr::F32ReinterpretI32 => wasm.push(0xBE), Instr::F64ReinterpretI64 => wasm.push(0xBF),
			Instr::I32Extend8S => wasm.push(0xC0), Instr::I32Extend16S => wasm.push(0xC1),
			Instr::I64Extend8S => wasm.push(0xC2), Instr::I64Extend16S => wasm.push(0xC3),
			Instr::I64Extend32S => wasm.push(0xC4),
		}
	}
}
