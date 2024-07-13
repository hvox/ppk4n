mod leb128;
use std::collections::HashMap;

use leb128::{Reader, Writer};

pub struct Wasm {
	pub imports: Vec<(FunctionType, String, String)>,
	pub exports: HashMap<String, usize>,
	pub start_function: Option<usize>,
	pub functions: Vec<Function>,
}

#[derive(Clone)]
pub struct Function {
	pub signature: FunctionType,
	pub locals: Vec<DataType>,
	pub body: Vec<Instr>
}

#[derive(Clone)]
pub struct FunctionType {
	pub params: Vec<DataType>,
	pub resuls: Vec<DataType>,
}

#[derive(Clone)]
pub enum DataType {
	I32,
	I64,
	F32,
	F64,
}
