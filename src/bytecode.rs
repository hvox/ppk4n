use indexmap::IndexMap;
use std::collections::HashMap;
pub mod interpreter;

pub struct Mod {
	pub imports: IndexMap<String, ImportedFn>,
	pub functions: IndexMap<String, Fn>,
	pub exports: HashMap<String, usize>,
}

pub struct ImportedFn {
	pub params: Vec<Var>,
	pub result: Vec<Type>,
	pub module: String,
	pub name: String,
}

pub struct Fn {
	pub params: Vec<Var>,
	pub result: Vec<Type>,
	pub locals: Vec<Var>,
	pub body: Vec<Op>,
}

pub enum Op {
	Set(usize),
	Get(usize),
	Tee(usize),
	Const(i32),
	Add,
	Sub,
	Mult,
}

pub struct Var {
	pub id: String,
	pub typ: Type,
}

pub enum Type {
	I32,
}

#[derive(Clone, Debug, Copy)]
pub enum Value {
	None,
	I32(i32),
}

impl Value {
	fn new(typ: &Type) -> Value {
		match typ {
			Type::I32 => Value::I32(0),
		}
	}
}
