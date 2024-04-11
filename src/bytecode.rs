use self::value::Value;
use indexmap::IndexMap;
use std::collections::HashMap;
pub mod interpreter;
pub mod value;

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
	Const(Value, Type),
	Binary(BinaryOp, Type),
}

pub enum BinaryOp {
	Add,
	Sub,
	Mul,
}

impl Op {
	pub fn i32(value: i32) -> Op {
		Op::Const(value.into(), Type::I32)
	}
}

pub struct Var {
	pub id: String,
	pub typ: Type,
}

pub enum Type {
	I32,
}
