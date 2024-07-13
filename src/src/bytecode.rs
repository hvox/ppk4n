use self::value::Value;
use indexmap::IndexMap;
use std::collections::HashMap;
pub mod interpreter;
pub mod value;

#[derive(Debug)]
pub struct Program {
	pub imports: IndexMap<String, ImportedFn>,
	pub functions: IndexMap<String, Fn>,
	pub exports: HashMap<String, usize>,
}

#[derive(Debug)]
pub struct ImportedFn {
	pub params: Vec<Var>,
	pub result: Vec<Type>,
	pub module: String,
	pub name: String,
}

#[derive(Debug)]
pub struct Fn {
	pub params: Vec<Var>,
	pub result: Vec<Type>,
	pub locals: Vec<Var>,
	pub body: Vec<Op>,
}

#[derive(Debug)]
pub enum Op {
	Drop,
	Return,
	Call(usize),
	Set(usize),
	Get(usize),
	Tee(usize),
	Const(Value, Type),
	IfElse(Vec<Op>, Vec<Op>),
	Binary(BinaryOp, Type),
}

#[derive(Debug)]
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

#[derive(Debug)]
pub struct Var {
	pub id: String,
	pub typ: Type,
}

#[derive(Debug)]
pub enum Type {
	I32,
}
