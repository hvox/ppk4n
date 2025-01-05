use indexmap::IndexMap;

use super::ast::Stmt;

// TODO: rename to module
#[derive(Debug)]
pub struct Program<'a> {
	pub functions: IndexMap<String, Function<'a>>,
	// types: IndexMap<String, Type>
	pub phantom: std::marker::PhantomData<&'a str>,
}

#[derive(Debug)]
pub struct Function<'a> {
	pub signature: FnType,
	pub locals: IndexMap<String, Type>,
	pub code: Vec<Instr<'a>>, // body
}

#[derive(Clone, Debug)]
pub struct FnType {
	arguments: Box<[Type]>,
	result: Type,
}

#[derive(Clone, Copy, Debug)]
pub enum Type {
	I64,
	F64,
	Str,
	Void,
}

#[derive(Clone, Debug)]
pub struct Instr<'a> {
	pub source: &'a str,
	pub kind: InstrKind<'a>,
}

#[derive(Clone, Debug)]
pub enum InstrKind<'a> {
	Definition(usize, Op<'a>),
	Assignment(usize, Op<'a>),
	Return(Op<'a>),
	While(Op<'a>, Vec<Instr<'a>>),
	Print(Op<'a>),
}

#[derive(Clone, Debug)]
pub struct Op<'a> {
	pub source: &'a str,
	pub kind: OpKind<'a>,
}

#[derive(Clone, Debug)]
pub enum OpKind<'a> {
	Integer(i64),
	Float(f64),
	Variable(usize),
	String(String),
	Call(usize, Vec<Op<'a>>),
	AddI64(Box<Op<'a>>, Box<Op<'a>>),
	SubI64(Box<Op<'a>>, Box<Op<'a>>),
	MulI64(Box<Op<'a>>, Box<Op<'a>>),
	DivI64(Box<Op<'a>>, Box<Op<'a>>),
	AddF64(Box<Op<'a>>, Box<Op<'a>>),
	LessI64(Box<Op<'a>>, Box<Op<'a>>),
}

impl FnType {
	pub fn new(arguments: &[Type], result: Type) -> Self {
		Self { arguments: arguments.into(), result }
	}
}
