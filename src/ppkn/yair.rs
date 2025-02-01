// Yet another intermediate representation
#![allow(dead_code)]

use std::rc::Rc;

use indexmap::IndexMap;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Program {
	pub types: IndexMap<Str, Struct>,
	pub fns: IndexMap<Str, Function>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Struct {
	pub name: Str,
	pub type_params: Vec<Str>,
	pub fields: Vec<Variable>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Function {
	pub name: Str,
	pub type_params: Vec<Str>,
	pub params: Vec<Variable>,
	pub result: Variable,
	pub body: Expr,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Variable {
	pub name: Str,
	pub typ: Type,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Type {
	// TODO Interning
	pub path: Str,
	pub args: Vec<Type>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Expr {
	// TODO source: &'source str
	pub kind: ExprKind,
	pub typ: Type,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExprKind {
	Block(Vec<Expr>),
	DefLocal(Str),
	SetLocal(Str, Box<Expr>),
	FnCall(Str, Vec<Expr>),
	GenericFnCall(Str, Vec<Str>, Vec<Expr>),
	Return(Box<Expr>),
	While(Box<Expr>, Box<Expr>),
}

pub type Str = Rc<str>;

impl From<&str> for Type {
	fn from(value: &str) -> Self {
		Self { path: Str::from(value), args: vec![] }
	}
}
