// Yet another intermediate representation again
#![allow(unused)]
use std::rc::Rc;

use indexmap::IndexMap;

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
	pub fns: IndexMap<Str, Function>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
	pub name: Str,
	pub signature: FnSignature,
	pub parameters: Vec<Str>,
	pub body: Code<Type>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FnSignature {
	pub params: Vec<Type>,
	pub result: Type,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
	Unit,
	Bool,
	I32,
	U32,
	F32,
	Str,
	Array(Rc<Type>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Code<Type> {
	// TODO: add field
	// source: &'source str
	pub kind: CodeKind<Type>,
	pub typ: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub enum CodeKind<Type> {
	Unit,
	Unreachable,
	Block(Vec<Code<Type>>),
	DefLocal(Str, Box<Code<Type>>),
	SetLocal(Str, Box<Code<Type>>),
	GetLocal(Str),
	FnCall(Str, Vec<Code<Type>>),
	MethodCall(Str, Box<Code<Type>>, Vec<Code<Type>>),
	Return(Box<Code<Type>>),
	While(Box<Code<Type>>, Box<Code<Type>>),
	If(Box<Code<Type>>, Box<Code<Type>>, Option<Box<Code<Type>>>),
	Literal(Literal),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Literal {
	Number(u64),
	String(Str),
}

pub type Str = Rc<str>;

impl Literal {
	fn as_number(&self) -> u64 {
		match self {
			Literal::Number(value) => *value,
			Literal::String(_) => unreachable!(),
		}
	}

	fn as_i32(&self) -> i32 {
		self.as_number() as i32
	}

	fn as_u32(&self) -> u32 {
		self.as_number() as u32
	}
}

impl<T> Code<T> {
	pub fn map<R>(self, f: &mut impl FnMut(T) -> R) -> Code<R> {
		Code { kind: self.kind.map(f), typ: f(self.typ) }
	}
}

impl<T> CodeKind<T> {
	pub fn map<R>(self, f: &mut impl FnMut(T) -> R) -> CodeKind<R> {
		match self {
			CodeKind::Unit => CodeKind::Unit,
			CodeKind::Unreachable => CodeKind::Unreachable,
			CodeKind::Block(stmts) => CodeKind::Block(stmts.into_iter().map(|stmt| stmt.map(f)).collect()),
			CodeKind::DefLocal(name, value) => CodeKind::DefLocal(name, value.map(f).into()),
			CodeKind::SetLocal(name, value) => CodeKind::SetLocal(name, value.map(f).into()),
			CodeKind::GetLocal(name) => CodeKind::GetLocal(name),
			CodeKind::FnCall(name, args) => CodeKind::FnCall(name, args.into_iter().map(|arg| arg.map(f)).collect()),
			CodeKind::MethodCall(name, object, args) => {
				CodeKind::MethodCall(name, object.map(f).into(), args.into_iter().map(|arg| arg.map(f)).collect())
			}
			CodeKind::Return(result) => CodeKind::Return(result.map(f).into()),
			CodeKind::While(condition, body) => CodeKind::While(condition.map(f).into(), body.map(f).into()),
			CodeKind::If(condition, then, otherwise) => CodeKind::If(
				condition.map(f).into(),
				then.map(f).into(),
				otherwise.map(|some| some.map(f).into()).into(),
			),
			CodeKind::Literal(literal) => CodeKind::Literal(literal),
		}
	}
}
