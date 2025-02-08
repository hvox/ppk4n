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
	pub body: Expr<Type>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FnSignature {
	pub params: Vec<Type>,
	pub result: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block<Type> {
	// TODO: add field
	// source: &'source str
	pub stmts: Vec<Stmt<Type>>,
	pub result: Option<Box<Expr<Type>>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt<Type> {
	// TODO: add field
	// source: &'source str
	Expr(Expr<Type>),
	Def(Str, Expr<Type>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expr<Type> {
	// TODO: add field
	// source: &'source str
	pub kind: ExprKind<Type>,
	pub typ: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind<Type> {
	Value(Literal),
	GetLocal(Str),
	SetLocal(Str, Box<Expr<Type>>),
	Block(Block<Type>),
	While(Box<Expr<Type>>, Box<Expr<Type>>),
	If(Box<Expr<Type>>, Box<Expr<Type>>, Option<Box<Expr<Type>>>),
	MethodCall(Box<Expr<Type>>, Str, Vec<Expr<Type>>),
	FnCall(Str, Vec<Expr<Type>>),
	Return(Box<Expr<Type>>),
	Unreachable,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Literal {
	Number(u64),
	String(Str),
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

impl<T> Block<T> {
	pub fn map<R>(self, f: &mut impl FnMut(T) -> R) -> Block<R> {
		let stmts = self.stmts.into_iter().map(|stmt| stmt.map(f)).collect();
		let result = self.result.map(|res| res.map(f).into());
		Block { stmts, result }
	}
}

impl<T> Stmt<T> {
	pub fn map<R>(self, f: &mut impl FnMut(T) -> R) -> Stmt<R> {
		match self {
			Stmt::Expr(expr) => Stmt::Expr(expr.map(f)),
			Stmt::Def(name, value) => Stmt::Def(name, value.map(f)),
		}
	}
}

impl<T> Expr<T> {
	pub fn map<R>(self, f: &mut impl FnMut(T) -> R) -> Expr<R> {
		Expr { kind: self.kind.map(f), typ: f(self.typ) }
	}
}

impl<T> ExprKind<T> {
	pub fn map<R>(self, f: &mut impl FnMut(T) -> R) -> ExprKind<R> {
		match self {
			ExprKind::Unreachable => ExprKind::Unreachable,
			ExprKind::Block(block) => ExprKind::Block(block.map(f)),
			ExprKind::SetLocal(name, value) => ExprKind::SetLocal(name, value.map(f).into()),
			ExprKind::GetLocal(name) => ExprKind::GetLocal(name),
			ExprKind::FnCall(name, args) => ExprKind::FnCall(name, args.into_iter().map(|arg| arg.map(f)).collect()),
			ExprKind::MethodCall(object, method, args) => ExprKind::MethodCall(
				object.map(f).into(), method.into(), args.into_iter().map(|arg| arg.map(f)).collect(),
			),
			ExprKind::Return(result) => ExprKind::Return(result.map(f).into()),
			ExprKind::While(condition, body) => ExprKind::While(condition.map(f).into(), body.map(f).into()),
			ExprKind::If(condition, then, otherwise) => ExprKind::If(
				condition.map(f).into(),
				then.map(f).into(),
				otherwise.map(|some| some.map(f).into()).into(),
			),
			ExprKind::Value(literal) => ExprKind::Value(literal),
		}
	}
}
