#![allow(unused)]
use std::ops::Deref;

use crate::utils::try_map;

#[derive(Clone, Debug, PartialEq)]
pub struct Block<'a, T> {
	pub source: &'a str,
	pub stmts: Vec<Expr<'a, T>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Expr<'a, T> {
	pub source: &'a str,
	pub kind: ExprKind<'a, T>,
	pub annotations: T,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ExprKind<'a, T> {
	Print(Box<Expr<'a, T>>),
	Println(Box<Expr<'a, T>>),
	Definition(Identifier<'a>, Typename<'a>, Box<Expr<'a, T>>),
	Assignment(Identifier<'a>, Box<Expr<'a, T>>),
	If(Box<Expr<'a, T>>, Block<'a, T>, Option<Block<'a, T>>),
	While(Box<Expr<'a, T>>, Block<'a, T>),
	Return(Box<Expr<'a, T>>),
	Function(Identifier<'a>, Vec<(Identifier<'a>, Typename<'a>)>, Block<'a, T>),
	Class(Identifier<'a>, Vec<(Identifier<'a>, Typename<'a>)>),
	Import(Identifier<'a>),

	Integer(u64),
	Float(f64),
	String(Box<str>),
	Variable(Box<str>), // TODO why do I need Box<str>?

	Grouping(Box<Expr<'a, T>>),
	Unary(UnaryOp<'a>, Box<Expr<'a, T>>),
	Binary(Box<Expr<'a, T>>, BinOp<'a>, Box<Expr<'a, T>>),
	FunctionCall(Identifier<'a>, Vec<Expr<'a, T>>),
	MethodCall(Identifier<'a>, Identifier<'a>, Vec<Expr<'a, T>>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct UnaryOp<'a> {
	pub source: &'a str,
	pub kind: UnaryOpKind,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum UnaryOpKind {
	Minus,
	Bang,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BinOp<'a> {
	pub source: &'a str,
	pub kind: BinOpKind,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum BinOpKind {
	Minus,
	Plus,
	Slash,
	Star,
	Greater,
	GreaterEqual,
	Less,
	LessEqual,
	BangEqual,
	EqualEqual,
}

pub type Identifier<'a> = &'a str;
pub type Typename<'a> = &'a str;

impl<'a> Expr<'a, ()> {
	pub fn new(source: &'a str, kind: ExprKind<'a, ()>) -> Self {
		Self::from(source, kind, ())
	}

	pub fn add_annotations<F, R>(self, mut f: F) -> Expr<'a, R>
	where
		F: FnMut() -> R,
	{
		self.map_annotations(&mut |_, _| f())
	}
}

impl<'a, T> Expr<'a, T> {
	pub fn from(source: &'a str, kind: ExprKind<'a, T>, data: T) -> Self {
		Self { source, kind, annotations: data }
	}

	pub fn map_annotations<F, R>(self, f: &mut F) -> Expr<'a, R>
	where
		F: FnMut(T, &ExprKind<'a, R>) -> R,
	{
		let kind = self.kind.map_annotations(f);
		let annotations = f(self.annotations, &kind);
		Expr { source: self.source, kind, annotations }
	}

	pub fn try_map_annotations<F, R, E>(self, f: &mut F) -> Result<Expr<'a, R>, E>
	where
		F: FnMut(T, &'a str, &ExprKind<'a, R>) -> Result<R, E>,
	{
		let kind = self.kind.try_map_annotations(f)?;
		let annotations = f(self.annotations, self.source, &kind)?;
		Ok(Expr { source: self.source, kind, annotations })
	}

	pub fn try_foreach<F, E>(self, f: &mut F) -> Result<Self, E>
	where
		F: FnMut(T, &'a str, &ExprKind<'a, T>) -> Result<(), E>,
		T: Clone,
	{
		self.try_map_annotations(&mut |x, source, kind| {
			f(x.clone(), source, kind)?;
			Ok(x)
		})
	}
}

impl<'a, T> ExprKind<'a, T> {
	pub fn map_annotations<F, R>(self, f: &mut F) -> ExprKind<'a, R>
	where
		F: FnMut(T, &ExprKind<'a, R>) -> R,
	{
		use ExprKind::*;
		match self {
			Print(expr) => Print(Box::new(expr.map_annotations(f))),
			Println(expr) => Println(Box::new(expr.map_annotations(f))),
			Definition(var, typ, expr) => Definition(var, typ, Box::new(expr.map_annotations(f))),
			Assignment(var, expr) => Assignment(var, Box::new(expr.map_annotations(f))),
			If(expr, then, otherwise) => If(
				Box::new(expr.map_annotations(f)),
				Block {
					source: then.source,
					stmts: then.stmts.into_iter().map(|stmt| stmt.map_annotations(f)).collect(),
				},
				otherwise.map(|block| Block {
					source: block.source,
					stmts: block.stmts.into_iter().map(|stmt| stmt.map_annotations(f)).collect(),
				}),
			),
			While(expr, block) => While(
				Box::new(expr.map_annotations(f)),
				Block {
					source: block.source,
					stmts: block.stmts.into_iter().map(|stmt| stmt.map_annotations(f)).collect(),
				},
			),
			Return(expr) => Return(Box::new(expr.map_annotations(f))),
			Grouping(expr) => Grouping(Box::new(expr.map_annotations(f))),
			Unary(op, expr) => Unary(op, Box::new(expr.map_annotations(f))),
			Binary(lhs, op, rhs) => {
				Binary(Box::new(lhs.map_annotations(f)), op, Box::new(rhs.map_annotations(f)))
			}
			FunctionCall(name, args) => {
				FunctionCall(name, args.into_iter().map(|stmt| stmt.map_annotations(f)).collect())
			}
			MethodCall(obj, func, args) => {
				MethodCall(obj, func, args.into_iter().map(|stmt| stmt.map_annotations(f)).collect())
			}
			Variable(x) => Variable(x),
			Integer(x) => Integer(x),
			String(x) => String(x),
			Float(x) => Float(x),
			_ => unreachable!(),
		}
	}

	pub fn try_map_annotations<F, R, E>(self, f: &mut F) -> Result<ExprKind<'a, R>, E>
	where
		F: FnMut(T, &'a str, &ExprKind<'a, R>) -> Result<R, E>,
	{
		use ExprKind::*;
		Ok(match self {
			Print(expr) => Print(Box::new(expr.try_map_annotations(f)?)),
			Println(expr) => Println(Box::new(expr.try_map_annotations(f)?)),
			Definition(var, typ, expr) => Definition(var, typ, Box::new(expr.try_map_annotations(f)?)),
			Assignment(var, expr) => Assignment(var, Box::new(expr.try_map_annotations(f)?)),
			If(expr, then, otherwise) => If(
				Box::new(expr.try_map_annotations(f)?),
				Block {
					source: then.source,
					stmts: try_map(then.stmts, |stmt| stmt.try_map_annotations(f))?,
				},
				match otherwise {
					Some(block) => Some(Block {
						source: block.source,
						stmts: try_map(block.stmts, |stmt| stmt.try_map_annotations(f))?,
					}),
					None => None,
				},
			),
			While(expr, block) => While(
				Box::new(expr.try_map_annotations(f)?),
				Block {
					source: block.source,
					stmts: try_map(block.stmts, |stmt| stmt.try_map_annotations(f))?,
				},
			),
			Return(expr) => Return(Box::new(expr.try_map_annotations(f)?)),
			Grouping(expr) => Grouping(Box::new(expr.try_map_annotations(f)?)),
			Unary(op, expr) => Unary(op, Box::new(expr.try_map_annotations(f)?)),
			Binary(lhs, op, rhs) => {
				Binary(Box::new(lhs.try_map_annotations(f)?), op, Box::new(rhs.try_map_annotations(f)?))
			}
			FunctionCall(name, args) => {
				FunctionCall(name, try_map(args, |arg| arg.try_map_annotations(f))?)
			}
			MethodCall(obj, func, args) => {
				MethodCall(obj, func, try_map(args, |arg| arg.try_map_annotations(f))?)
			}
			Variable(x) => Variable(x),
			Integer(x) => Integer(x),
			String(x) => String(x),
			Float(x) => Float(x),
			_ => unreachable!(),
		})
	}
}

impl<'a, T> Deref for Expr<'a, T> {
	type Target = T;

	fn deref(&self) -> &Self::Target {
		&self.annotations
	}
}

impl<'a, T> ToString for Block<'a, T>
where
	T: Clone,
{
	fn to_string(&self) -> String {
		let mut lines = String::new();
		for stmt in &self.stmts {
			lines.push_str(&stmt.to_string());
			lines.push_str("; ");
		}
		lines
	}
}

impl<'a, T> ToString for Expr<'a, T>
where
	T: Clone,
{
	fn to_string(&self) -> String {
		match &self.kind {
			ExprKind::Print(expr) => "print(".to_string() + &expr.to_string() + ")",
			ExprKind::Println(expr) => "println(".to_string() + &expr.to_string() + ")",
			ExprKind::Definition(_, _, expr) => "variable: type = ".to_string() + &expr.to_string(),
			ExprKind::Assignment(_, expr) => todo!(),
			ExprKind::If(expr, block, block1) => todo!(),
			ExprKind::While(expr, block) => todo!(),
			ExprKind::Return(expr) => todo!(),
			ExprKind::Function(name, vec, block) => {
				format!("fun name() {} {}{}", "{", block.to_string(), "}")
			}
			ExprKind::Class(_, vec) => todo!(),
			ExprKind::Import(_) => todo!(),

			ExprKind::Integer(_) => String::from("number"),
			ExprKind::String(_) => String::from("string"),
			ExprKind::Variable(_) => String::from("variable"),
			ExprKind::Grouping(expr) => todo!(),
			ExprKind::Unary(unary_op, expr) => todo!(),
			ExprKind::Binary(lhs, bin_op, rhs) => {
				"(".to_string()
					+ &lhs.to_string() + " "
					+ match bin_op.kind {
						BinOpKind::Minus => "-",
						BinOpKind::Plus => "+",
						BinOpKind::Slash => "/",
						BinOpKind::Star => "*",
						BinOpKind::Greater => ">",
						BinOpKind::GreaterEqual => ">=",
						BinOpKind::Less => "<",
						BinOpKind::LessEqual => "<=",
						BinOpKind::BangEqual => "!=",
						BinOpKind::EqualEqual => "==",
					} + " " + &rhs.to_string()
					+ ")"
			}
			_ => todo!(),
		}
	}
}
