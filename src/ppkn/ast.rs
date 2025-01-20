use std::ops::Deref;

#[derive(Clone, Debug, PartialEq)]
pub struct Block<'a, T> {
	pub source: &'a str,
	pub stmts: Vec<Expr<'a, T>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Expr<'a, T> {
	pub source: &'a str,
	pub kind: ExprKind<'a, T>,
	pub additional_data: T,
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
	Variable(Box<str>),

	Grouping(Box<Expr<'a, T>>),
	Unary(UnaryOp<'a>, Box<Expr<'a, T>>),
	Binary(Box<Expr<'a, T>>, BinOp<'a>, Box<Expr<'a, T>>),
	FunctionCall(Box<Expr<'a, T>>, Vec<Expr<'a, T>>),
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
}

impl<'a, T> Expr<'a, T> {
	pub fn from(source: &'a str, kind: ExprKind<'a, T>, data: T) -> Self {
		Self { source, kind, additional_data: data }
	}
}

impl<'a, T> Deref for Expr<'a, T> {
	type Target = T;

	fn deref(&self) -> &Self::Target {
		&self.additional_data
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
