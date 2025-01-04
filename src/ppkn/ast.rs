#[derive(Clone, Debug, PartialEq)]
pub struct Block<'a> {
	pub source: &'a str,
	pub stmts: Vec<Stmt<'a>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Stmt<'a> {
	pub source: &'a str,
	pub kind: StmtKind<'a>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum StmtKind<'a> {
	Expression(Box<Expr<'a>>),
	Print(Box<Expr<'a>>),
	Println(Box<Expr<'a>>),
	Definition(Identifier<'a>, Typename<'a>, Box<Expr<'a>>),
	Assignment(Identifier<'a>, Box<Expr<'a>>),
	If(Box<Expr<'a>>, Block<'a>, Option<Block<'a>>),
	While(Box<Expr<'a>>, Block<'a>),
	Return(Box<Expr<'a>>),
	Function(Identifier<'a>, Vec<(Identifier<'a>, Typename<'a>)>, Block<'a>),
	Class(Identifier<'a>, Vec<(Identifier<'a>, Typename<'a>)>),
	Import(Identifier<'a>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Expr<'a> {
	pub source: &'a str,
	pub kind: ExprKind<'a>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ExprKind<'a> {
	Integer(u64),
	Float(f64),
	String(Box<str>),
	Variable(Box<str>),

	Grouping(Box<Expr<'a>>),
	Unary(UnaryOp<'a>, Box<Expr<'a>>),
	Binary(Box<Expr<'a>>, BinOp<'a>, Box<Expr<'a>>),
	FunctionCall(Box<Expr<'a>>, Vec<Expr<'a>>),
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
