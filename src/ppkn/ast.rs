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

impl<'a> ToString for Block<'a> {
	fn to_string(&self) -> String {
		let mut lines = String::new();
		for stmt in &self.stmts {
			lines.push_str(&stmt.to_string());
			lines.push_str("; ");
		}
		lines
	}
}

impl<'a> ToString for Stmt<'a> {
	fn to_string(&self) -> String {
		match &self.kind {
			StmtKind::Expression(expr) => expr.to_string(),
			StmtKind::Print(expr) => "print(".to_string() + &expr.to_string() + ")",
			StmtKind::Println(expr) => "println(".to_string() + &expr.to_string() + ")",
			StmtKind::Definition(_, _, expr) => "variable: type = ".to_string() + &expr.to_string(),
			StmtKind::Assignment(_, expr) => todo!(),
			StmtKind::If(expr, block, block1) => todo!(),
			StmtKind::While(expr, block) => todo!(),
			StmtKind::Return(expr) => todo!(),
			StmtKind::Function(name, vec, block) => {
				format!("fun name() {} {}{}", "{", block.to_string(), "}")
			}
			StmtKind::Class(_, vec) => todo!(),
			StmtKind::Import(_) => todo!(),
		}
	}
}

impl<'a> ToString for Expr<'a> {
	fn to_string(&self) -> String {
		match &self.kind {
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
