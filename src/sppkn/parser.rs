#![allow(unused)]

use std::collections::HashMap;
use std::rc::Rc;

use indexmap::IndexMap;

use super::lexer::Token;
use super::lexer::TokenKind;
use super::lexer::tokenize;
use super::error::SyntaxError;

pub fn parse(source: &str) -> (Ast, Vec<SyntaxError>) {
	Parser::new(source).parse()
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Ast {
	pub location: Str,
	pub globals: IndexMap<Str, VarDef>,
	pub dependencies: Vec<Dependency>,
	pub functions: IndexMap<Str, FunDef>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct VarDef {
	pub location: (u32, u32),
	pub name: Str,
	pub mutable: bool,
	pub typename: Typename,
	pub value: Option<Rc<Expr>>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Dependency {
	pub location: (u32, u32),
	pub name: Str,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FunDef {
	pub location: usize,
	pub name: Str,
	pub params: Vec<FunParameter>,
	pub result: Typename,
	pub body: Rc<Expr>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FunParameter {
	pub location: usize,
	pub name: Str,
	pub typ: Typename,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Typename {
	pub location: (u32, u32),
	pub kind: TypenameKind,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TypenameKind {
	Tuple(Vec<VarDef>),
	Array(Box<Typename>),
	Name(Str),
	Unknown,
	Void,
}

#[derive(PartialEq, Eq, Clone)]
pub struct Expr {
	pub location: (u32, u32),
	pub kind: ExprKind,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ExprKind {
	String(Str),
	Integer(Str),
	Identifier(Str),
	Assignment(Str, Rc<Expr>),
	Block(Indented),
	While(Rc<Expr>, Rc<Expr>),
	If(Rc<Expr>, Rc<Expr>, Rc<Expr>),
	MethodCall(Rc<Expr>, Str, Vec<Expr>),
	FnCall(Str, Vec<Expr>),
	Return(Rc<Expr>),
	Unreachable,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Indented {
	pub location: (u32, u32),
	pub stmts: Vec<Stmt>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Stmt {
	DefLocal(VarDef),
	Expression(Expr),
}

type Str = Rc<str>;

struct Parser<'src> {
	source: &'src str,
	tokens: Vec<Token>,
	errors: Vec<SyntaxError>,
	cache: HashMap<usize, (Rc<Expr>, usize)>,
}

const DEBUG_LOGGING: bool = false;

impl<'s> Parser<'s> {
	fn new(source: &'s str) -> Self {
		let tokens = tokenize(source);
		Self { source, tokens, errors: vec![], cache: HashMap::new() }
	}

	fn parse(mut self) -> (Ast, Vec<SyntaxError>) {
		let mut dependencies: Vec<Dependency> = vec![];
		let mut functions: IndexMap<Str, FunDef> = IndexMap::new();
		let mut globals: IndexMap<Rc<str>, VarDef> = IndexMap::new();
		let mut position = 0;

		use TokenKind::*;
		while self.tokens[position].kind != Eof {
			match self.tokens[position].kind {
				Use => {
					if let Ok((name, pos)) = self.try_parse_identifier(position + 1) {
						dependencies.push(Dependency { location: self.tokens[position + 1].span(), name });
						position = pos;
					} else {
						self.errors.push(SyntaxError::new(self.tokens[position + 1].span(), "Expected module name"));
					}
				}
				Let => {
					// parse global
					todo!()
				}
				Fun => {
					if let Ok((function, pos)) = self.parse_function(position) {
						functions.insert(function.name.clone(), function);
						position = pos;
					} else {
						position = self.skip_line(position);
					}
				}
				Linend => position += 1,
				Indent => {
					self.errors.push(SyntaxError::new(self.tokens[position].span(), "Unexpected indent"));
					position = self.skip_block(position + 1);
				}
				_ => {
					self.errors.push(SyntaxError::new(self.tokens[position].span(), "Unexpected unfunny code"));
					position = self.skip_line(position + 1);
				}
			}
		}
		let ast = Ast { location: "main".into(), globals, dependencies, functions };
		(ast, self.errors)
	}

	fn parse_function(&mut self, position: usize) -> Result<(FunDef, usize), ()> {
		use TokenKind::*;
		let mut params: Vec<FunParameter> = vec![];
		let pos = self.parse_token(position, Fun)?;
		let (name, pos) = self.parse_identifier(pos)?;
		let mut param_pos = self.parse_token(pos, LeftParen)?;
		while let Ok((name, pos)) = self.try_parse_identifier(param_pos) {
			let pos = self.parse_token(pos, Colon)?;
			let (typ, pos) = self.parse_typename(pos)?;
			params.push(FunParameter { location: param_pos, name, typ });
			param_pos = self.try_parse_token(pos, Comma).unwrap_or(pos);
		}
		let pos = self.parse_token(param_pos, RightParen)?;
		let (result, pos) = if let Ok(pos) = self.try_parse_token(pos, RightArrow) {
			self.parse_typename(pos)?
		} else {
			(Typename { location: self.tokens[pos].span(), kind: TypenameKind::Void }, pos)
		};
		let (body, pos) = self.parse_expr(pos)?;
		let function = FunDef { location: position, name, params, result, body };
		Ok((function, pos))
	}

	fn parse_expr(&mut self, position: usize) -> Result<(Rc<Expr>, usize), ()> {
		if DEBUG_LOGGING {
			eprintln!("  parse_expr({:?})", position);
		}
		fn precedence(operator: TokenKind) -> u32 {
			match operator {
				Dot => 110,
				LeftParen => 100,
				Star | Slash | Rem => 90,
				Plus | Minus => 80,
				// LeftShift | RightShift => 70,
				// BinAnd => 60,
				// BinOr => 50,
				EqualEqual | BangEqual | Less | Greater | LessEqual | GreaterEqual => 40,
				And => 30,
				Or => 20,
				Equal => 10,
				_ => 0,
			}
		}
		use TokenKind::*;
		let start = self.tokens[position].span().0;
		if let Some((expr, pos)) = self.cache.get(&position) {
			return Ok((expr.clone(), *pos));
		}
		let (expr, mut pos) = self.parse_unary(position)?;
		let mut ops: Vec<Token> = vec![];
		let mut args = vec![expr];
		while args.len() != 1 || precedence(self.tokens[pos].kind) > 0 {
			if ops.last().is_some_and(|op| precedence(op.kind) >= precedence(self.tokens[pos].kind)) {
				let op = ops.pop().unwrap();
				let y = args.pop().unwrap();
				let x = args.pop().unwrap();
				let method_name = match op.kind {
					Dot => todo!(), // New expr kind?
					Minus => "sub",
					Plus => "add",
					Slash => "div",
					Star => "mul",
					Rem => "rem",
					BangEqual => "ne",
					EqualEqual => "eq",
					Greater => "gt",
					GreaterEqual => "ge",
					Less => "lt",
					LessEqual => "le",
					And => todo!(),
					Or => todo!(),
					Equal => todo!(),
					LeftParen => {
						let kind = match &x.kind {
							ExprKind::Identifier(name) => ExprKind::FnCall(name.clone(), vec![(*y).clone()]),
							_ => todo!(),
						};
						let z = Expr { location: x.location, kind };
						args.push(Rc::new(z));
						continue;
					}
					_ => unreachable!("{:?}", op),
				};
				let z = Expr { location: op.span(), kind: ExprKind::MethodCall(x, method_name.into(), vec![(*y).clone()]) };
				args.push(Rc::new(z));
			} else {
				ops.push(self.tokens[pos]);
				let expr;
				(expr, pos) = if self.tokens[pos].kind == LeftParen {
					let (expr, pos) = self.parse_expr(pos + 1)?;
					let pos = self.parse_token(pos, RightParen)?;
					(expr, pos)
				} else {
					self.parse_unary(pos + 1)?
				};
				args.push(expr);
			}
		}
		let expr = args.pop().unwrap();
		self.cache.insert(position, (expr.clone(), pos));
		Ok((expr, pos))
	}

	fn parse_unary(&mut self, position: usize) -> Result<(Rc<Expr>, usize), ()> {
		if DEBUG_LOGGING {
			eprintln!("  parse_unary({:?})", self.tokens[position]);
		}
		use TokenKind::*;
		match self.tokens[position].kind {
			LeftParen => {
				let (expr, pos) = self.parse_expr(position + 1)?;
				let pos = self.parse_token(pos, RightParen)?;
				return Ok((expr, pos));
			}
			LeftBrace => todo!(),   // Map
			LeftBracket => todo!(), // Array
			Minus => {
				let (expr, pos) = self.parse_unary(position + 1)?;
				let expr =
					Expr { location: self.tokens[position].span(), kind: ExprKind::MethodCall(expr, "neg".into(), vec![]) };
				return Ok((expr.into(), pos));
			}
			Plus => {
				let (expr, pos) = self.parse_unary(position + 1)?;
				return Ok((expr, pos));
			}
			Identifier => {
				let (name, pos) = self.parse_identifier(position)?;
				let expr = Expr { location: self.tokens[position].span(), kind: ExprKind::Identifier(name) };
				return Ok((expr.into(), pos));
			}
			Integer => {
				let (value, pos) = self.parse_identifier(position)?;
				let expr = Expr { location: self.tokens[position].span(), kind: ExprKind::Integer(value) };
				return Ok((expr.into(), pos));
			}
			String => {
				let value = self.parse_string(position);
				let expr = Expr { location: self.tokens[position].span(), kind: ExprKind::String(value) };
				return Ok((expr.into(), position + 1));
			}
			Decimal => {
				let (value, pos) = self.parse_identifier(position)?;
				let expr = Expr { location: self.tokens[position].span(), kind: ExprKind::Integer(value) };
				return Ok((expr.into(), pos));
			}
			UnterminatedString => {
				let value = self.parse_string(position);
				let expr = Expr { location: self.tokens[position].span(), kind: ExprKind::String(value) };
				return Ok((expr.into(), position + 1));
			}
			Indent => {
				let (block, pos) = self.parse_block(position + 1);
				let expr = Expr { location: self.tokens[position].span(), kind: ExprKind::Block(block) };
				return Ok((expr.into(), pos));
			}
			Linend | Dedent => {
				let expr = Expr { location: self.tokens[position].span(), kind: ExprKind::Unreachable };
				self.errors.push(SyntaxError::new(self.tokens[position].span(), "Expected expression"));
				return Ok((expr.into(), position));
			}
			_ => {
				let expr = Expr { location: self.tokens[position].span(), kind: ExprKind::Unreachable };
				self.errors.push(SyntaxError::new(self.tokens[position].span(), "Expected expression"));
				return Ok((expr.into(), position + 1));
			}
		}
	}

	fn parse_typename(&mut self, position: usize) -> Result<(Typename, usize), ()> {
		use TokenKind::*;
		match self.tokens[position].kind {
			Identifier => {
				let (name, pos) = self.parse_identifier(position)?;
				let typ = Typename { location: self.tokens[position].span(), kind: TypenameKind::Name(name) };
				Ok((typ, pos))
			}
			LeftParen => {
				if self.tokens[position + 1].kind == RightParen {
					let typ = Typename {
						location: (self.tokens[position].span().0, self.tokens[position + 1].span().1),
						kind: TypenameKind::Void,
					};
					return Ok((typ, position + 2));
				}
				let mut fields: Vec<VarDef> = vec![];
				let mut variable_pos = position + 1;
				while self.tokens[variable_pos].kind != RightParen {
					let (field, pos) = self.parse_field(variable_pos)?;
					fields.push(field);
					variable_pos = self.parse_token(pos, Comma).unwrap_or(pos);
				}
				let end_position = variable_pos + 1;
				let typ = Typename {
					location: (self.tokens[position].span().0, self.tokens[end_position - 1].span().1),
					kind: TypenameKind::Tuple(fields),
				};
				Ok((typ, end_position))
			}
			LeftBracket => {
				let (typ, pos) = self.parse_typename(position + 1)?;
				let pos = self.parse_token(pos, RightBracket)?;
				let typ = Typename {
					location: (self.tokens[position].span().0, self.tokens[pos - 1].span().1),
					kind: TypenameKind::Array(typ.into()),
				};
				Ok((typ, pos))
			}
			_ => {
				self.errors.push(SyntaxError::new(self.tokens[position].span(), "Expected typename"));
				Err(())
			}
		}
	}

	fn parse_block(&mut self, mut position: usize) -> (Indented, usize) {
		if DEBUG_LOGGING {
			eprintln!("parse_block({:?})", position);
		}
		let mut stmts = Vec::new();
		let mut location = self.tokens[position].span();
		while self.tokens[position].kind != TokenKind::Dedent {
			let (stmt, pos) = self.parse_stmt(position);
			location = (self.tokens[position].span().0, self.tokens[pos].span().1);
			position = pos;
			stmts.push(stmt);
			if !matches!(self.tokens[pos].kind, TokenKind::Linend | TokenKind::Indent) {
				self.errors.push(SyntaxError::new(self.tokens[position].span(), "Expected end of line"));
				position = self.skip_line(position);
			}
			while self.tokens[position].kind == TokenKind::Linend {
				position += 1;
			}
		}
		(Indented { stmts, location }, position + 1)
	}

	fn parse_field(&mut self, position: usize) -> Result<(VarDef, usize), ()> {
		use TokenKind::*;
		let (name, pos) = if self.tokens[position].kind == Identifier {
			let (name, pos) = self.parse_identifier(position)?;
			(name, self.parse_token(pos, Colon)?)
		} else {
			("".into(), position)
		};
		let (typ, pos) = self.parse_typename(pos)?;
		let (expr, pos) = if self.tokens[pos].kind == Equal {
			let (expr, pos) = self.parse_expr(pos + 1)?;
			(Some(expr.into()), pos)
		} else {
			(None, pos)
		};
		Ok((VarDef { location: self.tokens[position].span(), name, mutable: true, typename: typ, value: expr }, pos))
	}

	fn parse_stmt(&mut self, position: usize) -> (Stmt, usize) {
		if DEBUG_LOGGING {
			eprintln!(" parse_stmt({:?})", position);
		}
		use TokenKind::*;
		if matches!(self.tokens[position].kind, Let | Mut) {
			let id_pos = position;
			let Ok((name, pos)) = self.parse_identifier(id_pos) else {
				return (
					Stmt::Expression(Expr { location: self.tokens[id_pos].span(), kind: ExprKind::Unreachable }),
					self.skip_line(id_pos),
				);
			};
			let (typename, pos) = if self.tokens[pos].kind == Colon {
				self.parse_typename(pos + 1).unwrap()
			} else {
				(Typename { location: self.tokens[id_pos].span(), kind: TypenameKind::Unknown }, pos)
			};
			if self.tokens[pos].kind != Equal {
				self.errors.push(SyntaxError::new(self.tokens[pos].span(), "Expected `=`"));
				return (
					Stmt::Expression(Expr { location: self.tokens[id_pos].span(), kind: ExprKind::Unreachable }),
					self.skip_line(pos),
				);
			}
			let (expr, pos) = self
				.parse_expr(pos + 1)
				.unwrap_or((Expr { location: self.tokens[pos].span(), kind: ExprKind::Unreachable }.into(), pos + 1));
			let definition = VarDef {
				name,
				location: self.tokens[id_pos].span(),
				mutable: self.tokens[position].kind == Mut,
				typename,
				value: Some(expr),
			};
			return (Stmt::DefLocal(definition), pos);
		}
		let (expr, pos) = self
			.parse_expr(position)
			.unwrap_or((Expr { location: self.tokens[position].span(), kind: ExprKind::Unreachable }.into(), position));
		(Stmt::Expression((*expr).clone()), pos)
	}

	fn try_parse_token(&mut self, position: usize, token: TokenKind) -> Result<usize, ()> {
		self.parse_token(position, token).map_err(|_| {
			self.errors.pop();
		})
	}

	fn parse_token(&mut self, position: usize, token: TokenKind) -> Result<usize, ()> {
		if self.tokens[position].kind == token {
			Ok(position + 1)
		} else {
			self.errors.push(SyntaxError::new(self.tokens[position].span(), "Invalid syntax"));
			Err(())
		}
	}

	fn try_parse_identifier(&mut self, mut position: usize) -> Result<(Str, usize), ()> {
		let token = self.tokens[position];
		if token.kind == TokenKind::Identifier {
			let token_start = usize::from(token.position);
			let token_end = token_start + usize::from(token.length);
			let name = self.source[token_start..token_end].into();
			Ok((name, position + 1))
		} else {
			Err(())
		}
	}

	fn parse_identifier(&mut self, position: usize) -> Result<(Str, usize), ()> {
		self.try_parse_identifier(position)
			.map_err(|_| self.errors.push(SyntaxError::new(self.tokens[position].span(), "Expected an identifier")))
	}

	fn parse_string(&mut self, position: usize) -> Str {
		let mut result = String::new();
		let (start, end) = self.tokens[position].span();
		let mut chars = self.source[start as usize..end as usize].chars().skip(1);
		while let Some(chr) = chars.next() {
			// TODO escapes
			result.push(chr);
		}
		result.pop();
		result.into()
	}

	fn skip_line(&mut self, mut position: usize) -> usize {
		while self.tokens[position].kind != TokenKind::Linend {
			if self.tokens[position].kind == TokenKind::Indent {
				position = self.skip_block(position + 1);
			} else {
				position += 1
			}
		}
		position + 1
	}

	fn skip_block(&mut self, mut position: usize) -> usize {
		use TokenKind::*;
		let mut indent = 1;
		while indent > 0 {
			match self.tokens[position].kind {
				Indent => indent += 1,
				Dedent => indent -= 1,
				_ => {}
			}
			position += 1;
		}
		position
	}
}

impl std::fmt::Debug for Expr {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{:?}", self.kind)
	}
}
