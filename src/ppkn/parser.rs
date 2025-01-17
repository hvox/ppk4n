use core::str;
use std::slice;

use super::ast::{self, BinOp, BinOpKind, Identifier, Typename};
use super::token::{Token, TokenKind};

type Block<'a> = ast::Block<'a, ()>;
type Expr<'a> = ast::Expr<'a, ()>;
type ExprKind<'a> = ast::ExprKind<'a, ()>;

pub fn parse(tokens: Vec<Token>) -> Result<Block, SyntaxError> {
	Parser::new(tokens).parse()
}

struct Parser<'a> {
	tokens: Vec<Token<'a>>,
	position: usize,
	last_error: SyntaxError<'a>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct SyntaxError<'a> {
	pub source: &'a str,
	pub message: &'static str,
}

impl<'a> SyntaxError<'a> {
	fn new(token: &Token<'a>, message: &'static str) -> Self {
		Self { source: token.source, message }
	}
}

impl<'a> Parser<'a> {
	fn new(tokens: Vec<Token<'a>>) -> Self {
		let error = SyntaxError::new(&tokens[0], "hopeless start");
		Self { tokens, position: 0, last_error: error }
	}

	fn parse(mut self) -> Result<Block<'a>, SyntaxError<'a>> {
		let module = self.block()?;
		if self.position < self.tokens.len() - 1 {
			return Err(self.last_error);
		}
		Ok(module)
	}

	fn block(&mut self) -> Result<Block<'a>, SyntaxError<'a>> {
		let mut source = &self.tokens[self.position].source[..0];
		let mut stmts = vec![];
		while let Ok(statement) = self.statement() {
			source = span(source, statement.source);
			let _ = self.expect(TokenKind::Semicolon);
			stmts.push(statement);
		}
		Ok(Block { source, stmts })
	}

	fn statement(&mut self) -> Result<Expr<'a>, SyntaxError<'a>> {
		use ast::ExprKind::*;
		let start_position = self.position;
		let start = self.tokens[self.position].source;
		if let Ok(expr) = self.print() {
			Ok(Expr::new(span(start, expr.source), Print(Box::new(expr))))
		} else if let Ok(expr) = self.println() {
			Ok(Expr::new(span(start, expr.source), Println(Box::new(expr))))
		} else if let Ok((variable, typ, value)) = self.definition() {
			Ok(Expr::new(span(start, value.source), Definition(variable, typ, value)))
		} else if let Ok((variable, value)) = self.assignment() {
			Ok(Expr::new(span(start, value.source), Assignment(variable, value)))
		} else if let Ok(if_statement) = self.if_statement() {
			Ok(if_statement)
		} else if let Ok(while_loop) = self.while_loop() {
			Ok(while_loop)
		} else if let Ok(return_statement) = self.return_statement() {
			Ok(return_statement)
		} else if let Ok(function) = self.function() {
			Ok(function)
		} else if let Ok(expr) = self.expression() {
			Ok(expr)
		} else {
			self.position = start_position;
			return Err(self.last_error);
		}
		// Class(Identifier<'a>, Vec<(Identifier<'a>, Typename<'a>)>),
		// Import(Identifier<'a>),
	}

	fn print(&mut self) -> Result<Expr<'a>, SyntaxError<'a>> {
		self.expect(TokenKind::Print)?;
		match self.expression() {
			Ok(expr) => Ok(expr),
			Err(error) => {
				self.position -= 1;
				Err(error)
			}
		}
	}

	fn println(&mut self) -> Result<Expr<'a>, SyntaxError<'a>> {
		self.expect(TokenKind::Println)?;
		match self.expression() {
			Ok(expr) => Ok(expr),
			Err(error) => {
				self.position -= 1;
				Err(error)
			}
		}
	}

	fn definition(
		&mut self,
	) -> Result<(Identifier<'a>, Typename<'a>, Box<Expr<'a>>), SyntaxError<'a>> {
		// println!("{:?}", &self.tokens[self.position..]);
		let start_position = self.position;
		let variable = self.expect_identifier()?;
		self.expect(TokenKind::Colon).map_err(|err| {
			self.position = start_position;
			err
		})?;
		let typename = self.expect_identifier().map_err(|err| {
			self.position = start_position;
			err
		})?;
		self.expect(TokenKind::Equal).map_err(|err| {
			self.position = start_position;
			err
		})?;
		let value = self.expression().map_err(|err| {
			self.position = start_position;
			err
		})?;
		Ok((variable.source, typename.source, Box::new(value)))
	}

	fn assignment(&mut self) -> Result<(Identifier<'a>, Box<Expr<'a>>), SyntaxError<'a>> {
		// println!("{:?}", &self.tokens[self.position..]);
		let start_position = self.position;
		let variable = self.expect_identifier()?;
		self.expect(TokenKind::Equal).map_err(|err| {
			self.position = start_position;
			err
		})?;
		let value = self.expression().map_err(|err| {
			self.position = start_position;
			err
		})?;
		Ok((variable.source, Box::new(value)))
	}

	fn if_statement(&mut self) -> Result<Expr<'a>, SyntaxError<'a>> {
		let start_position = self.position;
		let start = self.expect(TokenKind::If)?;
		let condition = self.expression().map_err(|err| {
			self.position = start_position;
			err
		})?;
		self.expect(TokenKind::LeftBrace).map_err(|err| {
			self.position = start_position;
			err
		})?;
		let block = self.block().map_err(|err| {
			self.position = start_position;
			err
		})?;
		let end = self.expect(TokenKind::RightBrace).map_err(|err| {
			self.position = start_position;
			err
		})?;
		Ok(Expr::new(
			span(start.source, end.source),
			ExprKind::If(Box::new(condition), block, None),
		))
	}

	fn while_loop(&mut self) -> Result<Expr<'a>, SyntaxError<'a>> {
		let start_position = self.position;
		let start = self.expect(TokenKind::While)?;
		let condition = self.expression().map_err(|err| {
			self.position = start_position;
			err
		})?;
		self.expect(TokenKind::LeftBrace).map_err(|err| {
			self.position = start_position;
			err
		})?;
		// println!("{:?}", &self.tokens[(self.position - 1)..(self.position + 2)]);
		let block = self.block().map_err(|err| {
			self.position = start_position;
			err
		})?;
		let end = self.expect(TokenKind::RightBrace).map_err(|err| {
			self.position = start_position;
			err
		})?;
		Ok(Expr::new(span(start.source, end.source), ExprKind::While(Box::new(condition), block)))
	}

	fn return_statement(&mut self) -> Result<Expr<'a>, SyntaxError<'a>> {
		let start_position = self.position;
		let start = self.expect(TokenKind::Return)?;
		let return_value = self.expression().map_err(|err| {
			self.position = start_position;
			err
		})?;
		Ok(Expr::new(
			span(start.source, return_value.source),
			ExprKind::Return(Box::new(return_value)),
		))
	}

	fn function(&mut self) -> Result<Expr<'a>, SyntaxError<'a>> {
		let start_position = self.position;
		let first_token = self.expect(TokenKind::Fun)?;
		let name = self.expect_identifier().map_err(|err| {
			self.position = start_position;
			err
		})?;
		self.expect(TokenKind::LeftParen).map_err(|err| {
			self.position = start_position;
			err
		})?;
		let args = self.function_arguments().map_err(|err| {
			self.position = start_position;
			err
		})?;
		self.expect(TokenKind::RightParen).map_err(|err| {
			self.position = start_position;
			err
		})?;
		self.expect(TokenKind::LeftBrace).map_err(|err| {
			self.position = start_position;
			err
		})?;
		let body = self.block().map_err(|err| {
			self.position = start_position;
			err
		})?;
		let closing_brace = self.expect(TokenKind::RightBrace).map_err(|err| {
			self.position = start_position;
			err
		})?;
		Ok(Expr::new(
			span(first_token.source, closing_brace.source),
			ExprKind::Function(name.source, args, body),
		))
	}

	fn function_arguments(
		&mut self,
	) -> Result<Vec<(Identifier<'a>, Typename<'a>)>, SyntaxError<'a>> {
		let start_position = self.position;
		let mut args = vec![];
		loop {
			let Ok(name) = self.expect_identifier().map_err(|err| {
				self.position = start_position;
				err
			}) else {
				break;
			};
			self.expect(TokenKind::Colon).map_err(|err| {
				self.position = start_position;
				err
			})?;
			let typ = self.expect_identifier().map_err(|err| {
				self.position = start_position;
				err
			})?;
			args.push((name.source, typ.source));
			if self.expect(TokenKind::Comma).is_err() {
				break;
			}
		}
		Ok(args)
	}

	fn expression(&mut self) -> Result<Expr<'a>, SyntaxError<'a>> {
		use TokenKind::*;
		let mut expr = self.arithmetic_expression()?;
		if let Ok(op) = self.expect_any(&[Less, LessEqual, EqualEqual, GreaterEqual, Greater]) {
			let binop_kind = match op.kind {
				EqualEqual => BinOpKind::EqualEqual,
				Greater => BinOpKind::Greater,
				GreaterEqual => BinOpKind::GreaterEqual,
				Less => BinOpKind::Less,
				LessEqual => BinOpKind::LessEqual,
				_ => unreachable!(),
			};
			let rhs = self.arithmetic_expression()?;
			expr = Expr::new(
				span(expr.source, rhs.source),
				ExprKind::Binary(
					Box::new(expr),
					BinOp { source: op.source, kind: binop_kind },
					Box::new(rhs),
				),
			)
		}
		Ok(expr)
	}

	fn arithmetic_expression(&mut self) -> Result<Expr<'a>, SyntaxError<'a>> {
		use TokenKind::*;
		let mut sum = self.term()?;
		while let Ok(op) = self.expect_any(&[Plus, Minus]) {
			let Ok(factor) = self.term() else {
				self.position -= 1;
				break;
			};
			let source = op.source;
			let op = match op.kind {
				Plus => BinOp { source, kind: BinOpKind::Plus },
				Minus => BinOp { source, kind: BinOpKind::Minus },
				_ => unreachable!(),
			};
			sum = Expr::new(
				span(sum.source, factor.source),
				ExprKind::Binary(Box::new(sum), op, Box::new(factor)),
			)
		}
		Ok(sum)
	}

	fn term(&mut self) -> Result<Expr<'a>, SyntaxError<'a>> {
		use TokenKind::*;
		let mut product = self.factor()?;
		while let Ok(op) = self.expect_any(&[Star, Slash]) {
			let Ok(factor) = self.factor() else {
				self.position -= 1;
				break;
			};
			let source = op.source;
			let op = match op.kind {
				Star => BinOp { source, kind: BinOpKind::Star },
				Slash => BinOp { source, kind: BinOpKind::Slash },
				_ => unreachable!(),
			};
			product = Expr::new(
				span(product.source, factor.source),
				ExprKind::Binary(Box::new(product), op, Box::new(factor)),
			)
		}
		Ok(product)
	}

	fn factor(&mut self) -> Result<Expr<'a>, SyntaxError<'a>> {
		if let Ok(call) = self.function_call() {
			return Ok(call);
		}
		return self.unary();
	}

	fn function_call(&mut self) -> Result<Expr<'a>, SyntaxError<'a>> {
		let start_position = self.position;
		let function = self.unary().map_err(|err| {
			self.position = start_position;
			err
		})?;
		self.expect(TokenKind::LeftParen).map_err(|err| {
			self.position = start_position;
			err
		})?;
		let mut args = vec![];
		while let Ok(arg) = self.expression() {
			args.push(arg);
			if self.expect(TokenKind::Comma).is_err() {
				break;
			}
		}
		let end = self.expect(TokenKind::RightParen).map_err(|err| {
			self.position = start_position;
			err
		})?;
		return Ok(Expr::new(
			span(self.tokens[start_position].source, end.source),
			ExprKind::FunctionCall(Box::new(function), args),
		));
	}

	fn unary(&mut self) -> Result<Expr<'a>, SyntaxError<'a>> {
		let start_position = self.position;
		if let Ok(_) = self.expect(TokenKind::LeftParen) {
			return Ok(self.expression().map_err(|err| {
				self.position = start_position;
				err
			})?);
		}
		use TokenKind::*;
		let kind = match &self.tokens[self.position].kind {
			Integer(number) => ExprKind::Integer(*number),
			Float(number) => ExprKind::Float(*number),
			String(literal) => ExprKind::String(literal.clone()),
			Identifier(variable) => ExprKind::Variable(variable.clone()),
			_ => self.error("Expected some kind of expression")?,
		};
		let source = self.tokens[self.position].source;
		self.position += 1;
		return Ok(Expr::new(source, kind));
	}

	fn expect_any(&mut self, expected_tokens: &[TokenKind]) -> Result<Token<'a>, SyntaxError<'a>> {
		let token = &self.tokens[self.position];
		if expected_tokens.contains(&token.kind) {
			self.position += 1;
			// TODO: i want partial borrowing in Rust!
			return Ok(token.clone());
		}
		self.error(Box::leak(Box::new(format!("Expected {:?}", expected_tokens))))
	}

	fn expect(&mut self, kind: TokenKind) -> Result<Token<'a>, SyntaxError<'a>> {
		let token = &self.tokens[self.position];
		if token.kind == kind {
			self.position += 1;
			// TODO: i want partial borrowing in Rust!
			return Ok(token.clone());
		}
		self.error(Box::leak(Box::new(format!("Expected {:?}", kind))))
	}

	fn expect_identifier(&mut self) -> Result<Token<'a>, SyntaxError<'a>> {
		let token = &self.tokens[self.position];
		match token.kind {
			TokenKind::Identifier(_) => {
				self.position += 1;
				// TODO: i want partial borrowing in Rust!
				Ok(token.clone())
			}
			_ => self.error("Expected identifier"),
		}
	}

	fn error<T>(&mut self, message: &'static str) -> Result<T, SyntaxError<'a>> {
		let error = SyntaxError::new(&self.tokens[self.position], message);
		if error.source.as_ptr() > self.last_error.source.as_ptr() {
			self.last_error = error;
		}
		Err(error)
	}
}

fn span<'a>(start: &'a str, end: &'a str) -> &'a str {
	let end = end.as_ptr() as usize + end.len();
	let length = end - start.as_ptr() as usize;
	unsafe {
		let bytes = slice::from_raw_parts(start.as_ptr(), length);
		str::from_utf8_unchecked(bytes)
	}
}

#[cfg(test)]
mod tests {
	use crate::ppkn::tokenizer::tokenize;

	use super::*;

	fn show_tokens(source: &str) {
		let tokens: Vec<_> = tokenize(source).into_iter().map(|x| x.kind).collect();
		println!("{:?}", tokens);
	}

	#[test]
	fn whitespaces() {
		let source = "# notheratonhe ahoe ";
		let ast = Block { source: "", stmts: vec![] };
		assert_eq!(parse(tokenize(source)), Ok(ast));
	}

	#[test]
	fn function() {
		let source = "fun hello() { print 'Hello world'; }";
		let ast = "fun name() { print(string); }; ";
		let result = parse(tokenize(source)).map(|ast| ast.to_string());
		assert_eq!(result, Ok(ast.to_string()));
	}

	#[test]
	fn arithmetic() {
		let source = "x + 123 * y / bulbalka";
		let ast = "(variable + ((number * variable) / variable)); ";
		let result = parse(tokenize(source)).map(|ast| ast.to_string());
		assert_eq!(result, Ok(ast.to_string()));
	}

	#[test]
	fn types() {
		let source = "x: i32 = bulbalka";
		let ast = "variable: type = variable; ";
		let result = parse(tokenize(source)).map(|ast| ast.to_string());
		assert_eq!(result, Ok(ast.to_string()));
	}
}
