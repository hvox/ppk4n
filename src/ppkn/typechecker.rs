use indexmap::IndexMap;

use super::ast::{self, BinOpKind};
use super::ir::*;

type Block<'a> = ast::Block<'a, ()>;
type Expr<'a> = ast::Expr<'a, ()>;
type ExprKind<'a> = ast::ExprKind<'a, ()>;

pub fn typecheck(program: Block) -> Result<Program, TypeError> {
	Typechecker::new(program).typecheck()
}

struct Typechecker<'a> {
	ast: Block<'a>,
	functions: IndexMap<String, Function<'a>>,
	phantom: std::marker::PhantomData<&'a str>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct TypeError<'a> {
	pub location: &'a str,
	pub message: &'static str,
}

impl<'a> Typechecker<'a> {
	fn new(ast: Block<'a>) -> Self {
		Self { ast, functions: IndexMap::new(), phantom: std::marker::PhantomData }
	}

	fn typecheck(mut self) -> Result<Program<'a>, TypeError<'a>> {
		for stmt in &self.ast.stmts {
			match &stmt.kind {
				ExprKind::Function(name, parameters, _) => {
					let mut params = IndexMap::new();
					for (name, typ) in parameters.iter() {
						params.insert(name.to_string(), self.get_type_from_name(typ)?);
					}
					let f = Function {
						params,
						result: Type::Void,
						locals: IndexMap::new(),
						code: vec![],
					};
					self.functions.insert(name.to_string(), f);
				}
				_ => unreachable!(),
			}
		}
		for stmt in &self.ast.stmts {
			match &stmt.kind {
				ExprKind::Function(name, _, body) => {
					let func_id = self.functions.get_index_of(*name).unwrap();
					let mut locals = self.functions[func_id].params.clone();
					let mut instrs = vec![];
					for stmt in &body.stmts {
						instrs.extend(self.typecheck_stmt(&mut locals, stmt)?);
					}
					self.functions[func_id].locals = locals;
					self.functions[func_id].code = instrs;
				}
				_ => unreachable!(),
			}
		}
		Ok(Program { functions: self.functions, phantom: std::marker::PhantomData })
	}

	fn get_type_from_name(&self, typename: &'a str) -> Result<Type, TypeError<'a>> {
		match typename {
			"int" => Ok(Type::I64),
			"float" => Ok(Type::F64),
			"str" => Ok(Type::Str),
			"none" => Ok(Type::Void),
			_ => Err(TypeError { location: typename, message: "Unknown type" }),
		}
	}

	fn typecheck_stmt(
		&self,
		scope: &mut IndexMap<String, Type>,
		stmt: &Expr<'a>,
	) -> Result<Vec<Instr<'a>>, TypeError<'a>> {
		let mut instrs = vec![];
		match &stmt.kind {
			ExprKind::Definition(name, typ, expr) => {
				let var_id = scope.len();
				scope.insert(name.to_string(), self.get_type_from_name(typ)?);
				let op = self.typecheck_expr(scope, expr)?;
				instrs.push(Instr {
					source: stmt.source,
					kind: InstrKind::Definition(var_id, Box::new(op)),
				});
			}
			ExprKind::Print(expr) => {
				instrs.push(Instr {
					source: stmt.source,
					kind: InstrKind::Print(Box::new(self.typecheck_expr(scope, expr)?)),
				});
			}
			ExprKind::Println(expr) => {
				instrs.push(Instr {
					source: stmt.source,
					kind: InstrKind::Print(Box::new(self.typecheck_expr(scope, expr)?)),
				});
				instrs.push(Instr {
					source: stmt.source,
					kind: InstrKind::Print(Box::new(Instr {
						source: stmt.source,
						kind: InstrKind::String("\n".to_string()),
					})),
				});
			}
			ExprKind::Assignment(name, expr) => {
				let Some(var_id) = scope.get_index_of(*name) else {
					return Err(TypeError {
						location: name,
						message: "Variable is not defined in this scope",
					});
				};
				let op = self.typecheck_expr(scope, expr)?;
				instrs.push(Instr {
					source: stmt.source,
					kind: InstrKind::Assignment(var_id, Box::new(op)),
				});
			}
			ExprKind::If(expr, block, block1) => todo!(),
			ExprKind::While(expr, block) => {
				let mut block_instrs = vec![];
				for stmt in &block.stmts {
					block_instrs.extend(self.typecheck_stmt(scope, stmt)?);
				}
				instrs.push(Instr {
					source: stmt.source,
					kind: InstrKind::While(
						Box::new(self.typecheck_expr(scope, expr)?),
						block_instrs,
					),
				})
			}
			ExprKind::Return(expr) => instrs.push(Instr {
				source: stmt.source,
				kind: InstrKind::Return(Box::new(self.typecheck_expr(scope, expr)?)),
			}),
			ExprKind::Function(_, vec, block) => todo!(),
			ExprKind::Class(_, vec) => todo!(),
			ExprKind::Import(_) => todo!(),
			_ => {
				instrs.push(self.typecheck_expr(scope, stmt)?);
			}
		}
		Ok(instrs)
	}

	fn typecheck_expr(
		&self,
		scope: &IndexMap<String, Type>,
		expr: &Expr<'a>,
	) -> Result<Instr<'a>, TypeError<'a>> {
		let instr = match &expr.kind {
			ExprKind::Integer(number) => InstrKind::Integer(*number as i64),
			ExprKind::Float(number) => InstrKind::Float(*number),
			ExprKind::String(string) => InstrKind::String(string.to_string()),
			ExprKind::Variable(name) => InstrKind::Variable(scope.get_index_of(&name[..]).unwrap()),
			ExprKind::Grouping(expr) => todo!(),
			ExprKind::Unary(unary_op, expr) => todo!(),
			ExprKind::Binary(lhs, bin_op, rhs) => match bin_op.kind {
				BinOpKind::Minus => InstrKind::SubI64(
					Box::new(self.typecheck_expr(scope, lhs)?),
					Box::new(self.typecheck_expr(scope, rhs)?),
				),
				BinOpKind::Plus => InstrKind::AddI64(
					Box::new(self.typecheck_expr(scope, lhs)?),
					Box::new(self.typecheck_expr(scope, rhs)?),
				),
				BinOpKind::Slash => InstrKind::DivI64(
					Box::new(self.typecheck_expr(scope, lhs)?),
					Box::new(self.typecheck_expr(scope, rhs)?),
				),
				BinOpKind::Star => InstrKind::MulI64(
					Box::new(self.typecheck_expr(scope, lhs)?),
					Box::new(self.typecheck_expr(scope, rhs)?),
				),
				BinOpKind::Greater => todo!(),
				BinOpKind::GreaterEqual => todo!(),
				BinOpKind::Less => InstrKind::LessI64(
					Box::new(self.typecheck_expr(scope, lhs)?),
					Box::new(self.typecheck_expr(scope, rhs)?),
				),
				BinOpKind::LessEqual => todo!(),
				BinOpKind::BangEqual => todo!(),
				BinOpKind::EqualEqual => todo!(),
			},
			ExprKind::FunctionCall(fname, arguments) => {
				let mut args = vec![];
				for arg in arguments {
					args.push(self.typecheck_expr(scope, arg)?);
				}
				InstrKind::Call(self.functions.get_index_of(*fname).unwrap(), args)
			}
			_ => unreachable!(),
		};
		Ok(Instr { source: expr.source, kind: instr })
	}
}
