#![allow(unused)]
use std::cell::Cell;
use std::collections::HashMap;
use std::ops::Index;
use std::ops::IndexMut;
use std::rc::Rc;
use std::result;

use indexmap::IndexMap;

use super::ast::*;
use super::hir::{self, FnSignature, Function, Str, Type};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TypeError {
	// TODO: add field
	// pub location: (u32, u32),
	pub message: Box<str>,
}

pub fn typecheck(program: &Block<()>) -> Result<hir::Program, TypeError> {
	Ok(Typechecker::new().typecheck(program))
}

#[derive(Debug, PartialEq)]
struct Typechecker {
	pub fns: IndexMap<Str, hir::Function>,
	types: Types,
	restype: TypeId,
	// Maybe change it to indexmap?
	// Like one continues indexmap instead of a bunch of vectors
	scope: Vec<HashMap<Str, TypeId>>,
}

#[derive(Debug, PartialEq, Eq)]
struct Types {
	// Types are stored in Disjoint Set Union data structure
	// and derived types are stored only for leaders of sets.
	leaders: Vec<Cell<u32>>,
	// P.S. Will it be better to put indexmap here?
	types: HashMap<u32, UncheckedType>,
	unit: TypeId,
	bool: TypeId,
	i32: TypeId,
	u32: TypeId,
	f32: TypeId,
	str: TypeId,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum UncheckedType {
	Unknown,
	Integer,
	Float,
	Unit,
	Bool,
	I32,
	U32,
	F32,
	Str,
	Array(TypeId),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
struct TypeId(u32);

impl Typechecker {
	fn new() -> Self {
		Self { fns: IndexMap::new(), types: Types::new(), restype: TypeId(0), scope: vec![] }
	}

	fn typecheck(mut self, ast: &Block<()>) -> hir::Program {
		const TYPECHECKED_FUNCTION_BODY_PLACEHOLDER: hir::Expr<Type> =
			hir::Expr { kind: hir::ExprKind::Unreachable, typ: Type::Unit };
		let mut bodies = Vec::new();
		for Expr { source, kind, annotations } in &ast.stmts {
			match kind {
				ExprKind::Function(name, parameters, body) => {
					// TODO: Where function result type?
					let name = Str::from(*name);
					let mut params = vec![];
					let mut result = Type::Unit;
					let mut param_names = vec![];
					for (param, typename) in parameters {
						param_names.push(Str::from(*param));
						params.push(self.parse_type(typename));
					}
					let signature = FnSignature { params, result };
					bodies.push(body);
					self.fns.insert(
						name.clone(),
						Function {
							name,
							signature,
							parameters: param_names,
							body: TYPECHECKED_FUNCTION_BODY_PLACEHOLDER,
						},
					);
				}
				// TODO: return error
				_ => panic!("Expected function"),
			}
		}

		self.scope.push(HashMap::new());
		for (fn_idx, body) in bodies.into_iter().enumerate() {
			self.types.clear();
			self.restype = self.types.add_exact(&self.fns[fn_idx].signature.result);
			self.scope.truncate(1);
			self.scope[0].clear();
			for (param, typ) in self.fns[fn_idx].parameters.iter().zip(&self.fns[fn_idx].signature.params) {
				let type_id = self.types.add_exact(typ);
				self.scope[0].insert(param.clone(), type_id);
			}
			// TODO: Allow AST to have arbitary expressions as function bodies
			let function_body =
				hir::Expr { kind: hir::ExprKind::Block(self.typecheck_block(body, self.restype)), typ: self.restype };
			self.fns[fn_idx].body = function_body.map(&mut |type_id| self.resolve_type(type_id));
		}

		hir::Program { fns: self.fns }
	}

	fn typecheck_block(&mut self, block: &Block<()>, expected_type: TypeId) -> hir::Block<TypeId> {
		self.scope.push(HashMap::new());
		let final_operands;
		if block.stmts.is_empty() {
			final_operands = 0;
			self.types.unify(expected_type, self.types.unit);
		} else {
			final_operands = if self.types[expected_type] != UncheckedType::Unit { 1 } else { 0 };
		};
		let stmts: Vec<_> =
			block.stmts[0..block.stmts.len() - final_operands].iter().map(|stmt| self.typecheck_stmt(stmt)).collect();
		let result = if final_operands > 0 {
			let expr = block.stmts.last().unwrap();
			Some(self.typecheck_expr(expr, expected_type).into())
		} else {
			None
		};
		self.scope.pop();
		hir::Block { stmts, result }
	}

	fn typecheck_stmt(&mut self, stmt: &Expr<()>) -> hir::Stmt<TypeId> {
		match &stmt.kind {
			ExprKind::Definition(variable, typename, value) => {
				let name = Str::from(*variable);
				let typ = if typename.is_empty() {
					self.types.add(UncheckedType::Unknown)
				} else {
					self.types.add_exact(&self.parse_type(typename))
				};
				let scope = self.scope.last_mut().unwrap();
				scope.insert(name.clone(), typ);
				hir::Stmt::Def(name, self.typecheck_expr(value, typ))
			}
			ExprKind::If(_, _, _) | ExprKind::Indented(_) => {
				hir::Stmt::Expr(self.typecheck_expr(stmt, self.types.unit))
			}
			_ => {
				let expected_type = self.types.add(UncheckedType::Unknown);
				hir::Stmt::Expr(self.typecheck_expr(stmt, expected_type))
			}
		}
	}

	fn typecheck_expr(&mut self, expr: &Expr<()>, expected_type: TypeId) -> hir::Expr<TypeId> {
		// Different actions depending on expected_type
		// IF expected_type == Unit, then add pass to the end of blocks and ifs
		// otherwise unify with last expression in the block or if
		let kind = match &expr.kind {
			ExprKind::Print(expr) => {
				self.types.unify(expected_type, self.types.unit).expect("Type Error");
				let mut args = vec![];
				let expected_type = self.types.add(UncheckedType::Unknown);
				args.push(self.typecheck_expr(expr, expected_type));
				hir::ExprKind::FnCall(Str::from("print"), args)
			}
			ExprKind::Println(expr) => {
				self.types.unify(expected_type, self.types.unit).expect("Type Error");
				let mut args = vec![];
				let expected_type = self.types.add(UncheckedType::Unknown);
				args.push(self.typecheck_expr(expr, expected_type));
				hir::ExprKind::FnCall(Str::from("println"), args)
			}
			ExprKind::Definition(varname, typename, expr) => {
				panic!("Variable declaration is not allowed here");
				// self.types.unify(expected_type, self.types.unit).expect("Type Error");
				// let name = Str::from(*varname);
				// let typ = if typename.is_empty() {
				// 	self.types.add(UncheckedType::Unknown)
				// } else {
				// 	self.types.add_exact(&self.parse_type(typename))
				// };
				// if self.scope.is_empty() {
				// 	self.scope.push(HashMap::new());
				// }
				// let scope = self.scope.last_mut().unwrap();
				// scope.insert(name.clone(), typ);
				// hir::ExprKind::DefLocal(name, Box::new(self.typecheck_expr(expr, typ)))
			}
			ExprKind::Assignment(varname, expr) => {
				self.types.unify(expected_type, self.types.unit).expect("Type Error");
				let name = Str::from(*varname);
				let typ = self.local(&name);
				hir::ExprKind::SetLocal(name, Box::new(self.typecheck_expr(expr, typ)))
			}
			ExprKind::If(condition, then, otherwise) => {
				let condition = self.typecheck_expr(condition, self.types.bool);
				let then = then.clone().into_expr();
				if self.types[expected_type] == UncheckedType::Unit {
					let typ = self.types.add(UncheckedType::Unknown);
					let then = self.typecheck_expr(&then, typ);
					let then = self.nullify(then);
					let otherwise = if let Some(otherwise) = otherwise {
						let typ = self.types.add(UncheckedType::Unknown);
						let otherwise = self.typecheck_expr(&otherwise.clone().into_expr(), typ);
						Some(Box::new(self.nullify(otherwise)))
					} else {
						None
					};
					hir::ExprKind::If(Box::new(condition), Box::new(then), otherwise)
				} else {
					let then = self.typecheck_expr(&then, expected_type);
					let otherwise = if let Some(otherwise) = otherwise {
						Some(Box::new(self.typecheck_expr(&otherwise.clone().into_expr(), expected_type)))
					} else {
						self.types.unify(expected_type, self.types.unit);
						None
					};
					hir::ExprKind::If(Box::new(condition), Box::new(then), otherwise)
				}
			}
			ExprKind::While(condition, body) => {
				self.types.unify(expected_type, self.types.unit).expect("Type Error");
				let condition = self.typecheck_expr(condition, self.types.bool);
				let body = self.typecheck_expr(&body.clone().into_expr(), self.types.unit);
				hir::ExprKind::While(Box::new(condition), Box::new(body))
			}
			ExprKind::Return(result) => {
				let result = self.typecheck_expr(result, self.restype);
				hir::ExprKind::Return(Box::new(result))
			}
			ExprKind::Indented(block) => {
				let block = self.typecheck_block(block, expected_type);
				hir::ExprKind::Block(block)
			}
			ExprKind::Function(_, _, _) | ExprKind::Class(_, _) | ExprKind::Import(_) => {
				panic!("Declarations can't be expressions")
			}
			ExprKind::Integer(number) => {
				let integer_type = self.types.add(UncheckedType::Integer);
				self.types.unify(expected_type, integer_type);
				hir::ExprKind::Value(hir::Literal::Number(*number))
			}
			ExprKind::Float(number) => {
				let float_type = self.types.add(UncheckedType::Float);
				self.types.unify(expected_type, float_type);
				hir::ExprKind::Value(hir::Literal::Number(number.to_bits()))
			}
			ExprKind::String(string) => {
				let string = string.clone().into();
				self.types.unify(expected_type, self.types.str);
				hir::ExprKind::Value(hir::Literal::String(string))
			}
			ExprKind::Variable(_) => {
				let name = Str::from(expr.source);
				let actual_type = self.local(&name);
				self.types.unify(expected_type, actual_type).expect("Type Error");
				hir::ExprKind::GetLocal(name)
			}
			ExprKind::Grouping(expr) => self.typecheck_expr(expr, expected_type).kind,
			ExprKind::Unary(op, expr) => match op.kind {
				UnaryOpKind::Minus => {
					let expr = self.typecheck_expr(expr, expected_type);
					hir::ExprKind::MethodCall(expr.into(), "neg".into(), vec![])
				}
				UnaryOpKind::Bang => {
					self.types.unify(expected_type, self.types.bool);
					let expr = self.typecheck_expr(expr, expected_type);
					hir::ExprKind::MethodCall(expr.into(), "not".into(), vec![])
				}
			},
			ExprKind::Binary(lhs, op, rhs) => match op.kind {
				BinOpKind::Minus => {
					let lhs = self.typecheck_expr(lhs, expected_type);
					let rhs = self.typecheck_expr(rhs, expected_type);
					hir::ExprKind::MethodCall(lhs.into(), "sub".into(), vec![rhs])
				}
				BinOpKind::Plus => {
					let lhs = self.typecheck_expr(lhs, expected_type);
					let rhs = self.typecheck_expr(rhs, expected_type);
					hir::ExprKind::MethodCall(lhs.into(), "add".into(), vec![rhs])
				}
				BinOpKind::Slash => {
					let lhs = self.typecheck_expr(lhs, expected_type);
					let rhs = self.typecheck_expr(rhs, expected_type);
					hir::ExprKind::MethodCall(lhs.into(), "div".into(), vec![rhs])
				}
				BinOpKind::Star => {
					let lhs = self.typecheck_expr(lhs, expected_type);
					let rhs = self.typecheck_expr(rhs, expected_type);
					hir::ExprKind::MethodCall(lhs.into(), "mul".into(), vec![rhs])
				}
				BinOpKind::Greater => {
					self.types.unify(expected_type, self.types.bool);
					let expected_type = self.types.add(UncheckedType::Unknown);
					let lhs = self.typecheck_expr(lhs, expected_type);
					let rhs = self.typecheck_expr(rhs, expected_type);
					hir::ExprKind::MethodCall(lhs.into(), "gt".into(), vec![rhs])
				}
				BinOpKind::GreaterEqual => {
					self.types.unify(expected_type, self.types.bool);
					let expected_type = self.types.add(UncheckedType::Unknown);
					let lhs = self.typecheck_expr(lhs, expected_type);
					let rhs = self.typecheck_expr(rhs, expected_type);
					hir::ExprKind::MethodCall(lhs.into(), "ge".into(), vec![rhs])
				}
				BinOpKind::Less => {
					self.types.unify(expected_type, self.types.bool);
					let expected_type = self.types.add(UncheckedType::Unknown);
					let lhs = self.typecheck_expr(lhs, expected_type);
					let rhs = self.typecheck_expr(rhs, expected_type);
					hir::ExprKind::MethodCall(lhs.into(), "lt".into(), vec![rhs])
				}
				BinOpKind::LessEqual => {
					self.types.unify(expected_type, self.types.bool);
					let expected_type = self.types.add(UncheckedType::Unknown);
					let lhs = self.typecheck_expr(lhs, expected_type);
					let rhs = self.typecheck_expr(rhs, expected_type);
					hir::ExprKind::MethodCall(lhs.into(), "le".into(), vec![rhs])
				}
				BinOpKind::BangEqual => {
					self.types.unify(expected_type, self.types.bool);
					let expected_type = self.types.add(UncheckedType::Unknown);
					let lhs = self.typecheck_expr(lhs, expected_type);
					let rhs = self.typecheck_expr(rhs, expected_type);
					hir::ExprKind::MethodCall(
						hir::Expr {
							kind: hir::ExprKind::MethodCall(lhs.into(), "eq".into(), vec![rhs]),
							typ: self.types.bool,
						}
						.into(),
						"not".into(),
						vec![],
					)
				}
				BinOpKind::EqualEqual => {
					self.types.unify(expected_type, self.types.bool);
					let expected_type = self.types.add(UncheckedType::Unknown);
					let lhs = self.typecheck_expr(lhs, expected_type);
					let rhs = self.typecheck_expr(rhs, expected_type);
					hir::ExprKind::MethodCall(lhs.into(), "eq".into(), vec![rhs])
				}
			},
			ExprKind::FunctionCall(fname, arguments) => {
				let fname = Str::from(*fname);
				let signature = &self.fns[&fname].signature;
				let actual_type = self.types.add_exact(&signature.result);
				self.types.unify(expected_type, actual_type);
				let mut args = vec![];
				for (arg, typ) in arguments.iter().zip(signature.params.clone()) {
					let typ = self.types.add_exact(&typ);
					args.push(self.typecheck_expr(arg, typ));
				}
				hir::ExprKind::FnCall(fname, args)
			}
			// Store std types separately or inject them into program.types?
			ExprKind::MethodCall(object, method_name, arguments) => {
				let object_type = self.types.add(UncheckedType::Unknown);
				let object = self.typecheck_expr(object, object_type);
				let (params, result) = self.resolve_method(object_type, method_name);
				let mut typechecked_args = vec![];
				for (argument, param_type) in arguments.iter().zip(params) {
					typechecked_args.push(self.typecheck_expr(argument, param_type));
				}
				self.types.unify(expected_type, result);
				hir::ExprKind::MethodCall(object.into(), Str::from(*method_name), typechecked_args)
			}
		};
		hir::Expr { kind, typ: expected_type }
	}

	fn resolve_method(&mut self, self_type: TypeId, method_name: &str) -> (Vec<TypeId>, TypeId) {
		// We cound have returned &FnSignature if Typechecker was splitted into two
		// separate structs: ProgramTypechecker and FunctionTypechecker, then
		// FunctionTypechecker would borrow ProgramTypechecker immutably and this
		// FnSignature could also be borrowed at the same time.
		todo!()
	}

	fn resolve_type(&self, type_id: TypeId) -> Type {
		match self.types[type_id] {
			UncheckedType::Unknown => Type::Unit,
			UncheckedType::Integer => Type::I32,
			UncheckedType::Float => Type::F32,
			UncheckedType::Unit => Type::Unit,
			UncheckedType::Bool => Type::Bool,
			UncheckedType::I32 => Type::I32,
			UncheckedType::U32 => Type::U32,
			UncheckedType::F32 => Type::F32,
			UncheckedType::Str => Type::Str,
			UncheckedType::Array(typ) => Type::Array(self.resolve_type(typ).into()),
		}
	}

	fn local(&self, variable: &str) -> TypeId {
		for scope in self.scope.iter().rev() {
			let Some(&type_id) = scope.get(variable) else { continue };
			return type_id;
		}
		panic!("Name error: variable not defined")
	}

	fn nullify(&self, mut expr: hir::Expr<TypeId>) -> hir::Expr<TypeId> {
		if self.types[expr.typ] == UncheckedType::Unit {
			return expr;
		}
		// let mut stmts = match expr.kind {
		// 	hir::ExprKind::Block(mut stmts) => stmts,
		// 	_ => vec![expr],
		// };
		todo!()
		// stmts.push(hir::Expr { kind: hir::ExprKind::Unit, typ: self.types.unit });
		// hir::Expr { kind: hir::ExprKind::Block(stmts), typ: self.types.unit }
	}

	fn parse_type(&self, typename: &str) -> Type {
		match typename {
			"()" => Type::Unit,
			"bool" => Type::Bool,
			"i32" => Type::I32,
			"u32" => Type::U32,
			"f32" => Type::F32,
			"int" => Type::I32,
			"uint" => Type::U32,
			"float" => Type::F32,
			"str" => Type::Str,
			array if array.starts_with("[") && array.ends_with("]") => {
				let typ = self.parse_type(&array[1..array.len() - 1]);
				Type::Array(Rc::new(typ))
			}
			// TODO: Return TypeError instead of panic
			_ => panic!("wrong type"),
		}
	}
}

impl Types {
	fn new() -> Self {
		let mut types = Self {
			leaders: vec![],
			types: HashMap::new(),
			unit: TypeId(0),
			bool: TypeId(1),
			i32: TypeId(2),
			u32: TypeId(3),
			f32: TypeId(4),
			str: TypeId(5),
		};
		types.clear();
		types
	}

	fn clear(&mut self) {
		self.leaders.clear();
		self.leaders.extend((0..=self.str.0).map(Cell::new));
		// If field `self.types` was IndexMap,
		// then `self.clear()` would have been
		// simple truncate on the field.
		// Right?
		// Or keys get invalidated when `self.leaders` is reset?
		// Is there workaround?
		self.types.clear();
		self.types.insert(self.unit.0, UncheckedType::Unit);
		self.types.insert(self.bool.0, UncheckedType::Bool);
		self.types.insert(self.i32.0, UncheckedType::I32);
		self.types.insert(self.u32.0, UncheckedType::U32);
		self.types.insert(self.f32.0, UncheckedType::F32);
		self.types.insert(self.str.0, UncheckedType::Str);
	}

	fn add(&mut self, typ: UncheckedType) -> TypeId {
		let id = self.leaders.len() as u32;
		self.leaders.push(Cell::new(id));
		self.types.insert(id, typ);
		TypeId(id)
	}

	fn add_exact(&mut self, typ: &Type) -> TypeId {
		let typ = match typ {
			Type::Unit => return self.unit,
			Type::Bool => return self.bool,
			Type::I32 => return self.i32,
			Type::U32 => return self.u32,
			Type::F32 => return self.f32,
			Type::Str => return self.str,
			Type::Array(typ) => UncheckedType::Array(self.add_exact(typ)),
		};
		self.add(typ)
	}

	fn unify(&mut self, u: TypeId, v: TypeId) -> Result<(), (UncheckedType, UncheckedType)> {
		let u = self.get_leader(u);
		let v = self.get_leader(v);
		if u == v {
			return Ok(());
		}

		let u_type = self.types[&u];
		let v_type = self.types[&v];
		use UncheckedType::*;
		let unified_type = match (u_type, v_type) {
			(Unknown, Unknown) => Unknown,
			(Unknown, x) => x,
			(x, Unknown) => x,
			(x, also_x) if also_x == x => x,
			_ => {
				println!("{:?} != {:?}", u_type, v_type);
				todo!()
			}
		};
		self.types.remove(&u);
		self.types.remove(&v);
		self.leaders[v as usize].set(u);
		self.types.insert(u, unified_type);
		Ok(())
	}

	fn get_leader(&self, id: TypeId) -> u32 {
		let id = id.0 as usize;
		let mut leader = self.leaders[id].get();
		if self.leaders[leader as usize].get() != leader {
			leader = self.get_leader(TypeId(leader)) as u32;
			self.leaders[id].set(leader);
		}
		leader
	}
}

impl Index<TypeId> for Types {
	type Output = UncheckedType;

	fn index(&self, id: TypeId) -> &Self::Output {
		&self.types[&(self.get_leader(id) as u32)]
	}
}

impl IndexMut<TypeId> for Types {
	fn index_mut(&mut self, id: TypeId) -> &mut Self::Output {
		self.types.get_mut(&(self.get_leader(id) as u32)).unwrap()
	}
}
