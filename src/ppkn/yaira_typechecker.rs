#![allow(unused)]
use std::cell::Cell;
use std::collections::HashMap;
use std::ops::Index;
use std::ops::IndexMut;
use std::rc::Rc;

use indexmap::IndexMap;

use super::ast::*;
use super::yaira::*;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TypeError {
	// TODO: add field
	// pub location: (u32, u32),
	pub message: Box<str>,
}

pub fn typecheck(program: &Block<()>) -> Result<Program, TypeError> {
	Ok(Typechecker::new().typecheck(program))
}

#[derive(Debug, PartialEq)]
struct Typechecker {
	pub fns: IndexMap<Str, Function>,
	types: Types,
	restype: TypeId,
	scope: Vec<HashMap<Str, TypeId>>,
}

#[derive(Debug, PartialEq, Eq)]
struct Types {
	// Types are stored in Disjoint Set Union data structure
	// and derived types are stored only for leaders of sets.
	leaders: Vec<Cell<u32>>,
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

	fn typecheck(mut self, ast: &Block<()>) -> Program {
		const TYPECHECKED_FUNCTION_BODY_PLACEHOLDER: Code<Type> = Code { kind: CodeKind::Unreachable, typ: Type::Unit };
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
				_ => todo!("return error"),
			}
		}

		for (fn_idx, body) in bodies.into_iter().enumerate() {
			self.types.clear();
			self.restype = self.types.add_exact(&self.fns[fn_idx].signature.result);
			// TODO: Allow AST to have arbitary expressions as function bodies
			let body = Expr { source: body.source, kind: ExprKind::Indented(body.clone()), annotations: () };
			let code = self.typecheck_expr(&body, self.restype);
			self.fns[fn_idx].body = code.map(&mut |type_id| self.resolve_type(type_id));
		}

		Program { fns: self.fns }
	}

	fn typecheck_expr(&mut self, expr: &Expr<()>, expected_type: TypeId) -> Code<TypeId> {
		// Different actions depending on expected_type
		// IF expected_type == Unit, then add pass to the end of blocks and ifs
		// otherwise unify with last expression in the block or if
		let kind = match &expr.kind {
			ExprKind::Print(expr) => {
				self.types.unify(expected_type, self.types.unit).expect("Type Error");
				CodeKind::FnCall(Str::from("print"), vec![self.typecheck_expr(expr, self.types.str)])
			}
			ExprKind::Println(expr) => {
				self.types.unify(expected_type, self.types.unit).expect("Type Error");
				CodeKind::FnCall(Str::from("println"), vec![self.typecheck_expr(expr, self.types.str)])
			}
			ExprKind::Definition(varname, typename, expr) => {
				self.types.unify(expected_type, self.types.unit).expect("Type Error");
				let name = Str::from(*varname);
				let typ = if typename.is_empty() {
					self.types.add(UncheckedType::Unknown)
				} else {
					self.types.add_exact(&self.parse_type(typename))
				};
				if self.scope.is_empty() {
					self.scope.push(HashMap::new());
				}
				let scope = self.scope.last_mut().unwrap();
				scope.insert(name.clone(), typ);
				CodeKind::DefLocal(name, Box::new(self.typecheck_expr(expr, typ)))
			}
			ExprKind::Assignment(varname, expr) => {
				self.types.unify(expected_type, self.types.unit).expect("Type Error");
				let name = Str::from(*varname);
				let typ = self.local(&name);
				CodeKind::SetLocal(name, Box::new(self.typecheck_expr(expr, typ)))
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
					CodeKind::If(Box::new(condition), Box::new(then), otherwise)
				} else {
					let then = self.typecheck_expr(&then, expected_type);
					let otherwise = if let Some(otherwise) = otherwise {
						Some(Box::new(self.typecheck_expr(&otherwise.clone().into_expr(), expected_type)))
					} else {
						self.types.unify(expected_type, self.types.unit);
						None
					};
					CodeKind::If(Box::new(condition), Box::new(then), otherwise)
				}
			}
			ExprKind::While(condition, body) => {
				self.types.unify(expected_type, self.types.unit).expect("Type Error");
				let condition = self.typecheck_expr(condition, self.types.bool);
				let body = self.typecheck_expr(&body.clone().into_expr(), self.types.unit);
				CodeKind::While(Box::new(condition), Box::new(body))
			}
			ExprKind::Return(result) => {
				let result = self.typecheck_expr(result, self.restype);
				CodeKind::Return(Box::new(result))
			}
			ExprKind::Indented(block) => {
				let stmts = &block.stmts;
				if stmts.is_empty() {
					self.types.unify(expected_type, self.types.unit).expect("Type Error");
					CodeKind::Block(vec![])
				} else {
					let mut typechecked_stmts = Vec::with_capacity(stmts.len() + 1);
					self.scope.push(HashMap::new());
					for stmt in &stmts[..stmts.len() - 1] {
						// TODO: This type should be recommendation, not restriction
						typechecked_stmts.push(self.typecheck_expr(stmt, self.types.unit));
					}
					let last_stmt = stmts.last().unwrap();
					if self.types[expected_type] == UncheckedType::Unit {
						let typ = self.types.add(UncheckedType::Unknown);
						typechecked_stmts.push(self.typecheck_expr(last_stmt, typ));
					}
					self.scope.pop();
					todo!()
				}
			}
			// TODO: all the other things here
			ExprKind::Function(_, items, block) => todo!(),
			ExprKind::Class(_, items) => todo!(),
			ExprKind::Import(_) => todo!(),
			ExprKind::Integer(_) => todo!(),
			ExprKind::Float(_) => todo!(),
			ExprKind::String(_) => todo!(),
			ExprKind::Variable(_) => todo!(),
			ExprKind::Grouping(expr) => todo!(),
			ExprKind::Unary(unary_op, expr) => todo!(),
			ExprKind::Binary(expr, bin_op, expr1) => todo!(),
			ExprKind::FunctionCall(_, exprs) => todo!(),
			ExprKind::MethodCall(_, _, exprs) => todo!(),
		};
		Code { kind, typ: expected_type }
	}

	fn resolve_type(&self, type_id: TypeId) -> Type {
		match self.types[type_id] {
			UncheckedType::Unknown => Type::Unit,
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

	fn nullify(&self, mut expr: Code<TypeId>) -> Code<TypeId> {
		if self.types[expr.typ] == UncheckedType::Unit {
			return expr;
		}
		let mut stmts = match expr.kind {
			CodeKind::Block(mut stmts) => stmts,
			_ => vec![expr],
		};
		stmts.push(Code { kind: CodeKind::Unit, typ: self.types.unit });
		Code { kind: CodeKind::Block(stmts), typ: self.types.unit }
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
		self.leaders.extend((0..5).map(Cell::new));
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
			_ => {
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
