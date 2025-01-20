#![allow(unused)]
use std::cell::Cell;
use std::collections::HashMap;
use std::marker::PhantomData;
use std::ops::Index;
use std::ops::IndexMut;

use indexmap::IndexMap;

use super::ast::*;
use super::mir;
use super::mir::*;
use crate::utils;
use crate::utils::try_map;

pub fn typecheck(program: Block<()>) -> Result<Program, TypeError> {
	Typechecker::new().typecheck(program)
}

struct Typechecker<'a> {
	// ast: Block<'a, ()>,
	signatures: IndexMap<Str, FnSign>,
	_dummy: PhantomData<&'a ()>,
}

struct FnSign {
	name: Str,
	params: Vec<(Str, Type)>,
	result: Type,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct TypeError<'a> {
	pub location: &'a str,
	pub message: &'static str,
}

impl<'a> Typechecker<'a> {
	fn new() -> Self {
		Self { signatures: IndexMap::new(), _dummy: PhantomData }
	}

	fn test_mut(&mut self) {}

	fn typecheck(mut self, ast: Block<'a, ()>) -> Result<Program<'a>, TypeError<'a>> {
		let mut sources = vec![];

		for stmt in ast.stmts.into_iter() {
			let ExprKind::Function(name, params, body) = stmt.kind else {
				// TODO Use different type of error?
				return Err(TypeError { location: stmt.source, message: "Expected function definition" });
			};
			let name = Str::from(name);
			let params = try_map(params, |(name, t)| Ok((Str::from(name), self.parse_type(t)?)))?;
			let result = Type::Unit;
			self.signatures.insert(name.clone(), FnSign { name, params, result });
			sources.push(body);
		}
		let mut functions = vec![];
		for (signature, source) in self.signatures.values().zip(sources) {
			let fn_typechecker = FunctionTypechecker::new(&self, signature);
			functions.push(fn_typechecker.typecheck(source)?);
		}
		Ok(Program { functions })
	}

	fn parse_type(&self, typename: &'a str) -> Result<Type, TypeError<'a>> {
		match typename {
			"int" => Ok(Type::I64),
			"uint" => Ok(Type::U64),
			"float" => Ok(Type::F64),
			"str" => Ok(Type::Str),
			"unit" => Ok(Type::Unit),
			_ => Err(TypeError { location: typename, message: "Unknown type" }),
		}
	}
}

struct FunctionTypechecker<'a, 'b> {
	typechecker: &'b Typechecker<'a>,
	signature: &'b FnSign,
	locals: Vec<(Str, TypeId)>,
	scope: HashMap<Str, usize>,
	types: TypesDsu,
	_dummy: PhantomData<&'a ()>,
}

impl<'a, 'b> FunctionTypechecker<'a, 'b> {
	fn new(typechecker: &'b Typechecker<'a>, signature: &'b FnSign) -> Self {
		let mut locals = vec![];
		let mut scope = HashMap::new();
		let mut types = TypesDsu::new();
		for (i, (name, real_type)) in signature.params.iter().enumerate() {
			let type_id = types.add_type(*real_type);
			locals.push((name.clone(), type_id));
			scope.insert(name.clone(), i);
		}
		Self { typechecker, signature, locals, scope, types, _dummy: PhantomData }
	}

	fn typecheck(mut self, body: Block<'a, ()>) -> Result<Function<'a, ()>, TypeError<'a>> {
		let mut stmts = vec![];
		for stmt in body.stmts {
			stmts.push(self.annotate(stmt)?);
		}
		for typ in self.types.types.values_mut() {
			if typ.is_none() {
				*typ = Some(Type::Unit);
			}
		}
		let mut typechecked_body = vec![];
		for stmt in stmts {
			typechecked_body.push(self.typecheck_stmt(stmt)?);
		}
		Ok(Function {
			name: self.signature.name.clone(),
			params: self.signature.params.clone(),
			result: self.signature.result,
			locals: self.locals.into_iter().map(|(name, typ)| (name, self.types[typ].unwrap())).collect(),
			body: typechecked_body,
			extra: (),
		})
	}

	fn typecheck_stmt(
		&mut self,
		expr: Expr<'a, ExprDataWithTypeId>,
	) -> Result<InstrCntrl<'a>, TypeError<'a>> {
		let source = expr.source;
		let kind = match expr.kind {
			ExprKind::Print(expr) => todo!(),
			ExprKind::Println(expr) => InstrKindCntrl::Block(vec![
				InstrCntrl {
					source,
					kind: Box::new(InstrKindCntrl::PrintStr(match self.typecheck_expr(*expr)?.kind {
						InstrKind::Cntrl(instr_cntrl) => todo!(),
						InstrKind::Bool(instr_bool) => todo!(),
						InstrKind::I64(instr_i64) => todo!(),
						InstrKind::U64(instr_u64) => todo!(),
						InstrKind::F64(instr_f64) => todo!(),
						InstrKind::Str(instr_str) => instr_str,
					})),
				},
				InstrCntrl {
					source,
					kind: Box::new(InstrKindCntrl::PrintStr(InstrStr {
						source,
						kind: Box::new(InstrKindStr::Value("\n".into())),
					})),
				},
			]),
			ExprKind::Assignment(name, expr) | ExprKind::Definition(name, _, expr) => {
				match self.types[expr.type_id].unwrap() {
					Type::Unit => InstrKindCntrl::Drop(self.typecheck_expr(*expr)?),
					Type::Bool => InstrKindCntrl::SetBool(self.scope[name], self.typecheck_bool(*expr)?),
					Type::I64 => InstrKindCntrl::SetI64(self.scope[name], self.typecheck_i64(*expr)?),
					Type::U64 => InstrKindCntrl::SetU64(self.scope[name], self.typecheck_u64(*expr)?),
					Type::F64 => InstrKindCntrl::SetF64(self.scope[name], self.typecheck_f64(*expr)?),
					Type::Str => InstrKindCntrl::SetStr(self.scope[name], self.typecheck_str(*expr)?),
				}
			}
			ExprKind::If(expr, block, block1) => todo!(),
			ExprKind::While(expr, block) => todo!(),
			ExprKind::Return(expr) => todo!(),
			ExprKind::Variable(_) => todo!(),
			ExprKind::Grouping(expr) => todo!(),
			ExprKind::FunctionCall(expr, vec) => todo!(),
			_ => unreachable!(),
		};
		Ok(InstrCntrl { source, kind: Box::new(kind) })
	}

	fn typecheck_bool(&mut self, expr: Expr<'a, ExprDataWithTypeId>) -> Result<InstrBool<'a>, TypeError<'a>> {
		let source = expr.source;
		let kind = match expr.kind {
			ExprKind::Variable(name) => InstrKindBool::Variable(self.scope[&*name]),
			ExprKind::Grouping(expr) => *self.typecheck_bool(*expr)?.kind,
			ExprKind::Unary(unary_op, expr) => match unary_op.kind {
				UnaryOpKind::Bang => InstrKindBool::Not(self.typecheck_bool(*expr)?),
				_ => unreachable!(),
			},
			ExprKind::Binary(expr, bin_op, expr1) => todo!(),
			ExprKind::FunctionCall(expr, vec) => todo!(),
			_ => unreachable!(),
		};
		Ok(InstrBool { source, kind: Box::new(kind) })
	}

	fn typecheck_u64(&mut self, expr: Expr<'a, ExprDataWithTypeId>) -> Result<InstrU64<'a>, TypeError<'a>> {
		let source = expr.source;
		let kind = match expr.kind {
			_ => todo!(),
		};
		Ok(InstrU64 { source, kind: Box::new(kind) })
	}

	fn typecheck_i64(&mut self, expr: Expr<'a, ExprDataWithTypeId>) -> Result<InstrI64<'a>, TypeError<'a>> {
		let source = expr.source;
		let kind = match expr.kind {
			_ => todo!(),
		};
		Ok(InstrI64 { source, kind: Box::new(kind) })
	}

	fn typecheck_f64(&mut self, expr: Expr<'a, ExprDataWithTypeId>) -> Result<InstrF64<'a>, TypeError<'a>> {
		let source = expr.source;
		let kind = match expr.kind {
			_ => todo!(),
		};
		Ok(InstrF64 { source, kind: Box::new(kind) })
	}

	fn typecheck_str(&mut self, expr: Expr<'a, ExprDataWithTypeId>) -> Result<InstrStr<'a>, TypeError<'a>> {
		let source = expr.source;
		let kind = match expr.kind {
			ExprKind::String(literal) => InstrKindStr::Value(literal.into()),
			ExprKind::Variable(_) => todo!(),
			ExprKind::Grouping(expr) => todo!(),
			ExprKind::Binary(expr, bin_op, expr1) => todo!(),
			ExprKind::FunctionCall(expr, vec) => todo!(),
			_ => unreachable!(),
		};
		Ok(InstrStr { source, kind: Box::new(kind) })
	}

	fn typecheck_expr(&mut self, expr: Expr<'a, ExprDataWithTypeId>) -> Result<Instr<'a>, TypeError<'a>> {
		let source = expr.source;
		let kind = match self.types[expr.type_id].unwrap() {
			Type::Unit => InstrKind::Cntrl(self.typecheck_stmt(expr)?),
			Type::Bool => todo!(),
			Type::I64 => todo!(),
			Type::U64 => todo!(),
			Type::F64 => todo!(),
			Type::Str => InstrKind::Str(self.typecheck_str(expr)?),
		};
		Ok(Instr { source, kind })
	}

	fn annotate(&mut self, expr: Expr<'a, ()>) -> Result<Expr<'a, ExprDataWithTypeId>, TypeError<'a>> {
		use ExprKind::*;
		let mut type_id = self.types.new_type();
		let source = expr.source;
		let kind = match expr.kind {
			Print(expr) => {
				self.types[type_id] = Some(Type::Unit);
				Print(Box::new(self.annotate(*expr)?))
			}
			Println(expr) => {
				self.types[type_id] = Some(Type::Unit);
				Println(Box::new(self.annotate(*expr)?))
			}
			Definition(name, typename, expr) => {
				let id = self.types.new_type();
				if !typename.is_empty() {
					let typ = self.typechecker.parse_type(typename).unwrap();
					self.types[id] = Some(typ);
				}
				self.scope.insert(name.into(), self.locals.len());
				self.locals.push((name.into(), id));
				self.types[type_id] = Some(Type::Unit);
				let expr = self.annotate(*expr)?;
				if let Err((t1, t2)) = self.types.merge(id, expr.type_id) {
					return Err(TypeError {
						location: expr.source,
						message: Box::leak(format!("Expected {:?}, found {:?}", t1, t2).into()),
					});
				}
				Definition(name, typename, Box::new(expr))
			}
			Assignment(name, expr) => {
				let id = self.locals[self.scope[name]].1;
				let expr = self.annotate(*expr)?;
				if let Err((t1, t2)) = self.types.merge(id, expr.type_id) {
					return Err(TypeError {
						location: expr.source,
						message: Box::leak(format!("Expected {:?}, found {:?}", t1, t2).into()),
					});
				}
				self.types[type_id] = Some(Type::Unit);
				Assignment(name, Box::new(expr))
			}
			If(expr, block, block1) => todo!(),
			While(expr, block) => todo!(),
			Return(expr) => todo!(),
			Function(_, vec, block) => todo!(),
			Class(_, vec) => todo!(),
			Import(_) => todo!(),
			Integer(literal) => {
				self.types[type_id] = Some(Type::Str);
				Integer(literal)
			},
			Float(_) => todo!(),
			String(literal) => {
				self.types[type_id] = Some(Type::Str);
				String(literal)
			}
			Variable(_) => todo!(),
			Grouping(expr) => todo!(),
			Unary(unary_op, expr) => todo!(),
			Binary(expr, bin_op, expr1) => todo!(),
			FunctionCall(expr, vec) => todo!(),
		};
		Ok(Expr::typed(expr.source, kind, type_id))
	}
}

impl<'a> Expr<'a, ExprDataWithTypeId> {
	fn typed(
		source: &'a str,
		kind: ExprKind<'a, ExprDataWithTypeId>,
		type_id: TypeId,
	) -> Expr<'a, ExprDataWithTypeId> {
		Expr::from(source, kind, ExprDataWithTypeId { type_id })
	}
}

// fn expr<'a>(
// 	source: &'a str,
// 	kind: ExprKind<'a, TypeId>,
// 	type_id: DsuElementId,
// ) -> Expr<'a, TypeId> {
// 	Expr::from(source, kind, TypeId { type_id })
// }

struct ExprDataWithTypeId {
	pub type_id: TypeId,
}

struct TypesDsu {
	leaders: Vec<Cell<u32>>,
	types: HashMap<u32, Option<Type>>,
}

#[derive(Clone, Copy, Debug)]
struct TypeId(u32);

impl TypesDsu {
	fn new() -> Self {
		Self { leaders: vec![], types: HashMap::new() }
	}

	fn new_type(&mut self) -> TypeId {
		let id = self.leaders.len() as u32;
		self.leaders.push(Cell::new(id));
		self.types.insert(id, None);
		TypeId(id)
	}

	fn add_type(&mut self, typ: Type) -> TypeId {
		let id = self.leaders.len() as u32;
		self.leaders.push(Cell::new(id));
		self.types.insert(id, Some(typ));
		TypeId(id)
	}

	fn merge(&mut self, u: TypeId, v: TypeId) -> Result<(), (Type, Type)> {
		let u = self.resolve_leader(u);
		let v = self.resolve_leader(v);
		if u == v {
			return Ok(())
		}
		let t1 = self.types.remove(&(u as u32)).unwrap();
		let t2 = self.types.remove(&(v as u32)).unwrap();
		let union_type = match (t1, t2) {
			(None, None) => None,
			(None, Some(t)) => Some(t),
			(Some(t), None) => Some(t),
			(Some(t1), Some(t2)) => {
				if (t1 == t2) {
					Some(t1)
				} else {
					return Err((t1, t2));
				}
			}
		};
		let union_id = self.merge_sets(u, v) as u32;
		self.types.insert(union_id, union_type);
		Ok(())
	}

	fn resolve_leader(&self, id: TypeId) -> usize {
		let i = id.0 as usize;
		let mut leader = self.leaders[i].get();
		if self.leaders[leader as usize].get() != leader {
			leader = self.resolve_leader(TypeId(leader)) as u32;
			self.leaders[i].set(leader);
		}
		leader as usize
	}

	fn merge_sets(&mut self, mut u: usize, mut v: usize) -> usize {
		// TODO: Rank system?
		if u > v {
			std::mem::swap(&mut u, &mut v);
		}
		self.leaders[v] = Cell::new(u as u32);
		u
	}
}

impl Index<TypeId> for TypesDsu {
	type Output = Option<Type>;

	fn index(&self, index: TypeId) -> &Self::Output {
		&self.types[&(self.resolve_leader(index) as u32)]
	}
}

impl IndexMut<TypeId> for TypesDsu {
	fn index_mut(&mut self, index: TypeId) -> &mut Self::Output {
		self.types.get_mut(&(self.resolve_leader(index) as u32)).unwrap()
	}
}
