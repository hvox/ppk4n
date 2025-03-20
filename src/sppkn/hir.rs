#![allow(unused)]

use std::{
	cell::Cell,
	collections::HashMap,
	fmt::Debug,
	fs::{self, read_to_string},
	ops::{Add, Deref, Index, IndexMut},
	path::{Path, PathBuf},
	rc::Rc,
	time::Instant,
};

use ordered_float::Float;
use super::{
	error::{self, Error, PpknErrorKind},
	parser::*,
};
use PpknErrorKind::*;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Program {
	poisoned: bool,
	pub main: Str,
	pub root: PathBuf,
	pub sources: HashMap<Str, Str>,
	pub modules: HashMap<Str, Module>,

	pub types: HashMap<Str, NamedType>,
	// TODO: Automatically create body based on Type
	pub globals: HashMap<Str, (Type, Option<Body>)>,
	pub functions: HashMap<Str, Function>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct NamedType {
	methods: HashMap<Str, Function>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Module {
	pub name: Str,
	pub ast: Ast,
	pub dependents: Vec<Str>,
	pub poisoned: bool,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Function {
	pub module: Str,
	pub name: Str,
	pub parameters: Vec<(Str, Type)>,
	pub result: Type,
	pub body: Body,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Body {
	pub types: Types,
	pub value: Instr,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Instr {
	pub typ: TypeId,
	pub kind: InstrKind,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum InstrKind {
	String(Str),
	Integer(Str),
	Identifier(Str),
	Tuple(Vec<Instr>),
	SetLocal(Str, Rc<Instr>),
	GetLocal(Str),
	Block(Rc<Block>),
	While(Rc<Instr>, Rc<Instr>),
	If(Rc<Instr>, Rc<Instr>, Rc<Instr>),
	MethodCall(Rc<Instr>, Str, Vec<Instr>),
	FnCall(Str, Vec<Instr>),
	Return(Rc<Instr>),
	Unreachable,
	NoOp,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Block {
	stmts: Vec<(Option<(Str, bool)>, Instr)>,
	result: Instr,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Type {
	Tuple(Vec<Type>),
	Array(Rc<Type>),
	Name(Str),
	Void,
}

// TODO: think about less stupid name.
// Something like InferedType.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum PossiblyUnspecifiedType {
	Tuple(Vec<TypeId>),
	Array(TypeId),
	Name(Str),
	Void,
	Bool,
	Char,
	Str,
	Int(u8),
	Uint(u8),
	Float(u8),

	Unknown,
	UnkInt,
	UnkFloat,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Types {
	handles: Vec<Cell<u32>>,
	types: HashMap<u32, PossiblyUnspecifiedType>,
	// TODO: replace these guys with constants
	void: TypeId,
	bool: TypeId,
	char: TypeId,
	string: TypeId,
}

#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub struct TypeId(u32);

type Str = Rc<str>;

impl Program {
	pub fn new<Path>(source_root_path: Path, sources: HashMap<Str, Str>, main: Str) -> Self
	where
		Path: Into<PathBuf>,
	{
		let root = source_root_path.into();
		let modules = HashMap::new();
		let globals = HashMap::new();
		let functions = HashMap::new();
		let types = HashMap::new();
		let program = Self { poisoned: false, main, root, sources, modules, globals, functions, types };
		program
	}

	pub fn load_and_typecheck(&mut self, name: Str) -> Result<(), Vec<Error>> {
		let (modules, errors) = self.load(name);
		if !errors.is_empty() {
			return Err(errors);
		}
		let mut errors = vec![];
		for module in modules {
			if let Err(new_errors) = self.typecheck_module(&module) {
				errors.extend(new_errors);
			}
		}
		if errors.is_empty() {
			Ok(())
		} else {
			Err(errors)
		}
	}

	pub fn load(&mut self, name: Str) -> (Vec<Str>, Vec<Error>) {
		let mut parse_queue = vec![(name, "".into(), (0, 0))];
		let mut parsed = vec![];
		let mut errors = vec![];
		while let Some((name, dependent, location)) = parse_queue.pop() {
			if let Some(module) = self.modules.get_mut(&name) {
				module.dependents.push(name.clone());
				continue;
			}
			let source = match self.sources.get(&name) {
				Some(source) => source.clone(),
				None => {
					let source_filename = format!("{}.ppkn", name);
					let source_path = self.root.join(source_filename);
					// TODO: handle NotFound error
					let source = Str::from(fs::read_to_string(source_path).unwrap());
					self.sources.insert(name.clone(), source.clone());
					source
				}
			};
			let (ast, syntax_errors) = super::parser::parse(&source);
			errors.extend(syntax_errors.into_iter().map(|error| error.into_error(name.clone())));
			for dependency in &ast.dependencies {
				parse_queue.push((dependency.name.clone(), name.clone(), dependency.location));
			}
			let module = Module { name: name.clone(), ast, dependents: vec![dependent], poisoned: false };
			self.modules.insert(name.clone(), module);
			parsed.push(name);
		}
		(parsed, errors)
	}

	pub fn typecheck_module(&mut self, module_name: &str) -> Result<(), Vec<Error>> {
		let mut errors = vec![];
		// update self.functions and self.globals
		let ast = &self.modules[module_name].ast;
		// let globals: HashMap<Str, &FunDef> = HashMap::new();
		// let functions: HashMap<Str, &FunDef> = HashMap::new();
		for (_, global) in &ast.globals {
			let name = global.name.clone();
			let typ = self.resolve_typename(module_name, &global.typename);
			let body = match &global.value {
				Some(expr) => {
					let typechecker = BodyTypechecker::new(&self, module_name, &typ, &[]);
					let (body, new_errors) = typechecker.typecheck_body(&expr);
					errors.extend(new_errors);
					Some(body)
				}
				None => None,
			};
			let path = format!("{}.{}", module_name, name).into();
			self.globals.insert(path, (typ, body));
		}
		for (_, function) in &ast.functions {
			let name = function.name.clone();
			let params: Vec<_> =
				function.params.iter().map(|x| (x.name.clone(), self.resolve_typename(module_name, &x.typ))).collect();
			let result = self.resolve_typename(module_name, &function.result);
			let typechecker = BodyTypechecker::new(&self, module_name, &result, &params[..]);
			let (body, new_errors) = typechecker.typecheck_body(&function.body);
			let function = Function { module: module_name.into(), name: name.clone(), parameters: params, result, body };
			let path = format!("{}.{}", module_name, name).into();
			self.functions.insert(path, function);
			errors.extend(new_errors);
		}
		if errors.is_empty() {
			Ok(())
		} else {
			self.modules.get_mut(module_name).unwrap().poisoned = true;
			self.poisoned = true;
			Err(errors)
		}
	}

	fn resolve_typename(&self, module: &str, typename: &Typename) -> Type {
		use TypenameKind::*;
		match &typename.kind {
			Tuple(field_names) => {
				let mut fields = vec![];
				for field in field_names {
					fields.push(self.resolve_typename(module, field));
				}
				Type::Tuple(fields)
			}
			Array(item) => {
				let item = self.resolve_typename(module, item);
				Type::Array(item.into())
			}
			Name(name) => Type::Name(name.clone()),
			Unknown => Type::Void,
			Void => Type::Void,
		}
	}

	pub fn is_poisoned(&self) -> bool {
		self.poisoned
	}
}

impl Types {
	fn new() -> Self {
		use PossiblyUnspecifiedType::*;
		let handles = vec![Cell::new(0), Cell::new(1), Cell::new(2), Cell::new(3)];
		let types = HashMap::from([(0, Void), (1, Bool), (2, Char), (3, Str)]);
		assert!(handles.len() == types.len());
		let mut types = Self {
			handles: handles.into(),
			types: types.into(),
			void: TypeId(0),
			bool: TypeId(1),
			char: TypeId(2),
			string: TypeId(3),
		};
		types
	}

	fn insert_specified(&mut self, typ: &Type) -> TypeId {
		use PossiblyUnspecifiedType::*;
		let typ = match typ {
			Type::Tuple(fields) => Tuple(fields.iter().map(|x| self.insert_specified(x)).collect()),
			Type::Array(item_type) => Array(self.insert_specified(item_type)),
			Type::Name(name) => match &name[..] {
				"bool" => return self.bool,
				"char" => return self.char,
				"str" => return self.string,
				"i32" => Int(32),
				"i64" => Int(64),
				"u32" => Uint(32),
				"u64" => Uint(64),
				"f32" => Float(32),
				"f64" => Float(64),
				_ => Name(name.clone()),
			},
			Type::Void => return self.void,
		};
		self.insert(typ)
	}

	fn insert(&mut self, typ: PossiblyUnspecifiedType) -> TypeId {
		let id = self.handles.len() as u32;
		self.handles.push(Cell::new(id));
		self.types.insert(id, typ);
		TypeId(id)
	}

	fn unify(&mut self, u: TypeId, v: TypeId) -> Result<(), (PossiblyUnspecifiedType, PossiblyUnspecifiedType)> {
		let u_hndl = self.get_handle(u);
		let v_hndl = self.get_handle(v);
		if u_hndl == v_hndl {
			return Ok(());
		}

		// TODO: Try pop immediately instead of clone&remove
		let u_type = self.types[&u_hndl].clone();
		let v_type = self.types[&v_hndl].clone();
		use PossiblyUnspecifiedType::*;
		let unified_type = match (u_type, v_type) {
			(Unknown, Unknown) => Unknown,
			(Unknown, x) => x,
			(x, Unknown) => x,
			(x, y) if x == y => x,
			(Int(x), UnkInt) => Int(x),
			(UnkInt, Int(x)) => Int(x),
			(Uint(x), UnkInt) => Uint(x),
			(UnkInt, Uint(x)) => Uint(x),
			(Float(x), UnkFloat) => Float(x),
			(UnkFloat, Float(x)) => Float(x),
			(Tuple(xs), Tuple(ys)) if xs.len() == ys.len() => {
				for (x, y) in xs.iter().zip(&ys) {
					self.unify(*x, *y).map_err(|_| (Tuple(xs.clone()), Tuple(ys.clone())))?;
				}
				Tuple(xs)
			}
			(x, y) => return Err((x, y)),
		};
		let (u, v) = (u_hndl.min(v_hndl), v_hndl.max(u_hndl));
		self.types.remove(&u);
		self.types.remove(&v);
		self.handles[v as usize].set(u);
		self.types.insert(u, unified_type);
		Ok(())
	}

	fn get_handle(&self, id: TypeId) -> u32 {
		let id = id.0 as usize;
		let mut leader = self.handles[id].get();
		if self.handles[leader as usize].get() != leader {
			leader = self.get_handle(TypeId(leader));
			self.handles[id].set(leader);
		}
		leader
	}
}

impl Index<TypeId> for Types {
	type Output = PossiblyUnspecifiedType;

	fn index(&self, id: TypeId) -> &Self::Output {
		&self.types[&(self.get_handle(id))]
	}
}

impl IndexMut<TypeId> for Types {
	fn index_mut(&mut self, id: TypeId) -> &mut Self::Output {
		self.types.get_mut(&(self.get_handle(id))).unwrap()
	}
}

mod alternative_instr_type_with_unification {
	use super::*;

	#[derive(Default)]
	pub struct InstrType {
		kind: Cell<Rc<Cell<TypeKind>>>,
	}

	#[derive(Debug, PartialEq, Eq, Clone)]
	pub enum TypeKind {
		// Tuple(Vec<(Str, InstrType, Option<Instr>)>),
		Array(Box<InstrType>),
		Name(Str),
		Void,
		Unknown,
		Float,
		Integer,
		Proxy(InstrType),
	}

	impl Debug for InstrType {
		fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
			let reference = self.kind.take();
			let typ = reference.take();
			let result = write!(f, "{:?}", typ);
			reference.set(typ);
			self.kind.set(reference);
			result
		}
	}

	impl Clone for InstrType {
		fn clone(&self) -> Self {
			let kind = self.kind.take();
			self.kind.set(kind.clone());
			Self { kind: Cell::new(kind) }
		}
	}

	impl PartialEq for InstrType {
		fn eq(&self, other: &Self) -> bool {
			todo!()
		}
	}
	impl Eq for InstrType {}

	impl Default for TypeKind {
		fn default() -> Self {
			TypeKind::Unknown
		}
	}

	impl InstrType {
		fn new(kind: TypeKind) -> Self {
			Self { kind: Cell::new(Cell::new(kind).into()) }
		}

		fn get(&self) -> Rc<Cell<TypeKind>> {
			let reference = self.kind.take();
			let reference = match reference.take() {
				TypeKind::Proxy(x) => x.get(),
				kind => {
					reference.set(kind);
					reference
				}
			};
			self.kind.set(reference.clone());
			reference
		}

		fn set(&self, kind: Rc<Cell<TypeKind>>) {
			let root = self.get();
			if root.as_ptr() != kind.as_ptr() {
				self.kind.set(kind.clone());
				let new_root = InstrType { kind: kind.into() };
				root.set(TypeKind::Proxy(new_root));
			}
		}

		fn unify(&self, other: &InstrType) {
			let root = self.get();
			let other = other.get();
			if root.as_ptr() != other.as_ptr() {
				let new_root = InstrType { kind: other.into() };
				root.set(TypeKind::Proxy(new_root));
			}
		}
	}
}

mod alternative_instr_type {
	use super::*;

	// #[derive(Debug, PartialEq, Eq, Clone)]
	pub struct Ty {
		kind: Cell<TyKind>,
	}

	#[derive(Debug, PartialEq, Eq, Clone)]
	pub enum TyKind {
		Tuple(Vec<(Str, Ty /*, Option<Instr>*/)>),
		Array(Rc<Ty>),
		Name(Str),
		Void,
		Unknown,
		Proxy(Rc<Ty>),
	}

	impl Debug for Ty {
		fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
			let typ = self.kind.take();
			let result = write!(f, "{:?}", typ);
			self.kind.set(typ);
			result
		}
	}

	impl Eq for Ty {}
	impl PartialEq for Ty {
		fn eq(&self, other: &Self) -> bool {
			todo!()
		}
	}

	impl Clone for Ty {
		fn clone(&self) -> Self {
			let kind = Cell::new(self.get());
			Self { kind }
		}
	}

	impl Default for TyKind {
		fn default() -> Self {
			TyKind::Unknown
		}
	}

	impl Ty {
		fn get(&self) -> TyKind {
			let kind = self.kind.take();
			let kind = match &kind {
				TyKind::Proxy(ty) => ty.get(),
				_ => kind,
			};
			self.kind.set(kind.clone());
			kind
		}

		fn set(&self, typ: TyKind) {
			let kind = self.kind.replace(typ.clone());
			match kind {
				TyKind::Proxy(ty) => ty.set(typ),
				_ => todo!(),
			}
		}

		fn unify(&self, other: &Ty) {
			todo!()
		}
	}
}

struct BodyTypechecker<'a> {
	program: &'a Program,
	module: Str,
	result: TypeId,
	locals: Vec<(Str, TypeId)>,
	scopes: Vec<usize>,
	types: Types,
	errors: Vec<Error>,
}

impl<'a> BodyTypechecker<'a> {
	fn new(program: &'a Program, module: &'a str, result: &'a Type, params: &[(Str, Type)]) -> Self {
		let mut types = Types::new();
		let result = types.insert_specified(result);
		let locals = if params.is_empty() {
			vec![]
		} else {
			let mut locals = vec![];
			for (name, typ) in params {
				let type_id = types.insert_specified(typ);
				locals.push((name.clone(), type_id));
			}
			locals
		};
		Self { program, module: module.into(), result, locals, scopes: vec![], types, errors: vec![] }
	}

	fn typecheck_body(mut self, expr: &Expr) -> (Body, Vec<Error>) {
		let instr = self.typecheck(expr, self.result);
		(Body { types: self.types, value: instr }, self.errors)
	}

	fn typecheck(&mut self, expr: &Expr, typ: TypeId) -> Instr {
		use InstrKind::*;
		let loc = expr.location;
		let (kind, actual_type) = match &expr.kind {
			ExprKind::String(string) => (String(string.clone()), self.types.string),
			ExprKind::Integer(integer) => (Integer(integer.clone()), self.types.insert(PossiblyUnspecifiedType::UnkInt)),
			ExprKind::Identifier(name) => (GetLocal(name.clone()), self.vartype(loc, &name)),
			ExprKind::Tuple(fields) => {
				let mut instrs = vec![];
				let mut types = vec![];
				for expr in fields {
					let typ = self.types.insert(PossiblyUnspecifiedType::Unknown);
					let instr = self.typecheck(expr, typ);
					instrs.push(instr);
					types.push(typ);
				}
				let tuple_type = self.types.insert(PossiblyUnspecifiedType::Tuple(types));
				(Tuple(instrs), tuple_type)
			}
			ExprKind::Assignment(name, expr) => {
				let typ = self.vartype(loc, &name);
				let value = self.typecheck(expr, typ);
				(SetLocal(name.clone(), value.into()), self.types.void)
			}
			// ExprKind::GetLocal(name) => (GetLocal(name.clone()), self.vartype(loc, &name)),
			ExprKind::Block(block) => {
				let block = self.typecheck_block(block, typ);
				(InstrKind::Block(block.into()), typ)
			}
			ExprKind::While(condition, body) => {
				let condition = self.typecheck(&condition, self.types.bool);
				let body = self.typecheck(&body, self.types.void);
				(While(condition.into(), body.into()), self.types.void)
			}
			ExprKind::If(condition, then, otherwise) => {
				let condition = self.typecheck(&condition, self.types.bool);
				let then = self.typecheck(&then, typ);
				let otherwise = self.typecheck(&otherwise, typ);
				(If(condition.into(), then.into(), otherwise.into()), typ)
			}
			ExprKind::MethodCall(receiver, method, args) => {
				let receiver_typ = self.types.insert(PossiblyUnspecifiedType::Unknown);
				let receiver = self.typecheck(&receiver, receiver_typ);
				if let Ok(mut signature) = self.resolve_method(receiver_typ, method) {
					let result_type = signature.pop().unwrap();
					let params = signature;
					if params.len() == args.len() {
						let args = args.iter().zip(params).map(|(x, t)| self.typecheck(x, t)).collect();
						// let mut checked_args = vec![];
						// for (arg, typ) in args.iter().zip(params) {
						//     checked_args.push(self.typecheck(arg, typ));
						// }
						(MethodCall(receiver.into(), method.clone(), args), result_type)
					} else {
						let error_message = format!("this method takes {} arguments but {} were given", params.len(), args.len());
						self.error(TypeError, loc, error_message);
						// TODO: prettify
						let args = args
							.iter()
							.map(|arg| {
								let typ = self.types.insert(PossiblyUnspecifiedType::Unknown);
								self.typecheck(arg, typ)
							})
							.collect();
						(MethodCall(receiver.into(), method.clone(), args), result_type)
					}
				} else {
					self.error(NameError, loc, format!("method not found in {:?}", self.types[receiver_typ]));
					let args = args
						.iter()
						.map(|arg| {
							let typ = self.types.insert(PossiblyUnspecifiedType::Unknown);
							self.typecheck(arg, typ)
						})
						.collect();
					(MethodCall(receiver.into(), method.clone(), args), typ)
				}
			}
			ExprKind::FnCall(function, args) => {
				if let Ok((path, params, result_type)) = self.resolve_function(function) {
					let args = if params.len() == args.len() {
						args.iter().zip(params).map(|(x, t)| self.typecheck(x, t)).collect()
					} else {
						let error_message = format!("expected {} arguments but {} were given", params.len(), args.len());
						self.error(TypeError, loc, error_message);
						args.iter()
							.map(|arg| {
								let typ = self.types.insert(PossiblyUnspecifiedType::Unknown);
								self.typecheck(arg, typ)
							})
							.collect()
					};
					(FnCall(path, args), result_type)
				} else {
					if !matches!(function.as_ref(), "print" | "println" | "eprint" | "eprintln" | "memory_copy") {
						self.error(NameError, loc, "function not found in this scope".into());
					}
					let args = args
						.iter()
						.map(|arg| {
							let typ = self.types.insert(PossiblyUnspecifiedType::Unknown);
							self.typecheck(arg, typ)
						})
						.collect();
					(FnCall(function.clone(), args), typ)
				}
			}
			ExprKind::Return(value) => (Return(self.typecheck(value, self.result).into()), typ),
			ExprKind::Unreachable => (Unreachable, typ),
			ExprKind::Nothing => (NoOp, self.types.void),
		};
		if typ != actual_type {
			self.unify(expr.location, typ, actual_type);
		}
		Instr { typ, kind }
	}

	fn typecheck_block(&mut self, block: &Indented, result: TypeId) -> Block {
		let is_expr = self.types[result] != PossiblyUnspecifiedType::Void;
		let locals_count = self.locals.len();
		let mut block_stmts = &block.stmts[..block.stmts.len() - is_expr as usize];
		let mut stmts = vec![];
		for stmt in block_stmts {
			match stmt {
				Stmt::DefLocal(vardef) => {
					let name = vardef.name.clone();
					let typ = self.resolve_typename(&vardef.typename);
					self.locals.push((name.clone(), typ));
					let expr = self.typecheck(&vardef.value.clone().unwrap(), typ);
					stmts.push((Some((name, vardef.mutable)), expr));
				}
				Stmt::Expression(expr) => {
					let typ = match expr.kind {
						ExprKind::Block(_) | ExprKind::If(_, _, _) => self.types.void,
						_ => self.types.insert(PossiblyUnspecifiedType::Unknown),
					};
					stmts.push((None, self.typecheck(expr, typ)));
				}
			}
		}
		let result = if self.types[result] == PossiblyUnspecifiedType::Void {
			Instr { typ: self.types.void, kind: InstrKind::NoOp }
		} else if let Some(stmt) = block.stmts.last() {
			match stmt {
				Stmt::Expression(expr) => self.typecheck(expr, result),
				Stmt::DefLocal(vardef) => {
					let name = vardef.name.clone();
					let typ = self.resolve_typename(&vardef.typename);
					let expr = self.typecheck(&vardef.value.clone().unwrap(), typ);
					stmts.push((Some((name, vardef.mutable)), expr));
					self.unify(vardef.location, result, self.types.void);
					Instr { typ: self.types.void, kind: InstrKind::NoOp }
				}
			}
		} else {
			self.unify(block.location, result, self.types.void);
			Instr { typ: self.types.void, kind: InstrKind::NoOp }
		};
		self.locals.truncate(locals_count);
		Block { stmts, result }
	}

	fn resolve_function(&mut self, function: &str) -> Result<(Str, Vec<TypeId>, TypeId), ()> {
		let (module, fname) =
			if let Some(i) = function.find('.') { (&function[..i], &function[i + 1..]) } else { (&self.module[..], function) };
		let Some(f) = &self.program.modules[module].ast.functions.get(fname) else { return Err(()) };
		let params =
			f.params.iter().map(|x| self.types.insert_specified(&self.program.resolve_typename(module, &x.typ))).collect();
		let result = self.types.insert_specified(&self.program.resolve_typename(module, &f.result));
		Ok((format!("{}.{}", module, fname).into(), params, result))
	}

	fn resolve_method(&mut self, receiver: TypeId, method: &str) -> Result<Vec<TypeId>, ()> {
		use PossiblyUnspecifiedType::*;
		let t = receiver;
		// eprintln!("{:?}", t);
		// eprintln!("{:?}", self.types.handles);
		// eprintln!("{:?}", self.types.types);
		let signature = match (&self.types[t], method) {
			(Int(_) | Uint(_) | Float(_) | UnkInt | UnkFloat, "add" | "sub" | "mul" | "div") => vec![t, t],
			(Int(_) | Uint(_) | UnkInt, "div_floor" | "rem_floor") => vec![t, t],
			(Int(_) | Uint(_) | UnkInt | Bool, "bitadd" | "bitor" | "bitxor") => vec![t, t],
			(Str, "add") => vec![t, t],
			(Str, "push") => vec![self.types.char, self.types.void],
			(_, "eq" | "ne" | "lt" | "le" | "gt" | "ge") => vec![t, self.types.bool],
			_ => return Err(()),
		};
		Ok(signature)
	}

	fn unify(&mut self, location: (u32, u32), expected_type: TypeId, actual_type: TypeId) {
		let Err((dst, src)) = self.types.unify(expected_type, actual_type) else {
			return;
		};
		self.errors.push(Error {
			module: self.module.clone(),
			cause_location: location,
			message: format!("expected {:?}, found {:?}", dst, src),
			kind: PpknErrorKind::TypeError,
		});
	}

	fn vartype(&mut self, location: (u32, u32), name: &str) -> TypeId {
		for (variable, typ) in self.locals.iter().rev() {
			if *name == **variable {
				return *typ;
			}
		}

		let path = if name.contains('.') { name } else { &format!("{}.{}", self.module, name) };
		if let Some((global_typ, _)) = self.program.globals.get(path) {
			return self.types.insert_specified(global_typ);
		}

		self.errors.push(Error {
			module: self.module.clone(),
			cause_location: location,
			message: format!("name {:?} is not defined", name),
			kind: PpknErrorKind::NameError,
		});
		self.types.insert(PossiblyUnspecifiedType::Unknown)
	}

	fn resolve_typename(&mut self, typename: &Typename) -> TypeId {
		use PossiblyUnspecifiedType::*;
		let typ = match &typename.kind {
			TypenameKind::Tuple(field_names) => {
				let mut fields = vec![];
				for field in field_names {
					let typ = self.resolve_typename(&field);
					fields.push(typ);
				}
				// PossiblyUnspecifiedType::Tuple(fields)
				todo!()
			}
			TypenameKind::Array(item) => {
				let item = self.resolve_typename(item);
				PossiblyUnspecifiedType::Array(item.into())
			}
			TypenameKind::Name(name) => {
				let typ = self.program.resolve_typename(&self.module, typename);
				return self.types.insert_specified(&typ);
			}
			TypenameKind::Unknown => PossiblyUnspecifiedType::Unknown,
			TypenameKind::Void => PossiblyUnspecifiedType::Void,
		};
		self.types.insert(typ)
	}

	fn error(&mut self, kind: PpknErrorKind, location: (u32, u32), message: String) {
		self.errors.push(Error { module: self.module.clone(), cause_location: location, message, kind });
		// TODO: figure out how to not forget to set the flag
		// self.program.modules[&self.module].poisoned.set(true);
		// Is using Cell here overkill?
		// self.program.modules[&self.module].poisoned = true;
	}
}
