#![allow(unused)]

use std::collections::HashMap;
use std::rc::Rc;

use indexmap::IndexMap;
use indexmap::IndexSet;

use crate::ppkn::lir::instr::CALL_FUNC;
use crate::ppkn::lir::instr::CALL_IMPORT;
use crate::ppkn::yair::Variable;

use super::hir;
use super::hir::Function;

use super::lir;
use super::lir::*;

pub fn lower_hir_to_lir(program: &hir::Program) -> lir::Program {
	HirToLirLowerer::new().lower(program)
}

struct HirToLirLowerer {
	queue: IndexSet<Rc<str>>,
	imports: IndexSet<Import>,
	funcs: IndexMap<Rc<str>, Func>,
	globals: IndexMap<Rc<str>, GlobalVariable>,
	data: Vec<u8>,
}

struct FnLowerer<'a> {
	program_lowerer: &'a mut HirToLirLowerer,
	function: &'a Function,
	locals_i32: Locals,
	locals_f32: Locals,
}

struct Locals {
	slots_count: u32,
	// TODO: try to use u32 here instead of Vec<u32>
	free_slots: Vec<u32>,
	used_slots: Vec<u32>,
	vars: HashMap<Rc<str>, u32>,
	shadowed_vars: Vec<(Rc<str>, u32)>,
	scopes: Vec<(usize, usize)>,
}

impl HirToLirLowerer {
	fn new() -> Self {
		Self { queue: IndexSet::new(), imports: IndexSet::new(), funcs: IndexMap::new(), globals: IndexMap::new(), data: vec![] }
	}

	fn lower(mut self, program: &hir::Program) -> lir::Program {
		self.queue.extend(program.exports.iter().cloned());
		let mut queue_start = 0;
		while queue_start < self.queue.len() {
			let f = &program.fns[&self.queue[queue_start]];
			queue_start += 1;
			println!("func {} {:?} -> {:?}", f.name, f.signature.params, f.signature.result);
			let func = FnLowerer::new(&mut self, f).lower();
			self.funcs.insert(f.name.clone(), func);
		}
		Program { imports: self.imports, funcs: self.funcs, globals: self.globals, data: self.data }
	}

	fn lower_type(&self, typ: &hir::Type) -> &'static [lir::ValueType] {
		match typ {
			hir::Type::Unit => &[],
			hir::Type::Bool => &[ValueType::I32],
			hir::Type::I32 => &[ValueType::I32],
			hir::Type::U32 => &[ValueType::I32],
			hir::Type::F32 => &[ValueType::F32],
			hir::Type::Str => &[ValueType::I32],
			hir::Type::Array(_) => &[ValueType::I32],
		}
	}
}

impl<'a> FnLowerer<'a> {
	fn new(lowerer: &'a mut HirToLirLowerer, function: &'a hir::Function) -> Self {
		Self { program_lowerer: lowerer, locals_i32: Locals::new(), locals_f32: Locals::new(), function }
	}

	fn lower(mut self) -> lir::Func {
		for (param, typ) in self.function.parameters.iter().zip(&self.function.signature.params) {
			match typ {
				hir::Type::Unit => {}
				hir::Type::Bool => _ = self.locals_i32.push(param.clone()),
				hir::Type::I32 => _ = self.locals_i32.push(param.clone()),
				hir::Type::U32 => _ = self.locals_i32.push(param.clone()),
				hir::Type::F32 => _ = self.locals_f32.push(param.clone()),
				hir::Type::Str => _ = self.locals_i32.push(param.clone()),
				hir::Type::Array(_) => _ = self.locals_i32.push(param.clone()),
			}
		}
		let signature = FuncType {
			parameters: self.function.signature.params.iter().flat_map(|t| self.program_lowerer.lower_type(t)).copied().collect(),
			result: self.program_lowerer.lower_type(&self.function.signature.result).to_vec(),
		};

		let mut code: Vec<Instr> = vec![];
		self.lower_expr(&self.function.body, &mut code);
		Func { signature, i32_locals: self.locals_i32.len(), f32_locals: self.locals_f32.len(), code }
	}

	fn lower_expr(&mut self, expr: &hir::Expr<hir::Type>, code: &mut Vec<Instr>) {
		println!(" -> {:?}", expr);
		use hir::ExprKind::*;
		match &expr.kind {
			Value(literal) => match expr.typ {
				hir::Type::Unit => {}
				hir::Type::Bool => {
					code.push(instr::I32_CONST);
					code.extend_from_slice(&(literal.as_bool() as u32).to_ne_bytes());
				}
				hir::Type::I32 => {
					code.push(instr::I32_CONST);
					code.extend_from_slice(&literal.as_i32().to_ne_bytes());
				}
				hir::Type::U32 => {
					code.push(instr::U32_CONST);
					code.extend_from_slice(&literal.as_u32().to_ne_bytes());
				}
				hir::Type::F32 => {
					code.push(instr::F32_CONST);
					code.extend_from_slice(&literal.as_f32().to_ne_bytes());
				}
				hir::Type::Str => {
					let string = literal.as_str();
					let ptr = self.program_lowerer.data.len() as u32;
					self.program_lowerer.data.extend_from_slice(&1u32.to_le_bytes());
					self.program_lowerer.data.extend_from_slice(&(string.len() as u32).to_le_bytes());
					self.program_lowerer.data.extend_from_slice(string.as_bytes());
					code.push(instr::I32_CONST);
					code.extend_from_slice(&ptr.to_ne_bytes());
				}
				hir::Type::Array(_) => unreachable!(),
			},
			DefLocal(variable, expr) => {
				self.lower_expr(expr, code);
				match expr.typ {
					hir::Type::Unit => {}
					hir::Type::F32 => {
						code.push(instr::F32_SET);
						code.extend_from_slice(&self.locals_f32.push(variable.clone()).to_ne_bytes());
					}
					_ => {
						code.push(instr::I32_SET);
						code.extend_from_slice(&self.locals_i32.push(variable.clone()).to_ne_bytes());
					}
				}
			}
			SetLocal(variable, expr) => {
				self.lower_expr(expr, code);
				match expr.typ {
					hir::Type::Unit => {}
					hir::Type::F32 => {
						code.push(instr::F32_SET);
						code.extend_from_slice(&self.locals_f32.find(&variable).to_ne_bytes());
					}
					_ => {
						code.push(instr::I32_SET);
						code.extend_from_slice(&self.locals_i32.find(&variable).to_ne_bytes());
					}
				}
			}
			GetLocal(variable) => match &expr.typ {
				hir::Type::Unit => {}
				hir::Type::F32 => {
					code.push(instr::F32_GET);
					code.extend_from_slice(&self.locals_f32.find(&variable).to_ne_bytes());
				}
				_ => {
					code.push(instr::I32_GET);
					code.extend_from_slice(&self.locals_i32.find(&variable).to_ne_bytes());
				}
			},
			Block(block) => self.lower_block(block, code),
			While(condition, body) => {
				code.push(instr::BLOCK);
				code.push(instr::LOOP);
				self.lower_expr(condition, code);
				code.push(instr::JUMP_IF);
				code.extend_from_slice(&1u32.to_ne_bytes());
				self.lower_expr(body, code);
				code.push(instr::JUMP);
				code.extend_from_slice(&0u32.to_ne_bytes());
				code.push(instr::END);
				code.push(instr::END);
			}
			If(condition, then, otherwise) => {
				self.lower_expr(condition, code);
				code.push(instr::IF_THEN);
				self.lower_expr(then, code);
				if let Some(otherwise) = otherwise {
					code.push(instr::ELSE);
					self.lower_expr(otherwise, code);
				}
				code.push(instr::END);
			}
			MethodCall(obj, method, args) => {
				self.lower_expr(obj, code);
				args.iter().for_each(|arg| self.lower_expr(arg, code));
				use hir::Type::*;
				match (&obj.typ, &method[..]) {
					(I32, "aboba") => todo!(),
					_ => panic!("Unknown method {} of {:?}", method, obj.typ),
				}
			}
			FnCall(function, args) => {
				args.iter().for_each(|arg| self.lower_expr(arg, code));
				match &function[..] {
					"println" => {
						// TODO: convert all arguments to strings and then
						// concat them all before calling imported functions
						assert!(args.len() == 1 && args[0].typ == hir::Type::Str);
						code.push(CALL_IMPORT);
						let signature = FuncType { parameters: vec![ValueType::I32], result: vec![ValueType::ExternRef] };
						let import = Import { signature, namespace: "std".into(), func_name: "string".into() };
						let f = self.program_lowerer.imports.insert_full(import).0;
						code.push(CALL_IMPORT);
						let signature = FuncType { parameters: vec![ValueType::ExternRef], result: vec![] };
						let import = Import { signature, namespace: "std".into(), func_name: "println".into() };
						let f = self.program_lowerer.imports.insert_full(import).0;
						code.extend_from_slice(&(f as u32).to_ne_bytes());
					}
					_ => {
						code.push(CALL_FUNC);
						let f = self.program_lowerer.queue.insert_full(function.clone()).0;
						code.extend_from_slice(&(f as u32).to_ne_bytes());
					}
				}
			}
			Return(value) => {
				self.lower_expr(value, code);
				code.push(instr::RETURN);
			}
			Unreachable => {
				code.push(instr::UNREACHABLE);
			}
		}
		println!(" :: {:X?}", code);
	}

	fn lower_block(&mut self, block: &hir::Block<hir::Type>, code: &mut Vec<Instr>) {
		self.locals_i32.push_scope();
		self.locals_f32.push_scope();
		for stmt in &block.stmts {
			match stmt {
				hir::Stmt::Expr(expr) => {
					self.lower_expr(expr, code);
					if expr.typ != hir::Type::Unit {
						code.push(instr::DROP);
					}
				}
				hir::Stmt::Def(variable, expr) => {
					self.lower_expr(expr, code);
					match expr.typ {
						hir::Type::Unit => {}
						hir::Type::F32 => {
							code.push(instr::F32_SET);
							code.extend_from_slice(&self.locals_f32.push(variable.clone()).to_ne_bytes());
						}
						_ => {
							code.push(instr::I32_SET);
							code.extend_from_slice(&self.locals_i32.push(variable.clone()).to_ne_bytes());
						}
					}
				}
			}
		}
		self.locals_i32.drop_scope();
		self.locals_f32.drop_scope();
	}
}

impl Locals {
	fn new() -> Self {
		Self {
			scopes: vec![],
			slots_count: 0,
			free_slots: vec![],
			used_slots: vec![],
			shadowed_vars: vec![],
			vars: HashMap::new(),
		}
	}

	fn push_scope(&mut self) {
		self.scopes.push((self.used_slots.len(), self.shadowed_vars.len()));
	}

	fn push(&mut self, variable: Rc<str>) -> u32 {
		let location = self.free_slots.pop().unwrap_or_else(|| {
			let free_slot = self.slots_count;
			self.slots_count += 1;
			free_slot
		});
		if let Some(shadowed_location) = self.vars.insert(variable.clone(), location) {
			self.shadowed_vars.push((variable, shadowed_location));
		}
		self.used_slots.push(location);
		location
	}

	fn find(&self, variable: &str) -> u32 {
		self.vars[variable]
	}

	fn drop_scope(&mut self) {
		let (slots, shadows) = self.scopes.pop().unwrap();
		while slots < self.used_slots.len() {
			self.free_slots.push(self.used_slots.pop().unwrap());
		}
		while shadows < self.shadowed_vars.len() {
			let (name, idx) = self.shadowed_vars.pop().unwrap();
			self.vars.insert(name, idx);
		}
	}

	fn len(&self) -> usize {
		self.slots_count as usize
	}
}
