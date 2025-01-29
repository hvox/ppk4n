#![allow(unused)]
use std::collections::HashSet;

use super::mir::*;

fn transpile_type(typ: &Type) -> String {
	use Type::*;
	match typ {
		Unit => "None".into(),
		Bool => "bool".into(),
		I64 => "int".into(),
		U64 => "int".into(),
		F64 => "float".into(),
		Str => "str".into(),
		Vec(t) => format!("List[{}]", transpile_type(&t)),
	}
}

impl<'a> Program<'a> {
	pub fn transpile_to_python(&self) -> String {
		let mut host_language_dependencies = HashSet::new();
		let mut python_program = "".to_string();

		for f in &self.functions {
			let params = f
				.params
				.iter()
				.map(|(name, typ)| format!("{}: {}", name, transpile_type(&typ)))
				.collect::<Vec<_>>()
				.join(", ");
			let result = transpile_type(&f.result);
			python_program.push_str(&format!("def {}({}) -> {}:\n", f.name, params, result));

			// TODO: fix bug of Set instead of Def
			let mut transpiler = FunctionTranspiler::new(self, f);
			for stmt in &f.body {
				let line = format!("    {}\n", transpiler.transpile_unit(stmt));
				python_program.push_str(&line);
			}
			host_language_dependencies.extend(transpiler.deps);
			python_program.push_str("\n");
		}

		if self.functions.iter().map(|f| f.name.clone()).any(|name| &*name == "main") {
			python_program += "if __name__ == \"__main__\":\n";
			python_program += "    main()\n";
		}

		if !host_language_dependencies.is_empty() {
			let mut deps = host_language_dependencies
				.into_iter()
				.map(|(path, name)| format!("from {path} import {name}\n"))
				.collect::<Vec<_>>();
			deps.sort();
			python_program = deps.join("") + "\n" + &python_program;
		}

		python_program
	}
}

struct FunctionTranspiler<'a, 'b> {
	f: &'b Function<'a, ()>,
	fns: &'b Vec<Function<'a, ()>>,
	deps: HashSet<(&'static str, &'static str)>,
}

impl<'a, 'b> FunctionTranspiler<'a, 'b> {
	fn new(program: &'b Program<'a>, function: &'b Function<'a, ()>) -> Self {
		Self { f: function, fns: &program.functions, deps: HashSet::new() }
	}

	fn transpile_all(&mut self, vec: &Vec<Instr>) -> String {
		vec.into_iter().map(|instr| self.transpile_instr(instr)).collect::<Vec<_>>().join(", ")
	}

	fn transpile_instr(&mut self, instr: &Instr) -> String {
		use InstrKind::*;
		match &instr.kind {
			Cntrl(instr_unit) => self.transpile_unit(instr_unit),
			Bool(instr_bool) => todo!(),
			I64(instr_i64) => self.transpile_i64(instr_i64),
			U64(instr_u64) => self.transpile_u64(instr_u64),
			F64(instr_f64) => self.transpile_f64(instr_f64),
			Str(instr_str) => self.transpile_str(instr_str),
			Vec(instr_vec, _) => todo!(),
		}
	}

	fn transpile_unit(&mut self, instr: &InstrCntrl) -> String {
		use InstrKindCntrl::*;
		match &*instr.kind {
			Call(func, args) => format!("{}({})", self.fns[*func].name, self.transpile_all(args)),
			DefI64(var, instr) => format!("{}: int = {}", self.f.locals[*var].0, self.transpile_i64(instr)),
			DefU64(var, instr) => format!("{}: int = {}", self.f.locals[*var].0, self.transpile_u64(instr)),
			DefF64(var, instr) => format!("{}: float = {}", self.f.locals[*var].0, self.transpile_f64(instr)),
			DefStr(var, instr) => format!("{}: str = {}", self.f.locals[*var].0, self.transpile_str(instr)),
			DefBool(_, _) => todo!(),
			DefVec(var, instr, typ) => {
				self.deps.insert(("typing", "List"));
				format!(
					"{}: List[{}] = {}",
					self.f.locals[*var].0,
					transpile_type(typ),
					self.transpile_vec(instr)
				)
			}
			SetI64(var, instr) => format!("{} = {}", self.f.locals[*var].0, self.transpile_i64(instr)),
			SetU64(var, instr) => format!("{} = {}", self.f.locals[*var].0, self.transpile_u64(instr)),
			SetF64(var, instr) => format!("{} = {}", self.f.locals[*var].0, self.transpile_f64(instr)),
			SetStr(var, instr) => format!("{} = {}", self.f.locals[*var].0, self.transpile_str(instr)),
			SetBool(_, _) => todo!(),
			SetVec(_) => todo!(),
			PrintStr(instr) => match &*instr.kind {
				InstrKindStr::CastI64(instr) => format!("print({}, end=\"\")", self.transpile_i64(instr)),
				InstrKindStr::CastF64(instr) => format!("print({}, end=\"\")", self.transpile_f64(instr)),
				InstrKindStr::CastVec(instr, _) => format!("print({}, end=\"\")", self.transpile_vec(instr)),
				_ => format!("print({}, end=\"\")", self.transpile_str(instr)),
			},
			PrintlnStr(instr) => match &*instr.kind {
				InstrKindStr::CastI64(instr) => format!("print({})", self.transpile_i64(instr)),
				InstrKindStr::CastF64(instr) => format!("print({})", self.transpile_f64(instr)),
				InstrKindStr::CastVec(instr, _) => format!("print({})", self.transpile_vec(instr)),
				_ => format!("print({})", self.transpile_str(instr)),
			},
			While(instr_bool, vec) => todo!(),
			Block(vec) => todo!(),
			Return(instr) => todo!(),
			Drop(instr) => todo!(),
			Push(var, instr) => format!("{}.append({})", self.f.locals[*var].0, self.transpile_instr(instr)),
		}
	}

	fn transpile_i64(&mut self, instr: &InstrI64) -> String {
		use InstrKindI64::*;
		match &*instr.kind {
			Add(lhs, rhs) => format!("{} + {}", self.transpile_i64(lhs), self.transpile_i64(rhs)),
			Sub(lhs, rhs) => format!("{} - {}", self.transpile_i64(lhs), self.transpile_i64(rhs)),
			Mult(lhs, rhs) => format!("{} * {}", self.transpile_i64(lhs), self.transpile_i64(rhs)),
			Div(lhs, rhs) => format!("{} // {}", self.transpile_i64(lhs), self.transpile_i64(rhs)),
			Rem(lhs, rhs) => format!("{} % {}", self.transpile_i64(lhs), self.transpile_i64(rhs)),
			And(lhs, rhs) => format!("{} & {}", self.transpile_i64(lhs), self.transpile_i64(rhs)),
			Xor(lhs, rhs) => format!("{} ^ {}", self.transpile_i64(lhs), self.transpile_i64(rhs)),
			Or(lhs, rhs) => format!("{} | {}", self.transpile_i64(lhs), self.transpile_i64(rhs)),
			Return(instr) => format!("return {}", self.transpile_instr(instr)),
			Variable(var) => self.f.locals[*var].0.to_string(),
			Call(_) => todo!(),
			Value(value) => value.to_string(),
		}
	}

	fn transpile_u64(&mut self, instr: &InstrU64) -> String {
		use InstrKindU64::*;
		match &*instr.kind {
			Add(lhs, rhs) => format!("{} + {}", self.transpile_u64(lhs), self.transpile_u64(rhs)),
			Sub(lhs, rhs) => format!("{} - {}", self.transpile_u64(lhs), self.transpile_u64(rhs)),
			Mult(lhs, rhs) => format!("{} * {}", self.transpile_u64(lhs), self.transpile_u64(rhs)),
			Div(lhs, rhs) => format!("{} // {}", self.transpile_u64(lhs), self.transpile_u64(rhs)),
			Rem(lhs, rhs) => format!("{} % {}", self.transpile_u64(lhs), self.transpile_u64(rhs)),
			And(lhs, rhs) => format!("{} & {}", self.transpile_u64(lhs), self.transpile_u64(rhs)),
			Xor(lhs, rhs) => format!("{} ^ {}", self.transpile_u64(lhs), self.transpile_u64(rhs)),
			Or(lhs, rhs) => format!("{} | {}", self.transpile_u64(lhs), self.transpile_u64(rhs)),
			Return(instr) => format!("return {}", self.transpile_instr(&instr)),
			Variable(var) => self.f.locals[*var].0.to_string(),
			Call(_) => todo!(),
			Value(value) => value.to_string(),
		}
	}

	fn transpile_f64(&mut self, instr: &InstrF64) -> String {
		use InstrKindF64::*;
		match &*instr.kind {
			Add(lhs, rhs) => format!("{} + {}", self.transpile_f64(lhs), self.transpile_f64(rhs)),
			Sub(lhs, rhs) => format!("{} - {}", self.transpile_f64(lhs), self.transpile_f64(rhs)),
			Mult(lhs, rhs) => format!("{} * {}", self.transpile_f64(lhs), self.transpile_f64(rhs)),
			Div(lhs, rhs) => format!("{} / {}", self.transpile_f64(lhs), self.transpile_f64(rhs)),
			Return(instr) => format!("return {}", self.transpile_instr(&instr)),
			Variable(var) => self.f.locals[*var].0.to_string(),
			Call(_) => todo!(),
			Value(value) => value.to_string(),
		}
	}

	fn transpile_str(&mut self, instr: &InstrStr) -> String {
		use InstrKindStr::*;
		match &*instr.kind {
			Add(lhs, rhs) => format!("{} + {}", self.transpile_str(lhs), self.transpile_str(rhs)),
			Return(instr) => format!("return {}", self.transpile_instr(&instr)),
			Variable(var) => self.f.locals[*var].0.to_string(),
			Call(_) => todo!(),
			Value(value) => format!("{:?}", value),
			CastI64(instr) => format!("str({})", self.transpile_i64(instr)),
			CastF64(instr) => format!("str({})", self.transpile_f64(instr)),
			CastVec(instr, _) => todo!(),
		}
	}

	fn transpile_vec(&mut self, instr: &InstrVec) -> String {
		use InstrKindVec::*;
		match &*instr.kind {
			Return(instr) => format!("return {}", self.transpile_instr(&instr)),
			Variable(var) => self.f.locals[*var].0.to_string(),
			Call(_) => todo!(),
			Value(_) => todo!(),
			Empty => "[]".into(),
		}
	}
}
