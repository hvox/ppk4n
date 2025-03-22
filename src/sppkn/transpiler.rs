use super::hir::{Instr, InstrKind, Program, Type};

impl Program {
	pub fn to_python(&self) -> String {
		let mut code = String::new();
		for (fname, f) in &self.functions {
			if f.module.as_ref() == "std" {
				continue;
			}
			let fname = transpile_fname(fname);
			code.push_str(&format!("def {}(", fname));
			for (i, (name, typ)) in f.parameters.iter().enumerate() {
				code.push_str(&format!("{}: {}", name, transpile_type(&typ)));
				if i != f.parameters.len() - 1 {
					code.push_str(", ");
				}
			}
			code.push_str(&format!(") -> {}:\n", transpile_type(&f.result)));
			let body = transpile_instr(&f.body.value).lines().map(|line| format!("    {}\n", line)).collect::<Vec<_>>().join("");
			code.push_str(&body);
			code.push_str("\n\n");
		}
		code.push_str("if __name__ == \"__main__\":\n");
		code.push_str(&format!("    {}_main()\n", transpile_fname(&self.main)));
		code
	}

	pub fn to_rust(&self) -> String {
		todo!()
	}
}

fn transpile_instr(instr: &Instr) -> String {
	use InstrKind::*;
	match &instr.kind {
		String(string) => format!("{:?}", string),
		Integer(number) => number.to_string(),
		Identifier(id) => id.to_string(),
		Tuple(fields) => "(".to_string() + &fields.iter().map(|x| transpile_instr(x)).collect::<Vec<_>>().join(", ") + ")",
		Assignment(variable, value) => format!("{} = {}", variable, transpile_instr(value)),
		GetLocal(id) => id.to_string(),
		Block(block) => {
			let mut code = "".to_string();
			for (id, value) in &block.stmts {
				if let Some((id, _mutable)) = id {
					code.push_str(&format!("{} = ", id));
				}
				code.push_str(&format!("{}", transpile_instr(value)));
				if !code.ends_with("\n") {
					code.push_str("\n");
				}
			}
			if block.result.kind != NoOp {
				code.push_str(&format!("return {}\n", transpile_instr(&block.result)));
			}
			code
		}
		While(condition, body) => {
			let body = transpile_instr(body).lines().map(|line| format!("    {}\n", line)).collect::<Vec<_>>().join("");
			format!("while {}:\n{}", transpile_instr(condition), body)
		}
		If(condition, then, otherwise) => {
			let then = transpile_instr(then).lines().map(|line| format!("    {}\n", line)).collect::<Vec<_>>().join("");
			let els = transpile_instr(otherwise).lines().map(|line| format!("    {}\n", line)).collect::<Vec<_>>().join("");
			format!("if {}:\n{}else:\n{}", transpile_instr(condition), then, els)
		}
		MethodCall(receiver, method, args) => {
			let args = &args.iter().map(|x| transpile_instr(x)).collect::<Vec<_>>().join(", ");
			match method.as_ref() {
				"add" => format!("{} + {}", transpile_instr(receiver), args),
				"sub" => format!("{} - {}", transpile_instr(receiver), args),
				"rem" => format!("{} % {}", transpile_instr(receiver), args),
				"eq" => format!("{} == {}", transpile_instr(receiver), args),
				"gt" => format!("{} > {}", transpile_instr(receiver), args),
				_ => format!("{}.{}({})", transpile_instr(receiver), method, args),
			}
		}
		FnCall(func, args) => {
			let args = &args.iter().map(|x| transpile_instr(x)).collect::<Vec<_>>().join(", ");
			format!("{}({})", transpile_fname(func), args)
		}
		Return(value) => format!("return {}", transpile_instr(value)),
		Unreachable => "raise NotImplementedError()".to_string(),
		NoOp => "pass".to_string(),
	}
}

fn transpile_type(typ: &Type) -> String {
	match typ {
		Type::Tuple(_) => todo!(),
		Type::Array(element) => format!("List[{}]", transpile_type(element)),
		Type::Name(name) => match name.as_ref() {
			"i8" | "u8" | "i16" | "u16" | "i32" | "u32" | "i64" | "u64" => "int".to_string(),
			"f32" | "f64" => "float".to_string(),
			_ => format!("{}", name),
		},
		Type::Void => "None".to_string(),
	}
}

fn transpile_fname(fname: &str) -> String {
	match fname {
		"println" => "print".to_string(),
		_ => fname.replace(':', "_"),
	}
}
