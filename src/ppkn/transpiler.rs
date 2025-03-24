use super::hir::{Instr, InstrKind, Program, Type};

impl Program {
    pub fn to_python(&self) -> String {
        use python::*;
        let mut code = String::new();
        for (fname, f) in &self.functions {
            if f.module.as_ref() == "std" {
                continue;
            }
            let fname = transpile_fname(fname);
            code.push_str(&format!("def {}(", fname));
            for (i, (name, typ)) in f.parameters.iter().enumerate() {
                code.push_str(&format!("{}: {}", name, transpile_type(typ)));
                if i != f.parameters.len() - 1 {
                    code.push_str(", ");
                }
            }
            code.push_str(&format!(") -> {}:\n", transpile_type(&f.result)));
            let body = transpile_instr(&f.body.value)
                .lines()
                .map(|line| format!("    {}\n", line))
                .collect::<Vec<_>>()
                .join("");
            code.push_str(&body);
            code.push_str("\n\n");
        }
        code.push_str("if __name__ == \"__main__\":\n");
        code.push_str(&format!("    {}_main()\n", transpile_fname(&self.main)));
        code
    }

    pub fn to_rust(&self) -> String {
        use rust::*;
        let mut code = String::new();
        for (fname, f) in &self.functions {
            if f.module.as_ref() == "std" {
                continue;
            }
            let fname = transpile_fname(fname);
            code.push_str(&format!("fn {}(", fname));
            for (i, (name, typ)) in f.parameters.iter().enumerate() {
                code.push_str(&format!("{}: {}", name, transpile_type(typ)));
                if i != f.parameters.len() - 1 {
                    code.push_str(", ");
                }
            }
            code.push_str(&format!(") -> {} ", transpile_type(&f.result)));
            code.push_str("{\n");
            let body = transpile_instr(&f.body.value)
                .lines()
                .map(|line| format!("    {}\n", line))
                .collect::<Vec<_>>()
                .join("");
            code.push_str(&body);
            code.push_str("}\n\n");
        }
        code.push_str("fn main() {\n");
        code.push_str(&format!("    {}_main();\n", transpile_fname(&self.main)));
        code.push_str("}\n");
        code
    }

    pub fn to_zig(&self) -> String {
        unimplemented!("Zig is not stable enough to be target of transpilation")
    }
}

mod python {
    use super::*;
    pub fn transpile_instr(instr: &Instr) -> String {
        use InstrKind::*;
        match &instr.kind {
            String(string) => format!("{:?}", string),
            Integer(number) => number.to_string(),
            Identifier(id) => id.to_string(),
            Tuple(fields) => {
                "(".to_string()
                    + &fields.iter().map(transpile_instr).collect::<Vec<_>>().join(", ")
                    + ")"
            }
            Assignment(variable, value) => format!("{} = {}", variable, transpile_instr(value)),
            Block(block) => {
                let mut code = "".to_string();
                for (id, value) in &block.stmts {
                    if let Some((id, _mutable)) = id {
                        code.push_str(&format!("{} = ", id));
                    }
                    code.push_str(&transpile_instr(value).to_string());
                    if !code.ends_with("\n") {
                        code.push('\n');
                    }
                }
                if block.result.kind != NoOp {
                    code.push_str(&format!("return {}\n", transpile_instr(&block.result)));
                }
                code
            }
            While(condition, body) => {
                let body = transpile_instr(body)
                    .lines()
                    .map(|line| format!("    {}\n", line))
                    .collect::<Vec<_>>()
                    .join("");
                format!("while {}:\n{}", transpile_instr(condition), body)
            }
            If(condition, then, otherwise) => {
                let then = transpile_instr(then)
                    .lines()
                    .map(|line| format!("    {}\n", line))
                    .collect::<Vec<_>>()
                    .join("");
                let els = transpile_instr(otherwise)
                    .lines()
                    .map(|line| format!("    {}\n", line))
                    .collect::<Vec<_>>()
                    .join("");
                format!("if {}:\n{}else:\n{}", transpile_instr(condition), then, els)
            }
            MethodCall(receiver, method, args) => {
                let args = &args.iter().map(transpile_instr).collect::<Vec<_>>().join(", ");
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
                let args = &args.iter().map(transpile_instr).collect::<Vec<_>>().join(", ");
                format!("{}({})", transpile_fname(func), args)
            }
            Return(value) => format!("return {}", transpile_instr(value)),
            Unreachable => "raise NotImplementedError()".to_string(),
            NoOp => "pass".to_string(),
        }
    }

    pub fn transpile_type(typ: &Type) -> String {
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

    pub fn transpile_fname(fname: &str) -> String {
        match fname {
            "println" => "print".to_string(),
            _ => fname.replace(':', "_"),
        }
    }
}

mod rust {
    use super::*;
    pub fn transpile_instr(instr: &Instr) -> String {
        use InstrKind::*;
        match &instr.kind {
            String(string) => format!("{:?}", string),
            Integer(number) => number.to_string(),
            Identifier(id) => id.to_string(),
            Tuple(fields) => {
                "(".to_string()
                    + &fields.iter().map(transpile_instr).collect::<Vec<_>>().join(", ")
                    + ")"
            }
            Assignment(variable, value) => format!("{} = {}", variable, transpile_instr(value)),
            Block(block) => {
                let mut code = "".to_string();
                for (id, value) in &block.stmts {
                    if let Some((id, _mutable)) = id {
                        code.push_str(&format!("let {} = ", id));
                    }
                    code.push_str(&transpile_instr(value).to_string());
                    if !code.ends_with("\n") {
                        code.push_str(";\n");
                    }
                }
                if block.result.kind != NoOp {
                    code.push_str(&format!("{}\n", transpile_instr(&block.result)));
                }
                code
            }
            While(condition, body) => {
                let body = transpile_instr(body)
                    .lines()
                    .map(|line| format!("    {}\n", line))
                    .collect::<Vec<_>>()
                    .join("");
                let mut code = format!("while {}", transpile_instr(condition));
                code.push_str(" {\n");
                code.push_str(&body);
                code.push_str("}\n");
                code
            }
            If(condition, then, otherwise) => {
                let then = transpile_instr(then)
                    .lines()
                    .map(|line| format!("    {}\n", line))
                    .collect::<Vec<_>>()
                    .join("");
                let els = transpile_instr(otherwise)
                    .lines()
                    .map(|line| format!("    {}\n", line))
                    .collect::<Vec<_>>()
                    .join("");
                let mut code = format!("if {}", transpile_instr(condition));
                code.push_str(" {\n");
                code.push_str(&then);
                code.push_str("} else {\n");
                code.push_str(&els);
                code.push('}');
                code
            }
            MethodCall(receiver, method, args) => {
                let args = &args.iter().map(transpile_instr).collect::<Vec<_>>().join(", ");
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
                let args_str = &args.iter().map(transpile_instr).collect::<Vec<_>>().join(", ");
                match func.as_ref() {
                    "println" => format!("println!(\"{}\", {})", "{}".repeat(args.len()), args_str),
                    _ => format!("{}({})", transpile_fname(func), args_str),
                }
            }
            Return(value) => format!("return {}", transpile_instr(value)),
            Unreachable => "unreachable!()".to_string(),
            NoOp => "()".to_string(),
        }
    }

    pub fn transpile_type(typ: &Type) -> String {
        match typ {
            Type::Tuple(_) => todo!(),
            Type::Array(element) => format!("List[{}]", transpile_type(element)),
            Type::Name(name) => format!("{}", name),
            Type::Void => "()".to_string(),
        }
    }

    pub fn transpile_fname(fname: &str) -> String {
        fname.replace(':', "_")
    }
}
