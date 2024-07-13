use rustpython_parser::{ast, Parse};
use std::{env, process};

fn main() {
	let args: Vec<String> = env::args().collect();
	if args.len() != 2 {
		eprintln!("Usage: {} PATH", args[0]);
		process::exit(1);
	}
	let source = "prirnt('Hello world')\nx=2+2";
	let ast = ast::Suite::parse(source, "main").unwrap();
	let output = translate_ast(ast);
	for line in output {
		println!("{}", line);
	}
}

fn translate_ast(source: Vec<ast::Stmt>) -> Vec<String> {
	for stmt in source {
		match stmt {
			ast::Stmt::FunctionDef(_) => todo!(),
			ast::Stmt::AsyncFunctionDef(_) => todo!(),
			ast::Stmt::ClassDef(_) => todo!(),
			ast::Stmt::Return(_) => todo!(),
			ast::Stmt::Delete(_) => todo!(),
			ast::Stmt::Assign(_) => todo!(),
			ast::Stmt::TypeAlias(_) => todo!(),
			ast::Stmt::AugAssign(_) => todo!(),
			ast::Stmt::AnnAssign(_) => todo!(),
			ast::Stmt::For(_) => todo!(),
			ast::Stmt::AsyncFor(_) => todo!(),
			ast::Stmt::While(_) => todo!(),
			ast::Stmt::If(_) => todo!(),
			ast::Stmt::With(_) => todo!(),
			ast::Stmt::AsyncWith(_) => todo!(),
			ast::Stmt::Match(_) => todo!(),
			ast::Stmt::Raise(_) => todo!(),
			ast::Stmt::Try(_) => todo!(),
			ast::Stmt::TryStar(_) => todo!(),
			ast::Stmt::Assert(_) => todo!(),
			ast::Stmt::Import(_) => todo!(),
			ast::Stmt::ImportFrom(_) => todo!(),
			ast::Stmt::Global(_) => todo!(),
			ast::Stmt::Nonlocal(_) => todo!(),
			ast::Stmt::Expr(_) => todo!(),
			ast::Stmt::Pass(_) => todo!(),
			ast::Stmt::Break(_) => todo!(),
			ast::Stmt::Continue(_) => todo!(),
		}
	}
	vec![]
}
