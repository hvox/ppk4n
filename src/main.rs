use std::collections::HashMap;
use std::fs;
use std::env::args;
use std::path::PathBuf;
use std::process::exit;
use std::hash::Hash;
use std::hash::Hasher;
use std::rc::Rc;

use ppkn::{parse, PpknError};
use sppkn::hir::Program;
use utils::stringify_lossy;

mod ppkn;
mod pyppkn;
mod sppkn;
mod utils;

const USE_SPPKN_IMPLEMENTATION: bool = true;
const SPPKN_INNER_LOADER: bool = true;

fn main() {
	let args: Vec<_> = args().collect();
	if let Some(bytes) = get_packed_data() {
		let program = Program::from_bytes(&bytes);
		let Ok(program) = typecheck_program(program) else { return };
		let bytecode = program.to_lir();
		let mut runtime = bytecode.create_runtime();
		let status = runtime.run();
		if let Err(error) = status {
			println!("Exit code: {:?}", error);
		}
		_ = bytecode;
		return;
	}
	match args[1].as_str() {
		"into-python" => {
			let Ok(program) = load_program(&args[2]) else { return };
			print!("{}", program.to_python());
		}
		"into-rust" => {
			let Ok(program) = load_program(&args[2]) else { return };
			print!("{}", program.to_rust());
		}
		"into-wasm" => {
			let Ok(program) = load_program(&args[2]) else { return };
			let wasm = program.to_lir().to_wasm();
			fs::write(&args[3], &wasm).unwrap();
		}
		"into-exe" => {
			let Ok(program) = load_program(&args[2]) else { return };
			fs::write(&args[3], &pack_data(&program.to_bytes())).unwrap();
			#[cfg(target_family = "unix")]
			{
				use std::os::unix::fs::PermissionsExt;
				let mut perms = fs::metadata(&args[3]).unwrap().permissions();
				perms.set_mode(perms.mode() | 0o111);
				let _ = fs::set_permissions(&args[3], perms);
			}
		}
		_ => {
			if USE_SPPKN_IMPLEMENTATION {
				// println!("LIR Instruction size: {:?}", size_of::<sppkn::lir::Instr>());

				// eprintln!("\tSource path = {:?}", program.root);
				// eprintln!("\t{:?}", program.sources);
				// eprintln!("\t{:?}", program.modules);
				// eprintln!("\tGlobals: {:?}", program.globals);
				// eprintln!("Functions:");
				// for (name, func) in &program.functions {
				// 	eprintln!("    {}: {:?}", name, func);
				// }

				let Ok(program) = load_program(&args[1]) else {
					return;
				};
				let bytecode = program.to_lir();
				let mut runtime = bytecode.create_runtime();
				let status = runtime.run();
				if let Err(error) = status {
					println!("Exit code: {:?}", error);
				}
				_ = bytecode;
			} else {
				let source = fs::read_to_string(&args[1]).unwrap() + include_str!("ppkn/std.ppkn");
				let lir = ppkn::to_lir(&source).unwrap();
				let mut intrerpreter = ppkn::yalir_interpreter::Interpreter::new(&lir);
				intrerpreter.interpret("main").unwrap();
				// checked(&source, run(&source));
			}
		}
	}
}

fn load_program(path: &str) -> Result<Program, ()> {
	let path = PathBuf::from(path);
	let source = fs::read_to_string(&path).unwrap();

	if !SPPKN_INNER_LOADER {
		use sppkn::loader;
		let loader = loader::load(path.parent().unwrap().into(), "main".into(), source.clone().into());
		if false {
			for (name, module) in loader.modules {
				println!("\t{:?} : {:?}", name, module);
			}
			println!("\nERRORS: {:?}", loader.errors);
		}
	}

	let entry_path = Rc::<str>::from(path.file_stem().unwrap().to_string_lossy());
	let src_root = path.parent().unwrap();
	let sources = HashMap::from([(entry_path.clone(), source.into())]);
	let program = sppkn::hir::Program::new(src_root, sources, entry_path.clone());
	typecheck_program(program)
}

fn typecheck_program(mut program: Program) -> Result<Program, ()> {
	if let Err(errors) = program.load_and_typecheck(program.main.clone()) {
		let skip_panics = false;
		let mut last_error = (0, 0);
		for error in errors {
			let source = &program.sources[&error.module];
			let (start, end) = error.cause_location;
			let mut line_no = 1;
			let mut line_start = 0;
			for (i, _) in source.char_indices().filter(|(_, ch)| *ch == '\n') {
				if i + 1 > start as _ {
					break;
				}
				line_start = i + 1;
				line_no += 1;
			}
			let column = start as usize + 1 - line_start;
			if skip_panics && (line_no, column) == last_error {
				continue;
			} else {
				last_error = (line_no, column);
			}
			let line = source[line_start..].lines().next().unwrap();
			let path = error.module; // TODO
			println!("{:?} at {}:{}:{}", error.kind, path, line_no, column);
			println!("{}", line.replace("\t", " "));
			println!("{}{} {}", " ".repeat(column - 1), "^".repeat((end - start) as _), error.message);
		}
		return Err(());
	};
	Ok(program)
}

fn get_packed_data() -> Option<Vec<u8>> {
	let bytes = fs::read(std::env::current_exe().unwrap()).unwrap();
	let mut hasher = std::hash::DefaultHasher::new();
	bytes[..bytes.len() - 8].hash(&mut hasher);
	let hash = u64::from_le_bytes(bytes[bytes.len() - 8..].try_into().unwrap());
	if hasher.finish() != hash {
		return None;
	}
	let size = u32::from_le_bytes(bytes[bytes.len() - 12..bytes.len() - 8].try_into().unwrap()) as usize;
	Some(bytes[bytes.len() - 12 - size..bytes.len() - 12].to_vec())
}

fn pack_data(data: &[u8]) -> Vec<u8> {
	let mut bytes = fs::read(std::env::current_exe().unwrap()).unwrap();
	bytes.extend(data);
	bytes.extend((data.len() as u32).to_le_bytes());
	let mut hasher = std::hash::DefaultHasher::new();
	bytes[..].hash(&mut hasher);
	bytes.extend(hasher.finish().to_le_bytes());
	bytes
}

fn checked<'a, T>(source: &'a str, result: Result<T, impl Into<PpknError<'a>>>) -> T {
	match result {
		Ok(something) => something,
		Err(error) => {
			let error = error.into();
			let (row, column, line) = find_line_with(&source, error.location);
			println!("{:3}: {}", row, line.replace("\t", " "));
			println!("    {}{}", " ".repeat(column), "^".repeat(error.location.len()));
			println!("{}: {}", error.typ, error.message);
			exit(1);
		}
	}
}

fn find_line_with<'a>(text: &'a str, content: &str) -> (usize, usize, &'a str) {
	let location = content.as_ptr() as usize - text.as_ptr() as usize;
	let mut line_number = 1;
	let mut line_start = 0;
	let mut line_end = text.len();
	let mut found_line = false;
	for (i, ch) in text.char_indices() {
		match found_line {
			false => {
				found_line |= i == location;
				if ch == '\n' {
					line_start = i + ch.len_utf8();
					line_number += 1;
				}
			}
			true => {
				if ch == '\n' {
					line_end = i;
					break;
				}
			}
		}
	}
	(line_number, 1 + location - line_start, &text[line_start..line_end])
}
