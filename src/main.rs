use std::{
	env::args,
	fs,
	hash::{Hash, Hasher},
	process::exit,
};

use ppkn::{parse, run, PpknError};
use utils::stringify_lossy;

mod ppkn;
mod utils;

fn main() {
	let args: Vec<_> = args().collect();
	if let Some(data) = get_packed_data() {
		let source = String::from_utf8(data).unwrap();
		return checked(&source, run(&source));
	}
	match args[1].as_str() {
		"into-python" => {
			let source = fs::read_to_string(&args[2]).unwrap();
			let program = checked(&source, parse(&source));
			print!("{}", program.transpile_to_python());
		}
		"into-wasm" => {
			let source = fs::read_to_string(&args[2]).unwrap();
			let program = checked(&source, parse(&source));
			println!("{}", stringify_lossy(&program.compile_to_wasm()));
		}
		"into-exe" => {
			let source = fs::read_to_string(&args[2]).unwrap();
			checked(&source, parse(&source));
			fs::write(&args[3], &pack_data(source.as_bytes())).unwrap();
			#[cfg(target_family = "unix")]
			{
				use std::os::unix::fs::PermissionsExt;
				let mut perms = fs::metadata(&args[3]).unwrap().permissions();
				perms.set_mode(perms.mode() | 0o111);
				let _ = fs::set_permissions(&args[3], perms);
			}
		}
		_ => {
			// let source = fs::read_to_string(&args[1]).unwrap() + include_str!("ppkn/std.ppkn");
			// let lir = ppkn::to_lir(&source).unwrap();
			// let mut intrerpreter = ppkn::yalir_interpreter::Interpreter::new(&lir);
			// intrerpreter.interpret("main").unwrap();
			let source = fs::read_to_string(&args[1]).unwrap();
			checked(&source, run(&source));
		}
	}
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
