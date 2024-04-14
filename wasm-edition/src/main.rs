#![allow(dead_code)]
#![allow(unused_mut)]
#![allow(unused_variables)]
#![allow(unused_assignments)]
mod leb128;
mod wasm2;
use std::{collections::HashMap, io::IsTerminal};
use wasm2::{runtime::Value, Wasm};

fn aboba(x: Vec<Value>) -> Vec<Value> {
	println!("{:?}", x);
	vec![]
}

fn main() {
	let mut data: Vec<u8> = vec![];
	data.extend(include_bytes!("minimal.wasm"));
	let f: &dyn Fn(Vec<Value>) -> Vec<Value> = &aboba;
	let environment =
		HashMap::from([("imports".to_string(), HashMap::from([("log".to_string(), f)]))]);

	match Wasm::read(&*data) {
		Ok(wasm) => {
			wasm.instantiate(environment).call("main".to_string());
		}
		Err(cause) => {
			let prefix = "Failed to parse WASM file because of";
			if std::io::stdout().is_terminal() {
				eprintln!("\x1b[91m{} \x1b[93mwrong {:?}\x1b[91m.\x1b[0m", prefix, cause);
			} else {
				eprintln!("{} {:?}.", prefix, cause);
			}
		}
	}
}
