#![allow(dead_code)]
#![allow(unused_mut)]
#![allow(unused_variables)]
#![allow(unused_assignments)]
mod wasm2;
mod leb128;
use std::io::IsTerminal;
use wasm2::Wasm;

fn main() {
	let mut data: Vec<u8> = vec![];
	data.extend(include_bytes!("minimal.wasm"));
	match Wasm::read(&*data) {
		Ok(wasm) => {
			wasm.instantiate().call("main".to_string());
		},
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
