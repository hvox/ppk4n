#![allow(dead_code)]
#![allow(unused_mut)]
#![allow(unused_variables)]
mod wasm;
use std::io::IsTerminal;
use wasm::Wasm;

fn main() {
	match Wasm::from(include_bytes!("minimal.wasm")) {
		Ok(_) => (),
		Err(cause) => {
			let prefix = "Failed to parse WASM file because of";
			if std::io::stdout().is_terminal() {
				eprintln!("\x1b[91m{} \x1b[93mwrong {}\x1b[91m.\x1b[0m", prefix, cause);
			} else {
				eprintln!("{} {}.", prefix, cause);
			}
		}
	}
}
