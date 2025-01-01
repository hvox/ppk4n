use std::{env::args, fs, process::exit};

mod ppkn;

fn main() {
	let args: Vec<_> = args().collect();
	let source = fs::read_to_string(&args[1]).unwrap();

	todo!();
}
