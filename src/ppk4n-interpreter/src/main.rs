// #![allow(unused)]
mod scanner;
mod utils;

fn main() -> Result<(), std::io::Error> {
	let args: Vec<String> = std::env::args().collect();
	let source = if args.len() != 2 {
		println!("You are invalid btw");
		let source = "\"hi\" + \"hello there\"\n\thi\n# bibik";
		let pretty_source = source.replace("\t", "→   ").replace("\n", "\n  ");
		println!("Testing source:\n  {pretty_source}");
		source
	} else {
		&std::fs::read_to_string(&args[1])?
	};

	let tokens = scanner::scan_tokens(&source);

	println!("Tokens:");
	for token in tokens {
		let i = token.as_ptr() as usize - source.as_ptr() as usize;
		println!("\t[{}..{}] = {}", i, i + token.len(), token);
	}
	Ok(())
}
