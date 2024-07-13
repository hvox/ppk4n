#[allow(dead_code, unused_variables, unused_mut, unused_imports)]
mod scanner;
mod utils;

fn main() -> Result<(), std::io::Error> {
	let args: Vec<String> = std::env::args().collect();
	let source = if args.len() != 2 {
		println!("You are invalid btw");
		"+++-#nthar"
	} else {
		&std::fs::read_to_string(&args[1])?
	};
	let (tokens, errors) = scanner::scan_tokens(&source);
	for error in errors {
		let (line, column) = utils::get_grapheme_position(&source, error.position);
		println!("ERROR: {}", source.lines().nth(line).unwrap());
		println!("       {}^ unexpected character", " ".to_string().repeat(column));
	}
	for token in tokens {
		println!("{:?}", token.typ);
	}
	Ok(())
}
