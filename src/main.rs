use std::{env::args, fs, process::exit};

use ppkn::run;

mod ppkn;
mod utils;

fn main() {
	let args: Vec<_> = args().collect();
	let source = fs::read_to_string(&args[1]).unwrap();

	match run(&source) {
		Ok(program) => program,
		Err(error) => {
			let (row, column, line) = find_line_with(&source, error.location);
			println!("{:3}: {}", row, line.replace("\t", " "));
			println!("    {}{}", " ".repeat(column), "^".repeat(error.location.len()));
			println!("{}: {}", error.typ, error.message);
			exit(1);
		}
	};
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
