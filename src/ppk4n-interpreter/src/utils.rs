pub fn get_grapheme_position(string: &str, byte_position: usize) -> (usize, usize) {
	let mut lines = 0;
	let mut line_start = 0;
	for i in 0..byte_position {
		if string.as_bytes()[i] == b'\n' {
			line_start = i;
			lines += 1;
		}
	}
	let columns = string[line_start..byte_position].chars().count();
	(lines, columns)
}
