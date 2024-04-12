#[derive(Debug)]
pub enum Error {
	FileType,
	Corrupted,
	Instructions,
	UnsupportedSection(u8),
}
