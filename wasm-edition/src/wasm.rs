use indexmap::IndexMap;
macro_rules! check {
	($err:expr, $cond:expr) => {{
		if !$cond {
			return Err($err);
		}
	}};
	($err:expr) => {{
		return Err($err);
	}};
}

const WASM_BINARY_MAGIC: [u8; 4] = *b"\0asm";
const WASM_BINARY_VERSION: [u8; 4] = [1, 0, 0, 0];

pub struct Wasm {}

pub enum Op {
	I32Const(i32),
	I32Add,
	End,
}

// TODO: support indexes bigger than 128 using LEB128
impl Wasm {
	// TODO: use byte stream / iterator instead of slice
	pub fn from(data: &[u8]) -> Result<Self, &str> {
		let wasm = Wasm {};
		check!("file size", data.len() >= 40);
		check!("file type", data[0..4] == WASM_BINARY_MAGIC);
		check!("WASM version", data[4..8] == WASM_BINARY_VERSION);
		let mut i = 8;
		let mut types = vec![];
		let mut functions = vec![];
		let mut bodies = vec![];
		let mut exports = IndexMap::<String, usize>::new();
		while i + 3 <= data.len() {
			let section = SectionId::from(data[i])?;
			println!("Section {:?}", section);
			let size = data[i + 1] as usize;
			check!("file size", i + 2 + size <= data.len());
			println!("\tsize: {}", size);
			match section {
				SectionId::Type => {
					let mut j = i + 3;
					for _ in 0..data[i + 2] {
						let typ = FnType::from(data, &mut j)?;
						println!("\t{:?}", typ);
						types.push(typ);
					}
				}
				SectionId::Function => {
					let mut j = i + 3;
					for _ in 0..data[i + 2] {
						let f = data[j] as usize;
						println!("\t{:?}", types[f]);
						functions.push(data[j] as usize);
						j += 1;
					}
				}
				SectionId::Export => {
					let mut j = i + 3;
					for _ in 0..data[i + 2] {
						let str_len = data[j] as usize;
						let s = String::from_utf8(data[j + 1..j + 1 + str_len].to_vec());
						check!("function name", s.is_ok());
						let s = s.unwrap();
						let kind = data[j + str_len + 1];
						check!("function kind", kind == 0);
						let f = data[j + str_len + 2] as usize;
						j = j + 4 + str_len;
						println!("\tf#{} as {}", f, s);
						exports.insert(s, f);
					}
				}
				SectionId::Code => {
					let mut j = i + 3;
					for _ in 0..data[i + 2] {
						let body_size = data[j] as usize;
						let locals_len = data[j + 1] as usize;
						assert!(locals_len == 0);
						let locals: Vec<Type> = vec![];
						let mut i = j + 2 + locals_len;
						let mut body = vec![];
						use Op::*;
						print!("\t");
						while i < j + 1 + body_size {
							print!("{:02X} ", data[i]);
							body.push(match data[i] {
								0x41 => {
									let value = data[i + 1] as i32;
									i += 2;
									I32Const(value)
								}
								0x6a => {
									i += 1;
									I32Add
								}
								0x0b => {
									i += 1;
									End
								}
								_ => {
									println!();
									check!("opcode")
								}
							});
						}
						println!("");
						bodies.push((locals, body));
						j += 1 + body_size;
					}
				}
				_ => return Err("section: currently unsupported"),
			}
			i += 2 + size;
		}
		check!("file size", i == data.len());
		println!("{:?}", data);
		Ok(wasm)
	}
}

#[repr(u8)]
#[derive(Debug)]
enum SectionId {
	Custom = 0,
	Type = 1,
	Import = 2,
	Function = 3,
	Table = 4,
	Memory = 5,
	Global = 6,
	Export = 7,
	Start = 8,
	Element = 9,
	Code = 10,
	Data = 11,
	DataCount = 12,
}

impl SectionId {
	fn from(byte: u8) -> Result<Self, &'static str> {
		match byte {
			1 => Ok(Self::Type),
			2 => Err("section: Import section is not currently supported"),
			3 => Ok(Self::Function),
			4 => Err("section: Table section is not currently supported"),
			5 => Err("section: Memory section is not currently supported"),
			6 => Err("section: Global section is not currently supported"),
			7 => Ok(Self::Export),
			8 => Err("section: Start section is not currently supported"),
			9 => Err("section: Element section is not currently supported"),
			10 => Ok(Self::Code),
			11 => Err("section: Data section is not currently supported"),
			12 => Err("section: DataCount section is not currently supported"),
			_ => Err("section id"),
		}
	}
}

#[derive(Debug)]
struct FnType {
	params: Vec<Type>,
	results: Vec<Type>,
}

impl FnType {
	fn from(data: &[u8], offset: &mut usize) -> Result<Self, &'static str> {
		let i = *offset;
		check!("function type definition", data[i] == 96);
		let params_len = data[i + 1] as usize;
		let mut params = vec![];
		for j in 0..params_len {
			params[j] = Type::from(data[i + 1 + j])?;
		}
		let i = i + params_len;
		let results_len = data[i + 1] as usize;
		let mut results = vec![];
		for j in 0..results_len {
			results[j] = Type::from(data[i + 1 + j])?;
		}
		*offset = i + results_len + 2;
		Ok(FnType { params, results })
	}
}

#[repr(u8)]
#[derive(Debug)]
enum Type {
	I32 = 127,
}

impl Type {
	fn from(byte: u8) -> Result<Self, &'static str> {
		match byte {
			127 => Ok(Self::I32),
			_ => Err("type id"),
		}
	}
}
