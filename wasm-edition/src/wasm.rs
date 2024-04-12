pub struct Wasm {}

const WASM_BINARY_MAGIC: [u8; 4] = *b"\0asm";
const WASM_BINARY_VERSION: [u8; 4] = [1, 0, 0, 0];

macro_rules! check {
	($err:expr, $cond:expr) => {{
		if !$cond {
			return Err($err);
		}
	}};
}

impl Wasm {
	pub fn from(data: &[u8]) -> Result<Self, &str> {
		let wasm = Wasm {};
		check!("file size", data.len() >= 40);
		check!("file type", data[0..4] == WASM_BINARY_MAGIC);
		check!("WASM version", data[4..8] == WASM_BINARY_VERSION);
		let mut i = 8;
		let mut types = vec![];
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
				_ => println!("\tCURRENTLY NOT IMPLEMENTED"),
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
	Type = 1,
	Function = 3,
	Export = 7,
	Code = 10,
}

impl SectionId {
	fn from(byte: u8) -> Result<Self, &'static str> {
		match byte {
			1 => Ok(Self::Type),
			3 => Ok(Self::Function),
			7 => Ok(Self::Export),
			10 => Ok(Self::Code),
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
