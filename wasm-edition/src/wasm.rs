pub struct Wasm {}

const WASM_BINARY_MAGIC: [u8; 4] = *b"\0asm";
const WASM_BINARY_VERSION: [u8; 4] = [1, 0, 0, 0];

impl Wasm {
	pub fn from(data: &[u8]) -> Result<Self, &str> {
		macro_rules! check {
			($err:expr, $cond:expr) => {{
				if !$cond {
					return Err($err);
				}
			}};
		}
		let wasm = Wasm {};
		check!("file size", data.len() >= 40);
		check!("file type", data[0..4] == WASM_BINARY_MAGIC);
		check!("WASM version", data[4..8] == WASM_BINARY_VERSION);
		let mut i = 8;
		while i + 3 <= data.len() {
			let section = SectionId::from(data[i])?;
			println!("Section {:?}", section);
			let size = data[i + 1] as usize;
			check!("file size", i + 2 + size <= data.len());
			println!("\tsize: {}", size);
			match section {
				_ => println!("\tCURRENTLY NOT IMPLEMENTED")
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
			1 => Ok(SectionId::Type),
			3 => Ok(SectionId::Function),
			7 => Ok(SectionId::Export),
			10 => Ok(SectionId::Code),
			_ => Err("section id"),
		}
	}
}
