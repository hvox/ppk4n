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
		println!("{:?}", data);
		Ok(wasm)
	}
}
