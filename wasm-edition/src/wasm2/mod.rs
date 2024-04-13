mod binary;
mod errors;
mod instr;
mod macros;
mod valtype;
use valtype::Type;

use instr::Instr;

pub struct Wasm {}

impl Wasm {
	pub fn read<R: binary::Reader>(mut reader: R) -> Result<Self, errors::Error> {
		use errors::Error::*;
		use macros::check;
		check!(FileType, reader.u8s() == *b"\0asm\x01\0\0\0");
		let mut types = vec![];
		let mut imports = vec![];
		let mut functions = vec![];
		let mut memory = vec![];
		let mut export = vec![];
		let mut start = None;
		let mut code = vec![];
		while let Some(section_id) = reader.next() {
			check!(Corrupted, section_id < 12);
			let section_size = reader.u32() as usize;
			match section_id {
				0 => reader.skip(section_size), // Custom section
				1 => types = reader.vec(|r| r.check(96).vec2(|r| Type::from(r.u8()))),
				2 => imports = reader.vec(|r| (r.str(), r.str(), r.check(0).size())),
				3 => functions = reader.vec(|r| r.u32() as usize),
				// 4 => tables = todo!(),
				5 => memory.extend(reader.vec(|r| match r.u8() {
					0 => (r.u32(), u32::MAX),
					_ => (r.u32(), r.u32()),
				})),
				// 6 => globals = todo!(),
				7 => export = reader.vec(|r| (r.str(), r.check(0).u32() as usize)),
				8 => start = Some(reader.u32() as usize),
				// 9 => elements = todo!(),
				10 => code.extend(reader.vec(|reader| {
					let size = reader.size();
					let mut locals = vec![];
					for _ in 0..reader.size() {
						let amount = reader.size();
						let typ = Type::from_u8(reader.u8());
						locals.extend(vec![typ; amount]);
					}
					let code = Instr::read_from(reader, &types).unwrap_or(vec![]);
					(locals, code)
				})),
				_ => return Err(UnsupportedSection(section_id)),
			}
		}
		for (t, f) in code {
			println!("{:?} -> {:#?}", t, f);
		}
		Ok(Wasm {})
	}
}
