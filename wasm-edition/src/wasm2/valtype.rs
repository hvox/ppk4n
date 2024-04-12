#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Type {
	None = 0x40,
	// https://github.com/WebAssembly/stringref/blob/main/proposals/stringref/Overview.md#binary-encoding
	Str24 = 0x61,
	Str16 = 0x62,
	Str8 = 0x63,
	Str = 0x64,
	// https://webassembly.github.io/spec/core/binary/types
	Ref = 0x6f,
	Fun = 0x70,
	Vec = 0x7b,
	F64 = 0x7c,
	F32 = 0x7d,
	I64 = 0x7e,
	I32 = 0x7f,
}

impl Type {
	pub fn from_u8(code: u8) -> Type {
		match code {
			x if x == Type::Str24 as u8 => Type::Str24,
			x if x == Type::Str16 as u8 => Type::Str16,
			x if x == Type::Str8 as u8 => Type::Str8,
			x if x == Type::Str as u8 => Type::Str,
			x if x == Type::Ref as u8 => Type::Ref,
			x if x == Type::Fun as u8 => Type::Fun,
			x if x == Type::Vec as u8 => Type::Vec,
			x if x == Type::F64 as u8 => Type::F64,
			x if x == Type::F32 as u8 => Type::F32,
			x if x == Type::I64 as u8 => Type::I64,
			x if x == Type::I32 as u8 => Type::I32,
			_ => Type::None,
		}
	}

	pub fn from_table(table: &Vec<(Vec<Type>, Vec<Type>)>, s33: i64) -> Option<Vec<Type>> {
		if s33 > -0x80 && s33 < 0 {
			let typ = Type::from(s33 as u8);
			return Some(if typ != Type::None { vec![typ] } else { vec![] });
		}
		Some(table.get(s33 as usize)?.1.to_vec())
	}
}

impl From<u8> for Type {
	fn from(value: u8) -> Self {
		Self::from_u8(value)
	}
}
