use super::Type;

// https://webassembly.github.io/spec/core/binary/instructions
#[repr(u8)]
#[allow(non_camel_case_types)]
pub enum Instr {
	// Control Instructions
	Unreachable = 0x00,
	Nop = 0x01,
	Block(Vec<Type>, Vec<Instr>) = 0x02,
	Loop(Vec<Type>, Vec<Instr>) = 0x03,
	IfElse(Vec<Type>, Vec<Instr>, Vec<Instr>) = 0x04,
	Br(u32) = 0x0C,
	BrIf(u32) = 0x0D,
	BrTable(Vec<u32>, u32) = 0x0E,
	Return = 0x0F,
	Call(u32) = 0x10,
	CallIndirrect(u32) = 0x11,

	// Reference Instructions
	Null(Type) = 0xD0,
	IsNull = 0xD1,
	Func(u32) = 0xD2,

	// Parametric Instructions
	Drop = 0x1A,
	Select = 0x1B,
	SelectVec(Vec<Type>) = 0x1C,

	// Variable Instructions
	LocalGet(u32) = 0x20,
	LocalSet(u32) = 0x21,
	LocalTee(u32) = 0x22,
	GlobalGet(u32) = 0x23,
	GlobalSet(u32) = 0x24,

	// Table Instructions
	// TODO

	// Memory Instructions
	// TODO

	// Numeric Instructions
	ConstI32(i32) = 0x41,
	ConstI64(i64) = 0x42,
	ConstF32(f32) = 0x43,
	ConstF64(f64) = 0x44,
	i32_eqz = 0x45,
	i32_eq = 0x46,
	i32_ne = 0x47,
	i32_lt_s = 0x48,
	i32_lt_u = 0x49,
	i32_gt_s = 0x4a,
	i32_gt_u = 0x4b,
	i32_le_s = 0x4c,
	i32_le_u = 0x4d,
	i32_ge_s = 0x4e,
	i32_ge_u = 0x4f,
	i64_eqz = 0x50,
	i64_eq = 0x51,
	i64_ne = 0x52,
	i64_lt_s = 0x53,
	i64_lt_u = 0x54,
	i64_gt_s = 0x55,
	i64_gt_u = 0x56,
	i64_le_s = 0x57,
	i64_le_u = 0x58,
	i64_ge_s = 0x59,
	i64_ge_u = 0x5a,
	f32_eq = 0x5b,
	f32_ne = 0x5c,
	f32_lt = 0x5d,
	f32_gt = 0x5e,
	f32_le = 0x5f,
	f32_ge = 0x60,
	f64_eq = 0x61,
	f64_ne = 0x62,
	f64_lt = 0x63,
	f64_gt = 0x64,
	f64_le = 0x65,
	f64_ge = 0x66,
	i32_clz = 0x67,
	i32_ctz = 0x68,
	i32_popcnt = 0x69,
	i32_add = 0x6a,
	i32_sub = 0x6b,
	i32_mul = 0x6c,
	i32_div_s = 0x6d,
	i32_div_u = 0x6e,
	i32_rem_s = 0x6f,
	i32_rem_u = 0x70,
	i32_and = 0x71,
	i32_or = 0x72,
	i32_xor = 0x73,
	i32_shl = 0x74,
	i32_shr_s = 0x75,
	i32_shr_u = 0x76,
	i32_rotl = 0x77,
	i32_rotr = 0x78,
	i64_clz = 0x79,
	i64_ctz = 0x7a,
	i64_popcnt = 0x7b,
	i64_add = 0x7c,
	i64_sub = 0x7d,
	i64_mul = 0x7e,
	i64_div_s = 0x7f,
	i64_div_u = 0x80,
	i64_rem_s = 0x81,
	i64_rem_u = 0x82,
	i64_and = 0x83,
	i64_or = 0x84,
	i64_xor = 0x85,
	i64_shl = 0x86,
	i64_shr_s = 0x87,
	i64_shr_u = 0x88,
	i64_rotl = 0x89,
	i64_rotr = 0x8a,
	f32_abs = 0x8b,
	f32_neg = 0x8c,
	f32_ceil = 0x8d,
	f32_floor = 0x8e,
	f32_trunc = 0x8f,
	f32_nearest = 0x90,
	f32_sqrt = 0x91,
	f32_add = 0x92,
	f32_sub = 0x93,
	f32_mul = 0x94,
	f32_div = 0x95,
	f32_min = 0x96,
	f32_max = 0x97,
	f32_copysign = 0x98,
	f64_abs = 0x99,
	f64_neg = 0x9a,
	f64_ceil = 0x9b,
	f64_floor = 0x9c,
	f64_trunc = 0x9d,
	f64_nearest = 0x9e,
	f64_sqrt = 0x9f,
	f64_add = 0xa0,
	f64_sub = 0xa1,
	f64_mul = 0xa2,
	f64_div = 0xa3,
	f64_min = 0xa4,
	f64_max = 0xa5,
	f64_copysign = 0xa6,
	i32_wrap_i64 = 0xa7,
	i32_trunc_f32_s = 0xa8,
	i32_trunc_f32_u = 0xa9,
	i32_trunc_f64_s = 0xaa,
	i32_trunc_f64_u = 0xab,
	i64_extend_i32_s = 0xac,
	i64_extend_i32_u = 0xad,
	i64_trunc_f32_s = 0xae,
	i64_trunc_f32_u = 0xaf,
	i64_trunc_f64_s = 0xb0,
	i64_trunc_f64_u = 0xb1,
	f32_convert_i32_s = 0xb2,
	f32_convert_i32_u = 0xb3,
	f32_convert_i64_s = 0xb4,
	f32_convert_i64_u = 0xb5,
	f32_demote_f64 = 0xb6,
	f64_convert_i32_s = 0xb7,
	f64_convert_i32_u = 0xb8,
	f64_convert_i64_s = 0xb9,
	f64_convert_i64_u = 0xba,
	f64_promote_f32 = 0xbb,
	i32_reinterpret_f32 = 0xbc,
	i64_reinterpret_f64 = 0xbd,
	f32_reinterpret_i32 = 0xbe,
	f64_reinterpret_i64 = 0xbf,
	i32_extend8_s = 0xc0,
	i32_extend16_s = 0xc1,
	i64_extend8_s = 0xc2,
	i64_extend16_s = 0xc3,
	i64_extend32_s = 0xc4,
	// Vector Instructions
	// TODO: There are too many of them...
}

impl Instr {
	const fn discriminant(&self) -> u8 {
		unsafe { *(self as *const Self as *const u8) }
	}

	const fn as_u8(&self) -> u8 {
		return self.discriminant();
	}

	pub fn read_from<Reader: super::binary::Reader>(
		reader: &mut Reader,
		table: &Vec<(Vec<Type>, Vec<Type>)>,
	) -> Option<Vec<Self>> {
		let mut block = vec![];
		while let Some(instr) = reader.next() {
			use Instr::*;
			block.push(match instr {
				_ if instr == Unreachable.as_u8() => Unreachable,
				_ if instr == Nop.as_u8() => Nop,
				_ if instr == Block(vec![], vec![]).as_u8() => {
					Block(Type::from_table(table, reader.i64())?, Instr::read_from(reader, table)?)
				}
				_ if instr == Loop(vec![], vec![]).as_u8() => {
					Loop(Type::from_table(table, reader.i64())?, Instr::read_from(reader, table)?)
				}
				_ if instr == IfElse(vec![], vec![], vec![]).as_u8() => {
					Loop(Type::from_table(table, reader.i64())?, Instr::read_from(reader, table)?)
				}
				_ => panic!("Unsupported instruction: 0x{:02X}", instr),
			});
		}
		vec![].into()
	}
}
