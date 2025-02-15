#![allow(dead_code)]

use std::rc::Rc;

use super::lir::*;

const PAGE_SIZE: usize = 4096;

struct Interpreter {
	memory: Vec<u8>,
}

impl Interpreter {
	fn new(program: &Program) -> Self {
		let memory_size = (program.data.len() + PAGE_SIZE - 1) / PAGE_SIZE;
		let mut memory = program.data.clone();
		memory.reserve_exact(memory_size - memory.len());
		memory.extend((0..(memory_size - memory.len())).map(|_| 0));
		Self { memory }
	}

	fn interpret(&mut self, program: &Program, function: &str) -> Result<Vec<Value>, (Rc<str>, String)> {
		let f_idx = program.funcs.get_index_of(function).unwrap();
		let mut stack = vec![];
		let mut current_instr = 0;
		loop {
			let f = &program.funcs[f_idx];
			let f_name = program.funcs.get_index(f_idx).unwrap().0.clone();
			use instr::*;
			match f.code[current_instr] {
				UNREACHABLE => return Err((f_name, "Reached unreachable code".into())),
				PASS => {}
				END => return Ok(vec![]),
				F32_ABS => {
					let x = f32::from_bits(stack.pop().unwrap() as u32);
					stack.push((x.abs()).to_bits() as u64)
				}
				F32_CEIL => {
					let x = f32::from_bits(stack.pop().unwrap() as u32);
					stack.push((x.ceil()).to_bits() as u64)
				}
				F32_FLOOR => {
					let x = f32::from_bits(stack.pop().unwrap() as u32);
					stack.push((x.floor()).to_bits() as u64)
				}
				F32_NEAREST => {
					let x = f32::from_bits(stack.pop().unwrap() as u32);
					stack.push((x.round()).to_bits() as u64)
				}
				F32_NEG => {
					let x = f32::from_bits(stack.pop().unwrap() as u32);
					stack.push((-x).to_bits() as u64)
				}
				F32_SQRT => {
					let x = f32::from_bits(stack.pop().unwrap() as u32);
					stack.push((x.sqrt()).to_bits() as u64)
				}
				F32_TRUNC => {
					let x = f32::from_bits(stack.pop().unwrap() as u32);
					stack.push((x.trunc()).to_bits() as u64)
				}
				F32_COPYSIGN => {
					let x = f32::from_bits(stack.pop().unwrap() as u32);
					let y = f32::from_bits(stack.pop().unwrap() as u32);
					stack.push((x.copysign(y)).to_bits() as u64)
				}
				F32_ADD => {
					let x = f32::from_bits(stack.pop().unwrap() as u32);
					let y = f32::from_bits(stack.pop().unwrap() as u32);
					stack.push((x + y).to_bits() as u64)
				}
				F32_DIV => {
					let x = f32::from_bits(stack.pop().unwrap() as u32);
					let y = f32::from_bits(stack.pop().unwrap() as u32);
					stack.push((x / y).to_bits() as u64)
				}
				F32_MAX => {
					let x = f32::from_bits(stack.pop().unwrap() as u32);
					let y = f32::from_bits(stack.pop().unwrap() as u32);
					stack.push((x.max(y)).to_bits() as u64)
				}
				F32_MIN => {
					let x = f32::from_bits(stack.pop().unwrap() as u32);
					let y = f32::from_bits(stack.pop().unwrap() as u32);
					stack.push((x.min(y)).to_bits() as u64)
				}
				F32_MUL => {
					let x = f32::from_bits(stack.pop().unwrap() as u32);
					let y = f32::from_bits(stack.pop().unwrap() as u32);
					stack.push((x * y).to_bits() as u64)
				}
				F32_SUB => {
					let x = f32::from_bits(stack.pop().unwrap() as u32);
					let y = f32::from_bits(stack.pop().unwrap() as u32);
					stack.push((x - y).to_bits() as u64)
				}
				F32_EQ => {
					let x = f32::from_bits(stack.pop().unwrap() as u32);
					let y = f32::from_bits(stack.pop().unwrap() as u32);
					stack.push((x == y) as u64)
				}
				F32_GE => {
					let x = f32::from_bits(stack.pop().unwrap() as u32);
					let y = f32::from_bits(stack.pop().unwrap() as u32);
					stack.push((x >= y) as u64)
				}
				F32_GT => {
					let x = f32::from_bits(stack.pop().unwrap() as u32);
					let y = f32::from_bits(stack.pop().unwrap() as u32);
					stack.push((x > y) as u64)
				}
				F32_LE => {
					let x = f32::from_bits(stack.pop().unwrap() as u32);
					let y = f32::from_bits(stack.pop().unwrap() as u32);
					stack.push((x <= y) as u64)
				}
				F32_LT => {
					let x = f32::from_bits(stack.pop().unwrap() as u32);
					let y = f32::from_bits(stack.pop().unwrap() as u32);
					stack.push((x < y) as u64)
				}
				F32_NE => {
					let x = f32::from_bits(stack.pop().unwrap() as u32);
					let y = f32::from_bits(stack.pop().unwrap() as u32);
					stack.push((x != y) as u64)
				}
				I32_AND => {
					let x = (stack.pop().unwrap()) as i32;
					let y = (stack.pop().unwrap()) as i32;
					stack.push((x & y) as u64)
				}
				I32_OR => {
					let x = (stack.pop().unwrap()) as i32;
					let y = (stack.pop().unwrap()) as i32;
					stack.push((x | y) as u64)
				}
				I32_XOR => {
					let x = (stack.pop().unwrap()) as i32;
					let y = (stack.pop().unwrap()) as i32;
					stack.push((x ^ y) as u64)
				}
				I32_ADD => {
					let x = (stack.pop().unwrap()) as i32;
					let y = (stack.pop().unwrap()) as i32;
					stack.push((x.wrapping_add(y)) as u64)
				}
				I32_DIV => {
					let x = (stack.pop().unwrap()) as i32;
					let y = (stack.pop().unwrap()) as i32;
					stack.push((x.wrapping_div(y)) as u64)
				}
				I32_MUL => {
					let x = (stack.pop().unwrap()) as i32;
					let y = (stack.pop().unwrap()) as i32;
					stack.push((x.wrapping_mul(y)) as u64)
				}
				I32_REM => {
					let x = (stack.pop().unwrap()) as i32;
					let y = (stack.pop().unwrap()) as i32;
					stack.push((x.wrapping_rem(y)) as u64)
				}
				I32_ROTL => {
					let x = (stack.pop().unwrap()) as i32;
					let y = (stack.pop().unwrap()) as u32;
					stack.push((x.rotate_left(y)) as u64)
				}
				I32_ROTR => {
					let x = (stack.pop().unwrap()) as i32;
					let y = (stack.pop().unwrap()) as u32;
					stack.push((x.rotate_right(y)) as u64)
				}
				I32_SHL => {
					let x = (stack.pop().unwrap()) as i32;
					let y = (stack.pop().unwrap()) as u32;
					stack.push((x.wrapping_shl(y)) as u64)
				}
				I32_SHR => {
					let x = (stack.pop().unwrap()) as i32;
					let y = (stack.pop().unwrap()) as u32;
					stack.push((x.wrapping_shr(y)) as u64)
				}
				I32_SUB => {
					let x = (stack.pop().unwrap()) as i32;
					let y = (stack.pop().unwrap()) as i32;
					stack.push((x.wrapping_sub(y)) as u64)
				}
				I32_EQZ => {
					let x = (stack.pop().unwrap()) as i32;
					stack.push((x == 0) as u64)
				}
				I32_EQ => {
					let x = (stack.pop().unwrap()) as i32;
					let y = (stack.pop().unwrap()) as i32;
					stack.push((x != y) as u64)
				}
				I32_GE => {
					let x = (stack.pop().unwrap()) as i32;
					let y = (stack.pop().unwrap()) as i32;
					stack.push((x >= y) as u64)
				}
				I32_GT => {
					let x = (stack.pop().unwrap()) as i32;
					let y = (stack.pop().unwrap()) as i32;
					stack.push((x > y) as u64)
				}
				I32_LE => {
					let x = (stack.pop().unwrap()) as i32;
					let y = (stack.pop().unwrap()) as i32;
					stack.push((x <= y) as u64)
				}
				I32_LT => {
					let x = (stack.pop().unwrap()) as i32;
					let y = (stack.pop().unwrap()) as i32;
					stack.push((x < y) as u64)
				}
				I32_NE => {
					let x = (stack.pop().unwrap()) as i32;
					let y = (stack.pop().unwrap()) as i32;
					stack.push((x != y) as u64)
				}
				U32_AND => {
					let x = (stack.pop().unwrap()) as u32;
					let y = (stack.pop().unwrap()) as u32;
					stack.push((x & y) as u64)
				}
				U32_OR => {
					let x = (stack.pop().unwrap()) as u32;
					let y = (stack.pop().unwrap()) as u32;
					stack.push((x | y) as u64)
				}
				U32_XOR => {
					let x = (stack.pop().unwrap()) as u32;
					let y = (stack.pop().unwrap()) as u32;
					stack.push((x ^ y) as u64)
				}
				U32_ADD => {
					let x = (stack.pop().unwrap()) as u32;
					let y = (stack.pop().unwrap()) as u32;
					stack.push((x.wrapping_add(y)) as u64)
				}
				U32_DIV => {
					let x = (stack.pop().unwrap()) as u32;
					let y = (stack.pop().unwrap()) as u32;
					stack.push((x.wrapping_div(y)) as u64)
				}
				U32_MUL => {
					let x = (stack.pop().unwrap()) as u32;
					let y = (stack.pop().unwrap()) as u32;
					stack.push((x.wrapping_mul(y)) as u64)
				}
				U32_REM => {
					let x = (stack.pop().unwrap()) as u32;
					let y = (stack.pop().unwrap()) as u32;
					stack.push((x.wrapping_rem(y)) as u64)
				}
				U32_ROTL => {
					let x = (stack.pop().unwrap()) as u32;
					let y = (stack.pop().unwrap()) as u32;
					stack.push((x.rotate_left(y)) as u64)
				}
				U32_ROTR => {
					let x = (stack.pop().unwrap()) as u32;
					let y = (stack.pop().unwrap()) as u32;
					stack.push((x.rotate_right(y)) as u64)
				}
				U32_SHL => {
					let x = (stack.pop().unwrap()) as u32;
					let y = (stack.pop().unwrap()) as u32;
					stack.push((x.wrapping_shl(y)) as u64)
				}
				U32_SHR => {
					let x = (stack.pop().unwrap()) as u32;
					let y = (stack.pop().unwrap()) as u32;
					stack.push((x.wrapping_shr(y)) as u64)
				}
				U32_SUB => {
					let x = (stack.pop().unwrap()) as u32;
					let y = (stack.pop().unwrap()) as u32;
					stack.push((x.wrapping_sub(y)) as u64)
				}
				U32_EQZ => {
					let x = (stack.pop().unwrap()) as i32;
					stack.push((x == 0) as u64)
				}
				U32_EQ => {
					let x = (stack.pop().unwrap()) as u32;
					let y = (stack.pop().unwrap()) as u32;
					stack.push((x != y) as u64)
				}
				U32_GE => {
					let x = (stack.pop().unwrap()) as u32;
					let y = (stack.pop().unwrap()) as u32;
					stack.push((x >= y) as u64)
				}
				U32_GT => {
					let x = (stack.pop().unwrap()) as u32;
					let y = (stack.pop().unwrap()) as u32;
					stack.push((x > y) as u64)
				}
				U32_LE => {
					let x = (stack.pop().unwrap()) as u32;
					let y = (stack.pop().unwrap()) as u32;
					let result = x <= y;
					stack.push(result as u64);
				}
				U32_LT => {
					let x = (stack.pop().unwrap()) as u32;
					let y = (stack.pop().unwrap()) as u32;
					stack.push((x < y) as u64)
				}
				U32_NE => {
					let x = (stack.pop().unwrap()) as u32;
					let y = (stack.pop().unwrap()) as u32;
					stack.push((x != y) as u64)
				}
				instr => return Err((f_name, format!("Unknown instruction: 0x{:02X}", instr))),
			}
			current_instr += 1;
		}
	}
}
