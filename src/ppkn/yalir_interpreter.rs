#![allow(dead_code)]

use std::{collections::HashMap, rc::Rc};

use super::yalir::*;

const DEBUG_LOGGING: bool = false;
const PAGE_SIZE: usize = 4096;

pub struct Interpreter<'a> {
    program: &'a Program,
    memory: Vec<u8>,
    strings: Strings,
}

pub struct Strings {
    strings: HashMap<u64, Box<str>>,
    next_id: u64,
}

impl<'a> Interpreter<'a> {
    pub fn new(program: &'a Program) -> Self {
        let memory_size = (program.data.len() + PAGE_SIZE - 1) / PAGE_SIZE * PAGE_SIZE;
        let mut memory = program.data.clone();
        memory.reserve_exact(memory_size - memory.len());
        memory.extend((0..(memory_size - memory.len())).map(|_| 0));
        Self {
            program,
            memory,
            strings: Strings::new(),
        }
    }

    pub fn interpret(&mut self, function: &str) -> Result<Vec<Value>, (Rc<str>, String)> {
        let program = self.program;
        let mut f_idx = program.funcs.get_index_of(function).unwrap();
        let mut stack = vec![];
        let mut current_instr = 0;
        loop {
            let f = &program.funcs[f_idx];
            let f_name = program.funcs.get_index(f_idx).unwrap().0.clone();
            use instr::*;
            if DEBUG_LOGGING {
                println!("{:X?}", stack);
                println!(
                    "{:2?}: {:02X?}",
                    current_instr, NAMES[f.code[current_instr] as usize]
                );
            }
            match f.code[current_instr] {
                UNREACHABLE => return Err((f_name, "Reached unreachable code".into())),
                PASS => {}
                BLOCK => todo!(),
                LOOP => todo!(),
                IF_THEN => todo!(),
                ELSE => todo!(),
                END => return Ok(vec![]),
                JUMP => todo!(),
                JUMP_IF => todo!(),
                JUMP_INTO => todo!(),
                RETURN => todo!(),
                CALL_FUNC => {
                    let callee = u32::from_ne_bytes(
                        f.code[current_instr + 1..current_instr + 5]
                            .try_into()
                            .unwrap(),
                    ) as usize;
                    current_instr += 4;
                    stack.push(f_idx as u64);
                    stack.push(current_instr as u64);
                    f_idx = callee;
                    current_instr = 0;
                    continue;
                }
                CALL_IMPORT => {
                    let callee = u32::from_ne_bytes(
                        f.code[current_instr + 1..current_instr + 5]
                            .try_into()
                            .unwrap(),
                    ) as usize;
                    current_instr += 4;
                    self.call_import(&mut stack, callee as usize);
                }
                CALL_INDIRECT => todo!(),
                DROP => todo!(),
                SELECT => todo!(),
                GLOBAL_GET => todo!(),
                GLOBAL_SET => todo!(),
                LOCAL_GET => todo!(),
                LOCAL_SET => todo!(),
                LOCAL_TEE => todo!(),
                MEMORY_SIZE => todo!(),
                MEMORY_GROW => todo!(),
                I32_CONST => {
                    let x = i32::from_ne_bytes(
                        f.code[current_instr + 1..current_instr + 5]
                            .try_into()
                            .unwrap(),
                    );
                    current_instr += 4;
                    stack.push(x as u64);
                }
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
                    stack.push((x <= y) as u64)
                }
                U32_LT => {
                    let x = (stack.pop().unwrap()) as u32;
                    let y = (stack.pop().unwrap()) as u32;
                    stack.push((x < y) as u64)
                }
                instr if instr < MAX_CODE => todo!("{}", NAMES[instr as usize]),
                code => return Err((f_name, format!("Unknown instruction code: 0x{:02X}", code))),
            }
            current_instr += 1;
        }
    }

    fn call_import(&mut self, stack: &mut Vec<u64>, callee: usize) {
        let callee = &self.program.imports[callee];
        let stack_size = stack.len();
        match (&callee.namespace[..], &callee.func_name[..]) {
            ("std", "string") => {
                let ptr = stack.pop().unwrap() as usize;
                let length =
                    u32::from_le_bytes(self.memory[ptr + 4..ptr + 8].try_into().unwrap()) as usize;
                let string = String::from_utf8(self.memory[ptr + 8..ptr + 8 + length].to_owned())
                    .unwrap()
                    .into_boxed_str();
                let string_id = self.strings.push(string);
                stack.push(string_id);
            }
            ("std", "println") => {
                let string_id = stack.pop().unwrap();
                let string = self.strings.pop(string_id);
                println!("{}", string);
            }
            (namespace, function) => panic!("Function not found: {}.{}", namespace, function),
        }
        debug_assert_eq!(
            stack_size - callee.signature.parameters.len() + callee.signature.result.len(),
            stack.len()
        );
    }
}

impl Strings {
    pub fn new() -> Self {
        Self {
            strings: HashMap::new(),
            next_id: 0xABC0,
        }
    }

    pub fn push(&mut self, string: Box<str>) -> u64 {
        let id = self.next_id;
        self.strings.insert(id, string);
        self.next_id = self.next_id.wrapping_add(1);
        id
    }

    pub fn pop(&mut self, id: u64) -> Box<str> {
        self.strings.remove(&id).unwrap()
    }
}
