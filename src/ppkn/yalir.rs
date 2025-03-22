#![allow(dead_code)]

use std::rc::Rc;

use indexmap::{IndexMap, IndexSet};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Program {
    pub imports: IndexSet<Import>,
    pub funcs: IndexMap<Rc<str>, Func>,
    pub globals: IndexMap<Rc<str>, GlobalVariable>,
    pub data: Vec<u8>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Import {
    pub signature: FuncType,
    pub namespace: Rc<str>,
    pub func_name: Rc<str>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Func {
    pub signature: FuncType,
    pub i32_locals: usize,
    pub f32_locals: usize,
    pub code: Vec<Instr>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FuncType {
    pub parameters: Vec<ValueType>,
    pub result: Vec<ValueType>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GlobalVariable {
    pub typ: ValueType,
    pub mutable: bool,
    pub value: Value,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
pub enum ValueType {
    I32,
    I64,
    F32,
    F64,
    // TODO: FuncRef,
    ExternRef,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Value {
    bits: u64,
}

// Oh no, I really would like to implement that thing as `enum`,
// but then it would still be stored in `Vec<u8>` and convertion
// from `u8` into `enum` is very frustrating to implement and
// I doubt it would be fast, so let's stick with `u8` for now.
pub type Instr = u8;
pub mod instr {
    use super::Instr;
    pub const UNREACHABLE: Instr = 0x00;
    pub const PASS: Instr = 0x01;
    pub const BLOCK: Instr = 0x02;
    pub const LOOP: Instr = 0x03;
    pub const IF_THEN: Instr = 0x04;
    pub const ELSE: Instr = 0x05;
    pub const END: Instr = 0x06;
    pub const JUMP: Instr = 0x07;
    pub const JUMP_IF: Instr = 0x08;
    pub const JUMP_INTO: Instr = 0x09;
    pub const RETURN: Instr = 0x0A;
    pub const CALL_FUNC: Instr = 0x0B;
    pub const CALL_IMPORT: Instr = 0x0C;
    pub const CALL_INDIRECT: Instr = 0x0D;
    pub const DROP: Instr = 0x0E;
    pub const SELECT: Instr = 0x0F;
    pub const GLOBAL_GET: Instr = 0x10;
    pub const GLOBAL_SET: Instr = 0x11;
    pub const LOCAL_GET: Instr = 0x12;
    pub const LOCAL_SET: Instr = 0x13;
    pub const LOCAL_TEE: Instr = 0x14;
    pub const MEMORY_SIZE: Instr = 0x15;
    pub const MEMORY_GROW: Instr = 0x16;
    pub const F32_CONST: Instr = 0x17;
    pub const F32_LOAD: Instr = 0x18;
    pub const F32_STORE: Instr = 0x19;
    pub const F32_ABS: Instr = 0x1A;
    pub const F32_CEIL: Instr = 0x1B;
    pub const F32_FLOOR: Instr = 0x1C;
    pub const F32_NEAREST: Instr = 0x1D;
    pub const F32_NEG: Instr = 0x1E;
    pub const F32_SQRT: Instr = 0x1F;
    pub const F32_TRUNC: Instr = 0x20;
    pub const F32_COPYSIGN: Instr = 0x21;
    pub const F32_ADD: Instr = 0x22;
    pub const F32_DIV: Instr = 0x23;
    pub const F32_MAX: Instr = 0x24;
    pub const F32_MIN: Instr = 0x25;
    pub const F32_MUL: Instr = 0x26;
    pub const F32_SUB: Instr = 0x27;
    pub const F32_EQ: Instr = 0x28;
    pub const F32_GE: Instr = 0x29;
    pub const F32_GT: Instr = 0x2A;
    pub const F32_LE: Instr = 0x2B;
    pub const F32_LT: Instr = 0x2C;
    pub const F32_NE: Instr = 0x2D;
    pub const I32_CONST: Instr = 0x2E;
    pub const I32_LOAD: Instr = 0x2F;
    pub const I32_STORE: Instr = 0x30;
    pub const I32_AND: Instr = 0x31;
    pub const I32_OR: Instr = 0x32;
    pub const I32_XOR: Instr = 0x33;
    pub const I32_ADD: Instr = 0x34;
    pub const I32_DIV: Instr = 0x35;
    pub const I32_MUL: Instr = 0x36;
    pub const I32_REM: Instr = 0x37;
    pub const I32_ROTL: Instr = 0x38;
    pub const I32_ROTR: Instr = 0x39;
    pub const I32_SHL: Instr = 0x3A;
    pub const I32_SHR: Instr = 0x3B;
    pub const I32_SUB: Instr = 0x3C;
    pub const I32_EQZ: Instr = 0x3D;
    pub const I32_EQ: Instr = 0x3E;
    pub const I32_GE: Instr = 0x3F;
    pub const I32_GT: Instr = 0x40;
    pub const I32_LE: Instr = 0x41;
    pub const I32_LT: Instr = 0x42;
    pub const I32_NE: Instr = 0x43;
    pub const U32_CONST: Instr = 0x44;
    pub const U32_LOAD: Instr = 0x2F;
    pub const U32_STORE: Instr = 0x30;
    pub const U32_AND: Instr = 0x31;
    pub const U32_OR: Instr = 0x32;
    pub const U32_XOR: Instr = 0x33;
    pub const U32_ADD: Instr = 0x45;
    pub const U32_DIV: Instr = 0x46;
    pub const U32_MUL: Instr = 0x47;
    pub const U32_REM: Instr = 0x48;
    pub const U32_ROTL: Instr = 0x38;
    pub const U32_ROTR: Instr = 0x39;
    pub const U32_SHL: Instr = 0x49;
    pub const U32_SHR: Instr = 0x4A;
    pub const U32_SUB: Instr = 0x4B;
    pub const U32_EQZ: Instr = 0x3D;
    pub const U32_EQ: Instr = 0x3E;
    pub const U32_GE: Instr = 0x4C;
    pub const U32_GT: Instr = 0x4D;
    pub const U32_LE: Instr = 0x4E;
    pub const U32_LT: Instr = 0x4F;
    pub const U32_NE: Instr = 0x43;
    pub const MAX_CODE: u8 = 0x4f;

    pub const NAMES: [&str; 80] = [
        "unreachable",
        "pass",
        "block",
        "loop",
        "if_then",
        "else",
        "end",
        "jump",
        "jump_if",
        "jump_into",
        "return",
        "call_func",
        "call_import",
        "call_indirect",
        "drop",
        "select",
        "global.get",
        "global.set",
        "local.get",
        "local.set",
        "local.tee",
        "memory.size",
        "memory.grow",
        "f32.const",
        "f32.load",
        "f32.store",
        "f32.abs",
        "f32.ceil",
        "f32.floor",
        "f32.nearest",
        "f32.neg",
        "f32.sqrt",
        "f32.trunc",
        "f32.copysign",
        "f32.add",
        "f32.div",
        "f32.max",
        "f32.min",
        "f32.mul",
        "f32.sub",
        "f32.eq",
        "f32.ge",
        "f32.gt",
        "f32.le",
        "f32.lt",
        "f32.ne",
        "i32.const",
        "i32.load",
        "i32.store",
        "i32.and",
        "i32.or",
        "i32.xor",
        "i32.add",
        "i32.div",
        "i32.mul",
        "i32.rem",
        "i32.rotl",
        "i32.rotr",
        "i32.shl",
        "i32.shr",
        "i32.sub",
        "i32.eqz",
        "i32.eq",
        "i32.ge",
        "i32.gt",
        "i32.le",
        "i32.lt",
        "i32.ne",
        "u32.const",
        "u32.add",
        "u32.div",
        "u32.mul",
        "u32.rem",
        "u32.shl",
        "u32.shr",
        "u32.sub",
        "u32.ge",
        "u32.gt",
        "u32.le",
        "u32.lt",
    ];
}
