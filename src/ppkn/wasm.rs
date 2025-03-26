#![allow(unused)]
use std::rc::Rc;

use indexmap::IndexSet;

use super::lir::{BlockType, Bytecode, FuncType, Op, ValueType};

impl Bytecode {
    pub fn to_wasm(&self) -> Vec<u8> {
        Assembler::new(self).assemble()
    }
}

struct Assembler<'b> {
    code: &'b Bytecode,
    types: IndexSet<FuncType>,
}

impl<'b> Assembler<'b> {
    fn new(bytecode: &'b Bytecode) -> Self {
        Self { code: bytecode, types: IndexSet::new() }
    }

    fn assemble(mut self) -> Vec<u8> {
        let mut wasm = b"\x00asm\x01\x00\x00\x00".to_vec();
        let imports = self.assemble_imports();
        let (functions, codes) = self.assemble_functions();
        let mut memory = vec![1u8, 0, 1];
        let globals = self.assemble_globals();
        let exports = self.assemble_exports();
        let data = self.assemble_data();
        let types = self.assemble_types();
        for (section_id, section_content) in [
            (1, types),
            (2, imports),
            (3, functions),
            // (0x04, tables),
            (5, memory),
            (6, globals),
            (7, exports),
            // (0x08, start),
            // (0x09, elements),
            (10, codes),
            (11, data),
        ] {
            if section_content.len() == 0 {
                continue;
            }
            wasm.push(section_id);
            wasm.encode(section_content.len());
            wasm.extend(section_content);
        }
        wasm
    }

    fn assemble_imports(&mut self) -> Vec<u8> {
        let mut imports = Vec::with_capacity(16 * self.code.imports.len());
        imports.encode(self.code.imports.len());
        self.code.imports.values().for_each(|import| {
            let (idx, _) = self.types.insert_full(import.signature.clone());
            let (module, fname) = match (import.namespace.as_ref(), import.func_name.as_ref()) {
                ("string", "print") => ("console", "log"),
                // ("string", "new_utf8") => ("window", "log"),
                import => import,
            };
            imports.encode(module);
            imports.encode(fname);
            imports.push(0x00);
            imports.encode(idx);
        });
        imports
    }

    fn assemble_functions(&mut self) -> (Vec<u8>, Vec<u8>) {
        let mut signs = vec![];
        let mut codes = vec![];
        signs.encode(self.code.functions.len());
        codes.encode(self.code.functions.len());
        self.code.functions.values().for_each(|func| {
            signs.encode(self.types.insert_full(func.signature.clone()).0);
            let mut locals: Vec<(ValueType, u32)> = vec![];
            for local in &func.locals[func.signature.parameters.len()..] {
                if locals.last().is_some_and(|(t, _)| local == t) {
                    locals.last_mut().unwrap().1 += 1;
                } else {
                    locals.push((local.clone(), 1));
                }
            }
            // let locals = func.locals.iter().fold(Vec::new(), |mut locals, x| {
            //     if locals.last().is_some_and(|(t, _)| t == x) {
            //         locals.last_mut().unwrap().1 += 1u32;
            //     } else {
            //         locals.push((x.clone(), 1));
            //     }
            //     locals
            // });
            let mut body = vec![];
            body.encode(locals.len());
            for (typ, count) in locals {
                body.encode(count);
                body.encode(typ);
            }
            for operation in &func.code {
                self.assemble_opcode(operation, &mut body)
            }
            codes.encode(&body[..]);
        });
        (signs, codes)
    }

    fn assemble_globals(&self) -> Vec<u8> {
        let mut globals = vec![];
        globals.encode(self.code.globals.len());
        for global in &self.code.globals {
            globals.encode(global.typ);
            globals.push(global.mutable as u8);
            globals.push(match global.typ {
                ValueType::I32 => 0x41,
                ValueType::I64 => 0x42,
                ValueType::F32 => 0x43,
                ValueType::F64 => 0x44,
                ValueType::ExternRef => 0xD0,
            });
            match global.typ {
                ValueType::I32 => globals.encode(global.value as i32),
                ValueType::I64 => globals.encode(global.value as i64),
                ValueType::F32 => globals.encode(f32::from_bits(global.value as u32)),
                ValueType::F64 => globals.encode(f64::from_bits(global.value)),
                ValueType::ExternRef => {}
            }
            globals.push(0x0B);
        }
        globals
    }

    fn assemble_exports(&self) -> Vec<u8> {
        let mut exports = vec![];
        exports.encode(2);
        exports.encode("main");
        exports.push(0x00);
        exports.encode(self.code.imports.len());
        exports.encode("memory");
        exports.extend([0x02, 0]);
        exports
    }

    fn assemble_data(&self) -> Vec<u8> {
        let mut data = vec![];
        data.encode(1);
        // if self.offset != 0 {
        //     wasm = vec![0x00, 0x41];
        //     wasm.pack(self.offset);
        //     wasm.push(0x0B);
        // } else {
        //     wasm = vec![0x01];
        // }
        data.extend([0x00, 0x41, 0x00, 0x0B]);
        data.encode(&*self.code.data);
        data
    }

    fn assemble_blocktype(&mut self, blocktype: &BlockType, code: &mut Vec<u8>) {
        use BlockType::*;
        match blocktype {
            ValueType(value_type) => code.encode(*value_type),
            TypeIndex(index) => code.encode(*index as i64),
            Void => code.push(0x40),
        }
    }

    fn assemble_types(mut self) -> Vec<u8> {
        let mut types = vec![];
        types.encode(self.types.len());
        for typ in self.types {
            types.push(0x60);
            types.encode(typ.parameters.len());
            typ.parameters.iter().for_each(|valtype| types.encode(*valtype));
            types.encode(typ.results.len());
            typ.results.iter().for_each(|valtype| types.encode(*valtype));
        }
        types
    }

    fn assemble_opcode(&mut self, op: &Op, code: &mut Vec<u8>) {
        use Op::*;
        match op {
            Block(typ, _) => {
                code.push(0x02);
                self.assemble_blocktype(typ, code);
            }
            Loop(typ, _) => {
                code.push(0x03);
                self.assemble_blocktype(typ, code);
            }
            IfThen(typ, _) => {
                code.push(0x04);
                self.assemble_blocktype(typ, code);
            }
            JumpTable(labels) => {
                code.encode(&labels[..labels.len() - 1]);
                code.encode(labels[labels.len() - 1]);
                code.push(0x0E);
            }
            Unreachable => {
                code.push(0x00);
            }
            Nop => {
                code.push(0x01);
            }
            Else(_size) => {
                code.push(0x05);
            }
            End => {
                code.push(0x0B);
            }
            EndIf => {
                code.push(0x0B);
            }
            Br(label) => {
                code.push(0x0C);
                code.encode(*label);
            }
            BrIf(label) => {
                code.push(0x0D);
                code.encode(*label);
            }
            Return => {
                code.push(0x0F);
            }
            CallFunc(index) => {
                code.push(0x10);
                code.encode(*index + self.code.imports.len());
            }
            CallImport(index) => {
                code.push(0x10);
                code.encode(*index);
            }
            CallIndirect(index) => {
                code.push(0x11);
                code.encode(*index);
            }
            Drop => {
                code.push(0x1A);
            }
            Select(typ) => {
                code.push(0x1B);
                code.encode(*typ);
            }
            GlobalGet(idx) => {
                code.push(0x23);
                code.encode(*idx);
            }
            GlobalSet(idx) => {
                code.push(0x24);
                code.encode(*idx);
            }
            LocalGet(idx) => {
                code.push(0x20);
                code.encode(*idx);
            }
            LocalSet(idx) => {
                code.push(0x21);
                code.encode(*idx);
            }
            LocalTee(idx) => {
                code.push(0x22);
                code.encode(*idx);
            }
            MemorySize => {
                code.push(0x3F);
                code.push(0);
            }
            MemoryGrow => {
                code.push(0x40);
                code.push(0);
            }
            MemoryCopy => {
                code.extend([0xFC, 0x0A, 0, 0]);
            }
            F32Const(value) => {
                code.push(0x43);
                code.encode(*value);
            }
            F32Load(offset) => {
                code.push(0x2A);
                code.encode(*offset);
            }
            F32Store(offset) => {
                code.push(0x36);
                code.encode(*offset);
            }
            F32Abs => {
                code.push(0x8B);
            }
            F32Ceil => {
                code.push(0x8D);
            }
            F32Floor => {
                code.push(0x8E);
            }
            F32Nearest => {
                code.push(0x90);
            }
            F32Neg => {
                code.push(0x8C);
            }
            F32Sqrt => {
                code.push(0x91);
            }
            F32Trunc => {
                code.push(0x8F);
            }
            F32Copysign => {
                code.push(0x98);
            }
            F32Add => {
                code.push(0x92);
            }
            F32Div => {
                code.push(0x95);
            }
            F32Max => {
                code.push(0x97);
            }
            F32Min => {
                code.push(0x96);
            }
            F32Mul => {
                code.push(0x94);
            }
            F32Sub => {
                code.push(0x93);
            }
            F32Eq => {
                code.push(0x5B);
            }
            F32Ge => {
                code.push(0x60);
            }
            F32Gt => {
                code.push(0x5E);
            }
            F32Le => {
                code.push(0x5F);
            }
            F32Lt => {
                code.push(0x5D);
            }
            F32Ne => {
                code.push(0x5C);
            }
            I32Const(value) => {
                code.push(0x41);
                code.encode(*value as i64);
            }
            I32Load(offset) => {
                code.push(0x2F);
                code.encode(*offset);
            }
            I32Store(offset) => {
                code.push(0x36);
                code.encode(*offset);
            }
            I32And => {
                code.push(0x71);
            }
            I32Or => {
                code.push(0x72);
            }
            I32Xor => {
                code.push(0x73);
            }
            I32Add(location) => {
                code.push(0x6A);
            }
            I32Div(location) => {
                code.push(0x6D);
            }
            I32Mul(location) => {
                code.push(0x6C);
            }
            I32Rem(location) => {
                code.push(0x6F);
            }
            I32Rotl => {
                code.push(0x77);
            }
            I32Rotr => {
                code.push(0x78);
            }
            I32Shl(location) => {
                code.push(0x74);
            }
            I32Shr(location) => {
                code.push(0x75);
            }
            I32Sub(location) => {
                code.push(0x6B);
            }
            I32Eqz => {
                code.push(0x45);
            }
            I32Eq => {
                code.push(0x46);
            }
            I32Ge => {
                code.push(0x4E);
            }
            I32Gt => {
                code.push(0x4A);
            }
            I32Le => {
                code.push(0x4C);
            }
            I32Lt => {
                code.push(0x48);
            }
            I32Ne => {
                code.push(0x47);
            }
            U32Const(value) => {
                code.push(0x41);
                code.encode(*value as i64);
            }
            U32Load(offset) => {
                code.push(0x2F);
                code.encode(*offset);
            }
            U32Store(offset) => {
                code.push(0x36);
                code.encode(*offset);
            }
            U32And => {
                code.push(0x71);
            }
            U32Or => {
                code.push(0x72);
            }
            U32Xor => {
                code.push(0x73);
            }
            U32Add(location) => {
                code.push(0x6A);
            }
            U32Div(location) => {
                code.push(0x6E);
            }
            U32Mul(location) => {
                code.push(0x6C);
            }
            U32Rem(location) => {
                code.push(0x70);
            }
            U32Rotl => {
                code.push(0x77);
            }
            U32Rotr => {
                code.push(0x78);
            }
            U32Shl(location) => {
                code.push(0x74);
            }
            U32Shr(location) => {
                code.push(0x76);
            }
            U32Sub(location) => {
                code.push(0x6B);
            }
            U32Eqz => {
                code.push(0x45);
            }
            U32Eq => {
                code.push(0x46);
            }
            U32Ge => {
                code.push(0x4F);
            }
            U32Gt => {
                code.push(0x4B);
            }
            U32Le => {
                code.push(0x4D);
            }
            U32Lt => {
                code.push(0x49);
            }
            U32Ne => {
                code.push(0x47);
            }
            F64Const(value) => {
                code.push(0x43);
                code.encode(*value);
            }
            F64Load(offset) => {
                code.push(0x2B);
                code.encode(*offset);
            }
            F64Store(offset) => {
                code.push(0x39);
                code.encode(*offset);
            }
            F64Abs => {
                code.push(0x99);
            }
            F64Ceil => {
                code.push(0x9B);
            }
            F64Floor => {
                code.push(0x9C);
            }
            F64Nearest => {
                code.push(0x9E);
            }
            F64Neg => {
                code.push(0x9A);
            }
            F64Sqrt => {
                code.push(0x9F);
            }
            F64Trunc => {
                code.push(0x9D);
            }
            F64Copysign => {
                code.push(0xA6);
            }
            F64Add => {
                code.push(0xA0);
            }
            F64Div => {
                code.push(0xA3);
            }
            F64Max => {
                code.push(0xA5);
            }
            F64Min => {
                code.push(0xA4);
            }
            F64Mul => {
                code.push(0xA2);
            }
            F64Sub => {
                code.push(0xA1);
            }
            F64Eq => {
                code.push(0x61);
            }
            F64Ge => {
                code.push(0x66);
            }
            F64Gt => {
                code.push(0x64);
            }
            F64Le => {
                code.push(0x65);
            }
            F64Lt => {
                code.push(0x63);
            }
            F64Ne => {
                code.push(0x62);
            }
            I64Const(value) => {
                code.push(0x42);
                code.encode(*value);
            }
            I64Load(offset) => {
                code.push(0x29);
                code.encode(*offset);
            }
            I64Store(offset) => {
                code.push(0x37);
                code.encode(*offset);
            }
            I64And => {
                code.push(0x83);
            }
            I64Or => {
                code.push(0x84);
            }
            I64Xor => {
                code.push(0x85);
            }
            I64Add(location) => {
                code.push(0x7C);
            }
            I64Div(location) => {
                code.push(0x7F);
            }
            I64Mul(location) => {
                code.push(0x7E);
            }
            I64Rem(location) => {
                code.push(0x81);
            }
            I64Rotl => {
                code.push(0x89);
            }
            I64Rotr => {
                code.push(0x8A);
            }
            I64Shl(location) => {
                code.push(0x86);
            }
            I64Shr(location) => {
                code.push(0x87);
            }
            I64Sub(location) => {
                code.push(0x7D);
            }
            I64Eqz => {
                code.push(0x50);
            }
            I64Eq => {
                code.push(0x51);
            }
            I64Ge => {
                code.push(0x59);
            }
            I64Gt => {
                code.push(0x55);
            }
            I64Le => {
                code.push(0x57);
            }
            I64Lt => {
                code.push(0x53);
            }
            I64Ne => {
                code.push(0x52);
            }
            U64Const(value) => {
                code.push(0x42);
                code.encode(*value as i64);
            }
            U64Load(offset) => {
                code.push(0x29);
                code.encode(*offset);
            }
            U64Store(offset) => {
                code.push(0x37);
                code.encode(*offset);
            }
            U64And => {
                code.push(0x83);
            }
            U64Or => {
                code.push(0x84);
            }
            U64Xor => {
                code.push(0x85);
            }
            U64Add(location) => {
                code.push(0x7C);
            }
            U64Div(location) => {
                code.push(0x80);
            }
            U64Mul(location) => {
                code.push(0x7E);
            }
            U64Rem(location) => {
                code.push(0x82);
            }
            U64Rotl => {
                code.push(0x89);
            }
            U64Rotr => {
                code.push(0x8A);
            }
            U64Shl(location) => {
                code.push(0x86);
            }
            U64Shr(location) => {
                code.push(0x88);
            }
            U64Sub(location) => {
                code.push(0x7D);
            }
            U64Eqz => {
                code.push(0x50);
            }
            U64Eq => {
                code.push(0x51);
            }
            U64Ge => {
                code.push(0x5A);
            }
            U64Gt => {
                code.push(0x56);
            }
            U64Le => {
                code.push(0x58);
            }
            U64Lt => {
                code.push(0x54);
            }
            U64Ne => {
                code.push(0x52);
            }
        }
    }
}

pub trait Encode<T> {
    fn encode(&mut self, value: T);
}

impl Encode<i64> for Vec<u8> {
    fn encode(&mut self, value: i64) {
        self.encode(value.abs() as usize);
        // TODO: wrong sign
    }
}

impl Encode<usize> for Vec<u8> {
    fn encode(&mut self, value: usize) {
        // let original_value = value;
        // let pos = self.len();
        let mut value = value;
        loop {
            self.push(0x7f & value as u8 | 0x80 * (value > 0x7f) as u8);
            value >>= 7;
            if value == 0 {
                break;
            }
        }
        // println!("{} -> {:?}", original_value, &self[pos..]);
    }
}

impl Encode<i32> for Vec<u8> {
    fn encode(&mut self, value: i32) {
        self.encode(value as i64);
    }
}

impl Encode<u32> for Vec<u8> {
    fn encode(&mut self, value: u32) {
        self.encode(value as usize);
    }
}

impl Encode<&str> for Vec<u8> {
    fn encode(&mut self, string: &str) {
        self.encode(string.as_bytes());
    }
}

impl Encode<Rc<str>> for Vec<u8> {
    fn encode(&mut self, string: Rc<str>) {
        self.encode(string.as_bytes());
    }
}

impl Encode<Vec<Vec<u8>>> for Vec<u8> {
    fn encode(&mut self, array: Vec<Vec<u8>>) {
        self.encode(array.len());
        for bytes in array.into_iter() {
            self.extend(bytes);
        }
    }
}

impl Encode<&[u8]> for Vec<u8> {
    fn encode(&mut self, bytes: &[u8]) {
        self.encode(bytes.len());
        self.extend(bytes);
    }
}

impl Encode<&[usize]> for Vec<u8> {
    fn encode(&mut self, indexes: &[usize]) {
        self.encode(indexes.len());
        indexes.iter().for_each(|i| self.encode(*i));
    }
}

impl Encode<ValueType> for Vec<u8> {
    fn encode(&mut self, typ: ValueType) {
        self.push(match typ {
            ValueType::I32 => 0x7F,
            ValueType::I64 => 0x7E,
            ValueType::F32 => 0x7D,
            ValueType::F64 => 0x7C,
            ValueType::ExternRef => 0x6F,
        });
    }
}

impl Encode<f32> for Vec<u8> {
    fn encode(&mut self, number: f32) {
        self.extend(number.to_le_bytes());
    }
}

impl Encode<f64> for Vec<u8> {
    fn encode(&mut self, number: f64) {
        self.extend(number.to_le_bytes());
    }
}
