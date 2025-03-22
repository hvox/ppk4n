#![allow(unused)]
use indexmap::IndexSet;

use crate::utils::stringify_lossy;

use super::wasm;
use super::yair::*;

impl Program {
    pub fn to_wasm(&self) -> wasm::WebAssemblyModule {
        Compiler::new(self).compile();
        todo!()
    }
}

struct Compiler<'a> {
    program: &'a Program,
    types: IndexSet<WasmFnType>,
    signatures: Vec<usize>,
}

const WASM_VERSION: u32 = 1;
const HOSTLANG_DEPS: [(&'static str, &'static str); 1] = [("console", "log")];

impl<'a> Compiler<'a> {
    fn new(program: &'a Program) -> Self {
        Self {
            program,
            types: IndexSet::new(),
            signatures: vec![],
        }
    }

    fn compile(mut self) -> Vec<u8> {
        let mut binary = b"\x00asm".to_vec();
        binary.extend(WASM_VERSION.to_le_bytes());
        let mut code = vec![];
        for f in &self.program.fns {
            let (signature, body) = todo!();
            self.signatures.push(signature);
            code.push(body);
        }
        println!("{}", stringify_lossy(&binary));
        self.pack_types(&mut binary);
        // TODO: pack imports
        println!("{}", stringify_lossy(&binary));
        self.pack_signatures(&mut binary);
        println!("{}", stringify_lossy(&binary));
        self.pack_code(&mut binary, code);
        binary
    }

    fn pack_types(&self, binary: &mut Vec<u8>) {
        const TYPE_SECTION_ID: u8 = 0x01;
        let mut section = vec![];
        section.pack_usize(self.types.len());
        for typ in &self.types {
            section.pack_usize(typ.params.len());
            for param in &typ.params {
                // section.push(param.compile());
            }
            section.pack_usize(typ.results.len());
            // for param in &typ.results {
            // 	section.push(param.compile());
            // }
        }
        binary.pack_section(TYPE_SECTION_ID, &section);
    }

    fn pack_code(&self, binary: &mut Vec<u8>, code: Vec<Vec<u8>>) {
        const CODE_SECTION_ID: u8 = 0x0A;
        let mut section = vec![];
        section.pack_usize(code.len());
        for body in &code {
            section.pack_usize(body.len());
            section.extend(body);
        }
        binary.pack_section(CODE_SECTION_ID, &section);
    }

    fn pack_signatures(&self, binary: &mut Vec<u8>) {
        const FUNCTION_SECTION_ID: u8 = 0x03;
        let mut section = vec![];
        section.pack_usize(self.signatures.len());
        for &f in &self.signatures {
            section.pack_usize(f);
        }
        binary.pack_section(FUNCTION_SECTION_ID, &section);
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct WasmFnType {
    params: Vec<Vec<u8>>,
    results: Vec<Vec<u8>>,
}

impl WasmFnType {
    fn new(params: Vec<Type>, results: Vec<Type>) -> Self {
        todo!()
        // Self { params, results }
    }
}

trait Leb128 {
    fn pack_u32(&mut self, value: u32);
    fn pack_usize(&mut self, value: usize);
    fn pack_section(&mut self, typ: u8, section: &[u8]);
}

impl Leb128 for Vec<u8> {
    fn pack_u32(&mut self, mut value: u32) {
        loop {
            self.push(0x7f & value as u8 + (value > 0x7f) as u8);
            value >>= 7;
            if value == 0 {
                break;
            }
        }
    }

    fn pack_usize(&mut self, mut value: usize) {
        loop {
            self.push(0x7f & value as u8 + (value > 0x7f) as u8);
            value >>= 7;
            if value == 0 {
                break;
            }
        }
    }

    fn pack_section(&mut self, typ: u8, section: &[u8]) {
        self.push(typ);
        self.pack_u32(section.len() as u32);
        self.extend(section);
    }
}
