pub mod ast;
pub mod interpreter;
pub mod ir;
pub mod parser;
pub mod token;
pub mod tokenizer;
pub mod typechecker;

pub mod mir;
pub mod mir_compiler;
pub mod mir_generator;
pub mod mir_interpreter;
pub mod mir_to_yair_translator;
pub mod transpiler;

pub mod wasm;
pub mod yair;
pub mod yair_compiler;

pub mod yaira;
pub mod yaira_typechecker;

pub mod hir;
pub mod hir_compiler;
pub mod hir_gen;

pub mod lir;
pub mod lir_interpreter;

pub mod yahir;
pub mod yahir_compiler;
pub mod yahir_gen;
pub mod yalir;
pub mod yalir_interpreter;

use mir::Program;

const DEBUG_LOGGING: bool = false;

#[derive(Debug)]
pub struct PpknError<'a> {
    pub typ: &'static str,
    pub message: &'static str,
    pub location: &'a str,
}

pub fn parse(source: &str) -> Result<Program, PpknError> {
    const USE_YALIR: bool = false;
    if USE_YALIR {
        let _ = to_lir(source);
    }
    let tokens = tokenizer::tokenize(source);
    let ast = parser::parse(tokens)?;

    use std::io::IsTerminal;
    if DEBUG_LOGGING && std::io::stdout().is_terminal() {
        let hir = yahir_gen::typecheck(&ast).unwrap();
        println!("\x1b[91m[HIR] {:?}\x1b[0m", hir);
        let lir = yahir_compiler::lower_hir_to_lir(&hir);
        println!("\x1b[93m[LIR] {:?}\x1b[0m", lir);
        print!("\x1b[92m[OUTPUT]: ");
        print!("\x1b[0m");
    }
    let program = mir_generator::typecheck(ast)?;
    Ok(program)
}

pub fn to_lir(source: &str) -> Result<yalir::Program, PpknError> {
    let tokens = tokenizer::tokenize(source);
    let ast = parser::parse(tokens)?;
    let hir = yahir_gen::typecheck(&ast).unwrap();
    use std::io::IsTerminal;
    if DEBUG_LOGGING && std::io::stdout().is_terminal() {
        println!("\x1b[91m[HIR] {:?}\x1b[0m", hir);
    }
    let lir = yahir_compiler::lower_hir_to_lir(&hir);
    if DEBUG_LOGGING && std::io::stdout().is_terminal() {
        println!("\x1b[93m[LIR] {:?}\x1b[0m", lir);
        print!("\x1b[92m[OUTPUT]: ");
        print!("\x1b[0m");
    }
    Ok(lir)
}

pub fn run(source: &str) -> Result<(), PpknError> {
    const DEBUG_PARSING: bool = false;
    let program = parse(source)?;
    #[cfg(debug_assertions)]
    if DEBUG_PARSING {
        for func in &program.functions {
            println!("fun {} {:?} -> {:?}:", func.name, func.params, func.result);
            for stmt in &func.body {
                println!("  {:12} {:?}", stmt.source.replace("\n", "\\n"), stmt.kind);
            }
        }
    }
    program.run()?;
    Ok(())
}

impl<'a> From<mir_generator::TypeError<'a>> for PpknError<'a> {
    fn from(error: mir_generator::TypeError<'a>) -> Self {
        Self {
            typ: "TypeError",
            message: error.message,
            location: error.location,
        }
    }
}

impl<'a> From<parser::SyntaxError<'a>> for PpknError<'a> {
    fn from(error: parser::SyntaxError<'a>) -> Self {
        Self {
            typ: "SyntaxError",
            message: error.message,
            location: error.source,
        }
    }
}

impl<'a> From<mir_interpreter::RuntimeError<'a>> for PpknError<'a> {
    fn from(error: mir_interpreter::RuntimeError<'a>) -> Self {
        Self {
            typ: "RuntimeError",
            message: error.message,
            location: error.location,
        }
    }
}
