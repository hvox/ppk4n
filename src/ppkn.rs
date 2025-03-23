#![allow(clippy::ptr_arg, clippy::only_used_in_recursion)]
pub mod lexer;
pub mod parser;
pub mod hir;
pub mod transpiler;
pub mod lir;
pub mod wasm;
pub mod runtime;
pub mod error;
