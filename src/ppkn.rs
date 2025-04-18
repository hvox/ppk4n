#![allow(clippy::ptr_arg, clippy::only_used_in_recursion)]
pub mod error;
pub mod hir;
pub mod lexer;
pub mod lir;
pub mod parser;
pub mod runtime;
pub mod transpiler;
pub mod wasm;
