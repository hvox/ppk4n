#![allow(dead_code)]
use std::collections::HashMap;

use super::mir::*;
use super::bytecode::*;
use super::common::*;

impl Program {
    pub fn compile(&self) -> Bytecode {
        todo!()
    }
}

pub struct Compiler<'p> {
    lir: Bytecode,
    hir: &'p Program,
    // layouts: HashMap<Type, Vec<ValueType>>,
}

pub struct FunCompiler<'c, 'p> {
    ctx: &'c mut Compiler<'p>,
    body: &'p Body,
    // results: Vec<ValueType>,
    locals: Locals,
}

#[derive(Default)]
struct Locals {
    count: usize,
    locations: HashMap<Str, Vec<usize>>,
    shadowed: Vec<(Str, Vec<usize>)>,
}

struct Layout {
    types: Vec<ValueType>,
    destructors: Vec<Option<Handle<Function>>>,
}

impl<'p> Compiler<'p> {
    pub fn new(program: &'p Program) -> Self {
        Self { lir: Bytecode::default(), hir: program }
    }
}

impl<'c, 'p> FunCompiler<'c, 'p> {
    pub fn new(context: &'c mut Compiler<'p>, body: &'p Body) -> Self {
        Self { ctx: context, body, locals: Locals::default() }
    }

    pub fn compile(&self) -> Func {
        todo!()
    }
}

impl Locals {
}
