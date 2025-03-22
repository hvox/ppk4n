#![allow(unused)]
use indexmap::IndexMap;

// TODO: rename to module
#[derive(Debug)]
pub struct Program<'a> {
    pub functions: IndexMap<String, Function<'a>>,
    // types: IndexMap<String, Type>
    pub phantom: std::marker::PhantomData<&'a str>,
}

#[derive(Debug)]
pub struct Function<'a> {
    pub params: IndexMap<String, Type>,
    pub result: Type,
    pub locals: IndexMap<String, Type>,
    pub code: Vec<Instr<'a>>, // body
}

#[derive(Clone, Copy, Debug)]
pub enum Type {
    I64,
    F64,
    Str,
    Void,
}

#[derive(Clone, Debug)]
pub struct Instr<'a> {
    pub source: &'a str,
    pub kind: InstrKind<'a>,
}

#[derive(Clone, Debug)]
pub enum InstrKind<'a> {
    Definition(usize, Box<Instr<'a>>),
    Assignment(usize, Box<Instr<'a>>),
    Return(Box<Instr<'a>>),
    While(Box<Instr<'a>>, Vec<Instr<'a>>),
    Print(Box<Instr<'a>>),

    Integer(i64),
    Float(f64),
    Variable(usize),
    String(String),
    Call(usize, Vec<Instr<'a>>),
    AddI64(Box<Instr<'a>>, Box<Instr<'a>>),
    SubI64(Box<Instr<'a>>, Box<Instr<'a>>),
    MulI64(Box<Instr<'a>>, Box<Instr<'a>>),
    DivI64(Box<Instr<'a>>, Box<Instr<'a>>),
    AddF64(Box<Instr<'a>>, Box<Instr<'a>>),
    LessI64(Box<Instr<'a>>, Box<Instr<'a>>),
}
