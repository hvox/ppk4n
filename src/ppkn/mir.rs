#![allow(unused)]
use std::{ops::Deref, rc::Rc};

// TODO: rename to module
#[derive(Clone, Debug)]
pub struct Program<'a> {
    pub structs: Vec<Struct<'a>>,
    pub functions: Vec<Function<'a, ()>>,
}

#[derive(Clone, Debug)]
pub struct Struct<'a> {
    pub name: Str,
    pub params: Vec<Str>,
    pub methods: Vec<Function<'a, ()>>,
}

#[derive(Clone, Debug)]
pub struct Function<'a, T> {
    pub name: Str,
    pub params: Vec<(Str, Type)>,
    pub result: Type,
    pub locals: Vec<(Str, Type)>,
    pub body: Vec<InstrCntrl<'a>>,
    pub extra: T,
}

impl<T> Deref for Function<'_, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.extra
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Type {
    Unit,
    Bool,
    I64,
    U64,
    F64,
    Str,
    Vec(Rc<Type>),
}

#[derive(Clone, Debug)]
pub struct Instr<'a> {
    pub source: &'a str,
    pub kind: InstrKind<'a>,
}

#[derive(Clone, Debug)]
pub enum InstrKind<'a> {
    Cntrl(InstrCntrl<'a>),
    Bool(InstrBool<'a>),
    I64(InstrI64<'a>),
    U64(InstrU64<'a>),
    F64(InstrF64<'a>),
    Str(InstrStr<'a>),
    Vec(InstrVec<'a>, Type),
}

#[derive(Clone, Debug)]
pub struct InstrI64<'a> {
    pub source: &'a str,
    pub kind: Box<InstrKindI64<'a>>,
}

#[derive(Clone, Debug)]
pub enum InstrKindI64<'a> {
    Add(InstrI64<'a>, InstrI64<'a>),
    Sub(InstrI64<'a>, InstrI64<'a>),
    Mult(InstrI64<'a>, InstrI64<'a>),
    Div(InstrI64<'a>, InstrI64<'a>),
    Rem(InstrI64<'a>, InstrI64<'a>),
    And(InstrI64<'a>, InstrI64<'a>),
    Xor(InstrI64<'a>, InstrI64<'a>),
    Or(InstrI64<'a>, InstrI64<'a>),
    Return(Instr<'a>),
    Variable(usize),
    Call(usize),
    Value(i64),
}

impl<'a> InstrI64<'a> {
    pub fn new(source: &'a str, kind: InstrKindI64<'a>) -> Self {
        Self {
            source,
            kind: Box::new(kind),
        }
    }
}

#[derive(Clone, Debug)]
pub struct InstrU64<'a> {
    pub source: &'a str,
    pub kind: Box<InstrKindU64<'a>>,
}

#[derive(Clone, Debug)]
pub enum InstrKindU64<'a> {
    Add(InstrU64<'a>, InstrU64<'a>),
    Sub(InstrU64<'a>, InstrU64<'a>),
    Mult(InstrU64<'a>, InstrU64<'a>),
    Div(InstrU64<'a>, InstrU64<'a>),
    Rem(InstrU64<'a>, InstrU64<'a>),
    And(InstrU64<'a>, InstrU64<'a>),
    Xor(InstrU64<'a>, InstrU64<'a>),
    Or(InstrU64<'a>, InstrU64<'a>),
    Return(Instr<'a>),
    Variable(usize),
    Call(usize),
    Value(u64),
}

impl<'a> InstrU64<'a> {
    pub fn new(source: &'a str, kind: InstrKindU64<'a>) -> Self {
        Self {
            source,
            kind: Box::new(kind),
        }
    }
}

#[derive(Clone, Debug)]
pub struct InstrF64<'a> {
    pub source: &'a str,
    pub kind: Box<InstrKindF64<'a>>,
}

#[derive(Clone, Debug)]
pub enum InstrKindF64<'a> {
    Add(InstrF64<'a>, InstrF64<'a>),
    Sub(InstrF64<'a>, InstrF64<'a>),
    Mult(InstrF64<'a>, InstrF64<'a>),
    Div(InstrF64<'a>, InstrF64<'a>),
    Return(Instr<'a>),
    Variable(usize),
    Call(usize),
    Value(f64),
}

impl<'a> InstrF64<'a> {
    pub fn new(source: &'a str, kind: InstrKindF64<'a>) -> Self {
        Self {
            source,
            kind: Box::new(kind),
        }
    }
}

#[derive(Clone, Debug)]
pub struct InstrStr<'a> {
    pub source: &'a str,
    pub kind: Box<InstrKindStr<'a>>,
}

#[derive(Clone, Debug)]
pub enum InstrKindStr<'a> {
    Add(InstrStr<'a>, InstrStr<'a>),
    Return(Instr<'a>),
    Variable(usize),
    Value(Str),
    Call(usize),
    CastI64(InstrI64<'a>),
    CastF64(InstrF64<'a>),
    CastVec(InstrVec<'a>, Type),
}

impl<'a> InstrStr<'a> {
    pub fn new(source: &'a str, kind: InstrKindStr<'a>) -> Self {
        Self {
            source,
            kind: Box::new(kind),
        }
    }
}

#[derive(Clone, Debug)]
pub struct InstrBool<'a> {
    pub source: &'a str,
    pub kind: Box<InstrKindBool<'a>>,
}

#[derive(Clone, Debug)]
pub enum InstrKindBool<'a> {
    LessI64(InstrI64<'a>, InstrI64<'a>),
    Not(InstrBool<'a>),
    And(InstrBool<'a>, InstrStr<'a>),
    Return(Instr<'a>),
    Variable(usize),
    Value(Str),
    Call(usize),
}

impl<'a> InstrBool<'a> {
    pub fn new(source: &'a str, kind: InstrKindBool<'a>) -> Self {
        Self {
            source,
            kind: Box::new(kind),
        }
    }
}

#[derive(Clone, Debug)]
pub struct InstrVec<'a> {
    pub source: &'a str,
    pub kind: Box<InstrKindVec<'a>>,
}

#[derive(Clone, Debug)]
pub enum InstrKindVec<'a> {
    Return(Instr<'a>),
    Variable(usize),
    Value(Str),
    Call(usize),
    Empty,
}

impl<'a> InstrVec<'a> {
    pub fn new(source: &'a str, kind: InstrKindVec<'a>) -> Self {
        Self {
            source,
            kind: Box::new(kind),
        }
    }
}

#[derive(Clone, Debug)]
pub struct InstrCntrl<'a> {
    pub source: &'a str,
    pub kind: Box<InstrKindCntrl<'a>>,
}

#[derive(Clone, Debug)]
pub enum InstrKindCntrl<'a> {
    Call(usize, Vec<Instr<'a>>),
    DefI64(usize, InstrI64<'a>),
    DefU64(usize, InstrU64<'a>),
    DefF64(usize, InstrF64<'a>),
    DefStr(usize, InstrStr<'a>),
    DefBool(usize, InstrBool<'a>),
    DefVec(usize, InstrVec<'a>, Type),
    SetI64(usize, InstrI64<'a>),
    SetU64(usize, InstrU64<'a>),
    SetF64(usize, InstrF64<'a>),
    SetStr(usize, InstrStr<'a>),
    SetBool(usize, InstrBool<'a>),
    SetVec(usize),
    PrintStr(InstrStr<'a>),
    PrintlnStr(InstrStr<'a>),
    While(InstrBool<'a>, Vec<InstrCntrl<'a>>),
    Block(Vec<InstrCntrl<'a>>),
    Return(Instr<'a>),
    Drop(Instr<'a>),
    Push(usize, Instr<'a>),
}

impl<'a> InstrCntrl<'a> {
    pub fn new(source: &'a str, kind: InstrKindCntrl<'a>) -> Self {
        Self {
            source,
            kind: Box::new(kind),
        }
    }
}

pub type Str = Rc<str>;
