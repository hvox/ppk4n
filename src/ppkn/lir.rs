use indexmap::{IndexMap, IndexSet};
use std::{
    collections::{HashMap, VecDeque},
    rc::Rc,
};

use super::hir::*;

const LOGGING: bool = false;

#[derive(Debug, Clone, PartialEq)]
pub struct Bytecode {
    pub sources: HashMap<Str, Str>,
    pub types: IndexSet<FuncType>,
    pub imports: IndexMap<Str, Import>,
    pub globals: Vec<GlobalVariable>,
    pub functions: IndexMap<Str, Func>,
    pub data: Vec<u8>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Import {
    pub signature: FuncType,
    pub namespace: Str,
    pub func_name: Str,
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Func {
    pub module: Str,
    pub signature: FuncType,
    pub locals: Vec<ValueType>,
    pub code: Vec<Op>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub struct FuncType {
    pub parameters: Vec<ValueType>,
    pub results: Vec<ValueType>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GlobalVariable {
    pub typ: ValueType,
    pub mutable: bool,
    pub value: u64,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum BlockType {
    ValueType(ValueType),
    TypeIndex(usize),
    Void,
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

type Str = Rc<str>;

impl Program {
    pub fn to_lir(&self) -> Bytecode {
        ProgramCompiler::new(self).generate_lir()
    }
}

struct ProgramCompiler<'a> {
    program: &'a Program,
    lir: Bytecode,
    queue: VecDeque<Str>,
    globals: Variables,
}

#[derive(Default)]
struct Variables {
    valtypes: Vec<ValueType>,
    locations: HashMap<Str, Vec<usize>>,
    shadowed: Vec<(Str, Vec<usize>)>,
    vacant_i32: Vec<usize>,
    vacant_f32: Vec<usize>,
    vacant_i64: Vec<usize>,
    vacant_f64: Vec<usize>,
}

impl<'a> ProgramCompiler<'a> {
    fn new(program: &'a Program) -> Self {
        assert!(!program.is_poisoned());
        let main = program.main.to_string() + ":main";
        let queue = VecDeque::new();
        let lir = Bytecode {
            types: IndexSet::new(),
            imports: IndexMap::new(),
            sources: program.sources.clone(),
            globals: vec![],
            functions: IndexMap::new(),
            data: Vec::new(),
        };
        // program.functions.iter().for_each(|(f, _)| eprintln!("{}", f));
        let mut compiler = ProgramCompiler { program, lir, queue, globals: Variables::default() };
        compiler.queue_function(&main);
        compiler
    }

    fn generate_lir(mut self) -> Bytecode {
        while let Some(name) = self.queue.pop_front() {
            if LOGGING {
                eprintln!("> Compiling {}", name);
            }
            self.process_function(name);
        }
        self.lir
    }

    fn process_function(&mut self, fname: Str) {
        let function = &self.program.functions[&fname];
        let bytecode = FunctionCompiler::new(self, function).compile();
        self.lir.functions.insert(fname, bytecode);
    }

    fn queue_function(&mut self, fname: &str) -> Op {
        // println!("queue {}", fname);
        if let Some(idx) = self.lir.functions.get_index_of(fname) {
            Op::CallFunc(idx)
        } else if let Some(idx) = self.lir.imports.get_index_of(fname) {
            return Op::CallImport(idx);
        } else if let Some(import) = self.program.imports.get(fname) {
            let mut parameters = vec![];
            for (_, typ) in &import.parameters {
                let typ = self.process_type(typ);
                parameters.extend(&typ);
            }
            let signature = FuncType { parameters, results: self.process_type(&import.result) };
            let (namespace, func_name) = fname.split_once(':').unwrap();
            let import =
                Import { signature, namespace: namespace.into(), func_name: func_name.into() };
            let (idx, _) = self.lir.imports.insert_full(fname.into(), import);
            return Op::CallImport(idx);
        } else {
            let function = Str::from(fname);
            self.queue.push_back(function.clone());
            let (idx, _) = self.lir.functions.insert_full(function, Func::default());
            return Op::CallFunc(idx);
        }
    }

    fn queue_global(&mut self, global: &str) -> Vec<usize> {
        if let Some(layout) = self.globals.find(global) {
            return layout;
        }
        let (typ, body) = &self.program.globals[global];
        let typ = self.process_type(typ);
        for global_type in &typ {
            self.lir.globals.push(GlobalVariable { typ: *global_type, mutable: true, value: 0 });
        }
        let layout = self.globals.insert(global.into(), typ);
        let values = body
            .as_ref()
            .map(|expr| self.process_const_expr(expr))
            .unwrap_or_else(|| vec![0; layout.len()]);
        for (&i, value) in layout.iter().zip(values) {
            self.lir.globals[i].value = value;
        }
        layout
    }

    fn process_const_expr(&mut self, expr: &Body) -> Vec<u64> {
        use InstrKind::*;
        match &expr.value.kind {
            String(string) => {
                let start = self.lir.data.len() as u64;
                self.lir.data.extend(string.as_bytes());
                let length = string.len() as u64;
                vec![start, length]
            }
            Integer(value) => vec![value.parse().unwrap()],
            _ => unimplemented!("Constant evaluation of {:?}", expr),
        }
    }

    fn process_type(&mut self, typ: &Type) -> Vec<ValueType> {
        match typ {
            Type::Tuple(items) => {
                let mut flattened = vec![];
                for item in items {
                    flattened.extend(self.process_type(item));
                }
                flattened
            }
            Type::Array(_) => vec![ValueType::I32, ValueType::I32, ValueType::I32],
            Type::Name(typ) => match &typ[..] {
                "str" => vec![ValueType::I32, ValueType::I32],
                "i32" | "u32" | "i16" | "u16" | "i8" | "u8" => vec![ValueType::I32],
                "i64" | "u64" => vec![ValueType::I64],
                "f32" => vec![ValueType::F32],
                "f64" => vec![ValueType::F64],
                typ if self.program.types.get(typ).is_some_and(|t| *t == NamedType::Foreign) => {
                    vec![ValueType::ExternRef]
                }
                _ => todo!("Type {}", typ),
            },
            Type::Void => vec![],
        }
    }
}

impl Variables {
    fn insert(&mut self, name: Str, layout: Vec<ValueType>) -> Vec<usize> {
        // eprintln!("define {:?}: {:?}", name, layout);
        use ValueType::*;
        let old_layout = self.locations.remove(&name).unwrap_or_default();
        self.shadowed.push((name.clone(), old_layout));
        let mut new = |valtype| {
            let position = self.valtypes.len();
            self.valtypes.push(valtype);
            position
        };
        let mut slots = Vec::with_capacity(layout.len());
        for typ in layout {
            slots.push(match &typ {
                I32 => self.vacant_i32.pop().unwrap_or_else(|| new(typ)),
                I64 => self.vacant_i64.pop().unwrap_or_else(|| new(typ)),
                F32 => self.vacant_f32.pop().unwrap_or_else(|| new(typ)),
                F64 => self.vacant_f64.pop().unwrap_or_else(|| new(typ)),
                ExternRef => todo!(),
            });
        }
        self.locations.insert(name.clone(), slots.clone());
        slots
    }

    fn reset_shadowed(&mut self, count: usize) {
        while self.shadowed.len() > count {
            let (name, layout) = self.shadowed.pop().unwrap();
            // eprintln!("delete {:?}", name);
            if layout.is_empty() {
                self.locations.remove(&name);
            } else {
                self.locations.insert(name, layout);
            }
        }
    }

    fn find(&self, name: &str) -> Option<Vec<usize>> {
        self.locations.get(name).cloned()
    }
}

struct FunctionCompiler<'c, 'p> {
    function: &'p Function,
    ctx: &'c mut ProgramCompiler<'p>,
    locals: Variables,
    results: Vec<ValueType>,
}

impl<'a, 'p> FunctionCompiler<'a, 'p> {
    fn new(ctx: &'a mut ProgramCompiler<'p>, function: &'p Function) -> Self {
        let results = ctx.process_type(&function.result);
        Self { ctx, locals: Variables::default(), results, function }
    }

    fn compile(mut self) -> Func {
        let mut parameters = vec![];
        for (name, typ) in &self.function.parameters {
            let typ = self.ctx.process_type(typ);
            parameters.extend(&typ);
            self.locals.insert(name.clone(), typ);
        }
        let mut code = vec![];
        self.compile_instr(&self.function.body.value, &mut code);
        code.push(Op::End);
        let locals = self.locals.valtypes;
        let signature = FuncType { parameters, results: self.results };
        Func { module: self.function.module.clone(), signature, locals, code }
    }

    fn compile_instr(&mut self, instr: &Instr, code: &mut Vec<Op>) {
        use InstrKind::*;
        match &instr.kind {
            String(string) => {
                let string = string.as_bytes();
                let ptr = self.ctx.lir.data.len() as u32;
                let length = string.len() as u32;
                self.ctx.lir.data.extend(string);
                let clone = self.ctx.queue_function("std:clone_str");
                code.extend([Op::U32Const(ptr), Op::U32Const(length), clone]);
            }
            Integer(source) => code.push(match self.function.body.types.realize(instr.typ) {
                Type::Name(name) => match &name[..] {
                    "u8" | "u16" | "u32" => Op::U32Const(source.parse().unwrap()),
                    "i8" | "i16" | "i32" => Op::I32Const(source.parse().unwrap()),
                    "u64" => Op::U64Const(source.parse().unwrap()),
                    "i64" => Op::I64Const(source.parse().unwrap()),
                    _ => unreachable!(),
                },
                _ => unreachable!(),
            }),
            Tuple(fields) => fields.iter().for_each(|x| self.compile_instr(x, code)),
            Assignment(name, instr) => {
                self.compile_instr(instr, code);
                if let Some(layout) = self.locals.find(name) {
                    layout.iter().rev().for_each(|&x| code.push(Op::LocalSet(x)));
                } else {
                    let layout = self.ctx.globals.find(name).unwrap();
                    layout.iter().rev().for_each(|&x| code.push(Op::GlobalSet(x)));
                };
            }
            Identifier(name) => {
                if let Some(layout) = self.locals.find(name) {
                    layout.iter().for_each(|&x| code.push(Op::LocalGet(x)));
                } else {
                    let layout = self.ctx.queue_global(name);
                    layout.iter().for_each(|&x| code.push(Op::GlobalGet(x)));
                }
            }
            Block(block) => {
                let shadowed = self.locals.shadowed.len();
                for (target, instr, _) in &block.stmts {
                    self.compile_instr(instr, code);
                    let typ = self.function.body.types.realize(instr.typ);
                    if let Some((name, _mutable)) = target {
                        let layout = self.locals.insert(name.clone(), self.ctx.process_type(&typ));
                        layout.iter().for_each(|&x| code.push(Op::LocalSet(x)));
                    } else {
                        self.compile_drop(&typ, code);
                    }
                }
                self.compile_instr(&block.result, code);
                self.locals.reset_shadowed(shadowed);
            }
            While(condition, body) => {
                let mut inner_code = vec![];
                self.compile_instr(condition, &mut inner_code);
                inner_code.push(Op::U32Eqz);
                inner_code.push(Op::BrIf(1));
                self.compile_instr(body, &mut inner_code);
                inner_code.push(Op::Br(0));
                code.push(Op::Block(BlockType::Void, inner_code.len() + 2));
                code.push(Op::Loop(BlockType::Void, inner_code.len()));
                code.extend(inner_code);
                code.push(Op::End);
                code.push(Op::End);
            }
            If(condition, then, otherwise) => {
                self.compile_instr(condition, code);
                let typ = &self.function.body.types.realize(instr.typ);
                let valuetype = self.ctx.process_type(typ);
                let blocktype = match valuetype.len() {
                    0 => BlockType::Void,
                    1 => BlockType::ValueType(valuetype[0]),
                    _ => {
                        let functype =
                            FuncType { parameters: Vec::new(), results: valuetype.clone() };
                        let (index, _) = self.ctx.lir.types.insert_full(functype);
                        BlockType::TypeIndex(index)
                    }
                };
                let mut then_code = vec![];
                self.compile_instr(then, &mut then_code);
                code.push(Op::IfThen(blocktype, then_code.len()));
                code.extend(then_code);
                if otherwise.kind != InstrKind::NoOp {
                    let mut else_code = vec![];
                    self.compile_instr(then, &mut else_code);
                    code.push(Op::Else(else_code.len()));
                    code.extend(else_code);
                }
                code.push(Op::EndIf);
            }
            MethodCall(receiver, method, args) => {
                let src = instr.span.0 as usize;
                let typ = self.function.body.types.realize(receiver.typ);
                self.compile_instr(receiver, code);
                args.iter().for_each(|x| self.compile_instr(x, code));
                match &typ {
                    Type::Tuple(_) => unreachable!(),
                    Type::Array(_) => unreachable!(),
                    Type::Name(receiver) => match (&receiver[..], &method[..]) {
                        ("i32", "add") => code.push(Op::I32Add(src)),
                        ("i32", "sub") => code.push(Op::I32Sub(src)),
                        ("i32", "mul") => code.push(Op::I32Mul(src)),
                        ("i32", "div") => code.push(Op::I32Div(src)),
                        ("i32", "rem") => code.push(Op::I32Rem(src)),
                        ("i32", "shl") => code.push(Op::I32Shl(src)),
                        ("i32", "shr") => code.push(Op::I32Shr(src)),
                        ("i32", "eqz") => code.push(Op::I32Eqz),
                        ("i32", "eq") => code.push(Op::I32Eq),
                        ("i32", "ge") => code.push(Op::I32Ge),
                        ("i32", "gt") => code.push(Op::I32Gt),
                        ("i32", "le") => code.push(Op::I32Le),
                        ("i32", "lt") => code.push(Op::I32Lt),
                        ("i32", "ne") => code.push(Op::I32Ne),

                        ("u32", "add") => code.push(Op::U32Add(src)),
                        ("u32", "sub") => code.push(Op::U32Sub(src)),
                        ("u32", "mul") => code.push(Op::U32Mul(src)),
                        ("u32", "div") => code.push(Op::U32Div(src)),
                        ("u32", "rem") => code.push(Op::U32Rem(src)),
                        ("u32", "shl") => code.push(Op::U32Shl(src)),
                        ("u32", "shr") => code.push(Op::U32Shr(src)),
                        ("u32", "eqz") => code.push(Op::I32Eqz),
                        ("u32", "eq") => code.push(Op::U32Eq),
                        ("u32", "ge") => code.push(Op::U32Ge),
                        ("u32", "gt") => code.push(Op::U32Gt),
                        ("u32", "le") => code.push(Op::U32Le),
                        ("u32", "lt") => code.push(Op::U32Lt),
                        ("u32", "ne") => code.push(Op::U32Ne),

                        ("str", "add") => code.push(self.ctx.queue_function("std:str_add")),
                        x => todo!("{:?}", x),
                    },
                    Type::Void => unreachable!(),
                }
            }
            FnCall(name, args) => match &name[..] {
                "std:memory_copy" => {
                    args.iter().for_each(|arg| self.compile_instr(arg, code));
                    code.push(Op::MemoryCopy);
                }
                "println" => {
                    let mut string_in_stack = false;
                    for instr in args {
                        let typ = self.function.body.types.realize(instr.typ);
                        self.compile_instr(instr, code);
                        self.compile_stringify(&typ, code);
                        if string_in_stack {
                            code.push(self.ctx.queue_function("std:str_add"));
                        } else {
                            string_in_stack = true;
                        }
                    }
                    if !string_in_stack {
                        self.compile_string("", code);
                    }
                    code.push(self.ctx.queue_function("std:println"));
                }
                _ => {
                    args.iter().for_each(|arg| self.compile_instr(arg, code));
                    code.push(self.ctx.queue_function(name))
                }
            },
            Return(value) => {
                self.compile_instr(value, code);
                code.push(Op::Return);
            }
            Unreachable => code.push(Op::Unreachable),
            NoOp => {}
        }
    }

    fn compile_stringify(&mut self, typ: &Type, _code: &mut Vec<Op>) {
        match typ {
            Type::Tuple(_) => unreachable!(),
            Type::Array(_) => unreachable!(),
            Type::Name(typ) => match &typ[..] {
                "str" => {}
                _ => todo!("str({})", typ),
            },
            Type::Void => {}
        }
    }

    fn compile_drop(&mut self, typ: &Type, code: &mut Vec<Op>) {
        match typ {
            Type::Tuple(fields) => fields.iter().for_each(|typ| self.compile_drop(typ, code)),
            Type::Array(_) => todo!(),
            Type::Name(typ) => match &typ[..] {
                "i8" | "u8" | "i16" | "u16" | "i32" | "u32" | "f32" | "i64" | "u64" | "f64" => {
                    code.push(Op::Drop)
                }
                "str" => code.push(self.ctx.queue_function("std:free")),
                _ => todo!(),
            },
            Type::Void => {}
        }
    }

    fn compile_string(&mut self, string: &str, code: &mut Vec<Op>) {
        let string = string.as_bytes();
        let ptr = self.ctx.lir.data.len() as u32;
        let length = string.len() as u32;
        self.ctx.lir.data.extend(string);
        let clone = self.ctx.queue_function("std:clone_str");
        code.extend([Op::U32Const(ptr), Op::U32Const(length), clone]);
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq)]
pub enum Op {
    Unreachable,
    Nop,
    Block(BlockType, usize),
    Loop(BlockType, usize),
    IfThen(BlockType, usize),
    Else(usize),
    End,
    EndIf,
    Br(usize),
    BrIf(usize),
    JumpTable(Vec<usize>),
    Return,
    CallFunc(usize),
    CallImport(usize),
    CallIndirect(usize),
    Drop,
    Select(ValueType),
    GlobalGet(usize),
    GlobalSet(usize),
    LocalGet(usize),
    LocalSet(usize),
    LocalTee(usize),
    MemorySize,
    MemoryGrow,
    MemoryCopy,
    F32Const(f32),
    F32Load(usize),
    F32Store(usize),
    F32Abs,
    F32Ceil,
    F32Floor,
    F32Nearest,
    F32Neg,
    F32Sqrt,
    F32Trunc,
    F32Copysign,
    F32Add,
    F32Div,
    F32Max,
    F32Min,
    F32Mul,
    F32Sub,
    F32Eq,
    F32Ge,
    F32Gt,
    F32Le,
    F32Lt,
    F32Ne,
    I32Const(i32),
    I32Load(usize),
    I32Store(usize),
    I32And,
    I32Or,
    I32Xor,
    I32Add(usize),
    I32Div(usize),
    I32Mul(usize),
    I32Rem(usize),
    I32Rotl,
    I32Rotr,
    I32Shl(usize),
    I32Shr(usize),
    I32Sub(usize),
    I32Eqz,
    I32Eq,
    I32Ge,
    I32Gt,
    I32Le,
    I32Lt,
    I32Ne,
    U32Const(u32),
    U32Load(usize),
    U32Store(usize),
    U32And,
    U32Or,
    U32Xor,
    U32Add(usize),
    U32Div(usize),
    U32Mul(usize),
    U32Rem(usize),
    U32Rotl,
    U32Rotr,
    U32Shl(usize),
    U32Shr(usize),
    U32Sub(usize),
    U32Eqz,
    U32Eq,
    U32Ge,
    U32Gt,
    U32Le,
    U32Lt,
    U32Ne,
    F64Const(f64),
    F64Load(usize),
    F64Store(usize),
    F64Abs,
    F64Ceil,
    F64Floor,
    F64Nearest,
    F64Neg,
    F64Sqrt,
    F64Trunc,
    F64Copysign,
    F64Add,
    F64Div,
    F64Max,
    F64Min,
    F64Mul,
    F64Sub,
    F64Eq,
    F64Ge,
    F64Gt,
    F64Le,
    F64Lt,
    F64Ne,
    I64Const(i64),
    I64Load(usize),
    I64Store(usize),
    I64And,
    I64Or,
    I64Xor,
    I64Add(usize),
    I64Div(usize),
    I64Mul(usize),
    I64Rem(usize),
    I64Rotl,
    I64Rotr,
    I64Shl(usize),
    I64Shr(usize),
    I64Sub(usize),
    I64Eqz,
    I64Eq,
    I64Ge,
    I64Gt,
    I64Le,
    I64Lt,
    I64Ne,
    U64Const(u64),
    U64Load(usize),
    U64Store(usize),
    U64And,
    U64Or,
    U64Xor,
    U64Add(usize),
    U64Div(usize),
    U64Mul(usize),
    U64Rem(usize),
    U64Rotl,
    U64Rotr,
    U64Shl(usize),
    U64Shr(usize),
    U64Sub(usize),
    U64Eqz,
    U64Eq,
    U64Ge,
    U64Gt,
    U64Le,
    U64Lt,
    U64Ne,
}
