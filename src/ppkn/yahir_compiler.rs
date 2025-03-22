#![allow(unused)]

use std::collections::HashMap;
use std::rc::Rc;

use indexmap::IndexMap;
use indexmap::IndexSet;

use super::yahir;
use super::yahir::Function;

use super::yalir;
use super::yalir::instr;
use super::yalir::*;

const DEBUG_LOGGING: bool = false;

pub fn lower_hir_to_lir(program: &yahir::Program) -> yalir::Program {
    HirToLirLowerer::new().lower(program)
}

struct HirToLirLowerer {
    queue: IndexSet<Rc<str>>,
    imports: IndexSet<Import>,
    funcs: IndexMap<Rc<str>, Func>,
    globals: IndexMap<Rc<str>, GlobalVariable>,
    data: Vec<u8>,
}

struct FnLowerer<'a> {
    program_lowerer: &'a mut HirToLirLowerer,
    function: &'a Function,
    locals_i32: Locals,
    locals_f32: Locals,
}

struct Locals {
    slots_count: u32,
    // TODO: try to use u32 here instead of Vec<u32>
    free_slots: Vec<u32>,
    used_slots: Vec<u32>,
    vars: HashMap<Rc<str>, u32>,
    shadowed_vars: Vec<(Rc<str>, u32)>,
    scopes: Vec<(usize, usize)>,
}

impl HirToLirLowerer {
    fn new() -> Self {
        Self {
            queue: IndexSet::new(),
            imports: IndexSet::new(),
            funcs: IndexMap::new(),
            globals: IndexMap::new(),
            data: vec![],
        }
    }

    fn lower(mut self, program: &yahir::Program) -> yalir::Program {
        self.queue.extend(program.exports.iter().cloned());
        let mut queue_start = 0;
        while queue_start < self.queue.len() {
            if DEBUG_LOGGING {
                println!("{}", self.queue[queue_start]);
            }
            let f = &program.fns[&self.queue[queue_start]];
            queue_start += 1;
            if DEBUG_LOGGING {
                println!(
                    "func {} {:?} -> {:?}",
                    f.name, f.signature.params, f.signature.result
                );
            }
            let func = FnLowerer::new(&mut self, f).lower();
            self.funcs.insert(f.name.clone(), func);
        }
        Program {
            imports: self.imports,
            funcs: self.funcs,
            globals: self.globals,
            data: self.data,
        }
    }

    fn lower_type(&self, typ: &yahir::Type) -> &'static [ValueType] {
        match typ {
            yahir::Type::Unit => &[],
            yahir::Type::Bool => &[ValueType::I32],
            yahir::Type::I32 => &[ValueType::I32],
            yahir::Type::U32 => &[ValueType::I32],
            yahir::Type::F32 => &[ValueType::F32],
            yahir::Type::Str => &[ValueType::I32],
            yahir::Type::Array(_) => &[ValueType::I32],
        }
    }
}

impl<'a> FnLowerer<'a> {
    fn new(lowerer: &'a mut HirToLirLowerer, function: &'a yahir::Function) -> Self {
        Self {
            program_lowerer: lowerer,
            locals_i32: Locals::new(),
            locals_f32: Locals::new(),
            function,
        }
    }

    fn lower(mut self) -> yalir::Func {
        for (param, typ) in self
            .function
            .parameters
            .iter()
            .zip(&self.function.signature.params)
        {
            match typ {
                yahir::Type::Unit => {}
                yahir::Type::Bool => _ = self.locals_i32.push(param.clone()),
                yahir::Type::I32 => _ = self.locals_i32.push(param.clone()),
                yahir::Type::U32 => _ = self.locals_i32.push(param.clone()),
                yahir::Type::F32 => _ = self.locals_f32.push(param.clone()),
                yahir::Type::Str => _ = self.locals_i32.push(param.clone()),
                yahir::Type::Array(_) => _ = self.locals_i32.push(param.clone()),
            }
        }
        let signature = FuncType {
            parameters: self
                .function
                .signature
                .params
                .iter()
                .flat_map(|t| self.program_lowerer.lower_type(t))
                .copied()
                .collect(),
            result: self
                .program_lowerer
                .lower_type(&self.function.signature.result)
                .to_vec(),
        };

        let mut code: Vec<Instr> = vec![];
        self.lower_expr(&self.function.body, &mut code);
        code.push(instr::END);
        Func {
            signature,
            i32_locals: self.locals_i32.len(),
            f32_locals: self.locals_f32.len(),
            code,
        }
    }

    fn lower_expr(&mut self, expr: &yahir::Expr<yahir::Type>, code: &mut Vec<Instr>) {
        if DEBUG_LOGGING {
            println!(" -> {:?}", expr);
        }
        use yahir::ExprKind::*;
        match &expr.kind {
            Value(literal) => match expr.typ {
                yahir::Type::Unit => {}
                yahir::Type::Bool => {
                    code.push(instr::I32_CONST);
                    code.extend_from_slice(&(literal.as_bool() as u32).to_ne_bytes());
                }
                yahir::Type::I32 => {
                    code.push(instr::I32_CONST);
                    code.extend_from_slice(&literal.as_i32().to_ne_bytes());
                }
                yahir::Type::U32 => {
                    code.push(instr::U32_CONST);
                    code.extend_from_slice(&literal.as_u32().to_ne_bytes());
                }
                yahir::Type::F32 => {
                    code.push(instr::F32_CONST);
                    code.extend_from_slice(&literal.as_f32().to_ne_bytes());
                }
                yahir::Type::Str => {
                    let string = literal.as_str();
                    let ptr = self.program_lowerer.data.len() as u32;
                    self.program_lowerer
                        .data
                        .extend_from_slice(&1u32.to_le_bytes());
                    self.program_lowerer
                        .data
                        .extend_from_slice(&(string.len() as u32).to_le_bytes());
                    self.program_lowerer
                        .data
                        .extend_from_slice(string.as_bytes());
                    code.push(instr::I32_CONST);
                    code.extend_from_slice(&ptr.to_ne_bytes());
                }
                yahir::Type::Array(_) => unreachable!(),
            },
            DefLocal(variable, expr) => {
                self.lower_expr(expr, code);
                match expr.typ {
                    yahir::Type::Unit => {}
                    yahir::Type::F32 => {
                        code.push(instr::LOCAL_SET);
                        code.extend_from_slice(
                            &self.locals_f32.push(variable.clone()).to_ne_bytes(),
                        );
                    }
                    _ => {
                        code.push(instr::LOCAL_SET);
                        code.extend_from_slice(
                            &self.locals_i32.push(variable.clone()).to_ne_bytes(),
                        );
                    }
                }
            }
            SetLocal(variable, expr) => {
                self.lower_expr(expr, code);
                match expr.typ {
                    yahir::Type::Unit => {}
                    yahir::Type::F32 => {
                        code.push(instr::LOCAL_SET);
                        code.extend_from_slice(&self.locals_f32.find(&variable).to_ne_bytes());
                    }
                    _ => {
                        code.push(instr::LOCAL_SET);
                        code.extend_from_slice(&self.locals_i32.find(&variable).to_ne_bytes());
                    }
                }
            }
            GetLocal(variable) => match &expr.typ {
                yahir::Type::Unit => {}
                yahir::Type::F32 => {
                    code.push(instr::LOCAL_GET);
                    code.extend_from_slice(&self.locals_f32.find(&variable).to_ne_bytes());
                }
                _ => {
                    code.push(instr::LOCAL_GET);
                    code.extend_from_slice(&self.locals_i32.find(&variable).to_ne_bytes());
                }
            },
            Block(block) => self.lower_block(block, code),
            While(condition, body) => {
                code.push(instr::BLOCK);
                code.push(instr::LOOP);
                self.lower_expr(condition, code);
                code.push(instr::JUMP_IF);
                code.extend_from_slice(&1u32.to_ne_bytes());
                self.lower_expr(body, code);
                code.push(instr::JUMP);
                code.extend_from_slice(&0u32.to_ne_bytes());
                code.push(instr::END);
                code.push(instr::END);
            }
            If(condition, then, otherwise) => {
                self.lower_expr(condition, code);
                code.push(instr::IF_THEN);
                self.lower_expr(then, code);
                if let Some(otherwise) = otherwise {
                    code.push(instr::ELSE);
                    self.lower_expr(otherwise, code);
                }
                code.push(instr::END);
            }
            MethodCall(obj, method, args) => {
                self.lower_expr(obj, code);
                args.iter().for_each(|arg| self.lower_expr(arg, code));
                use yahir::Type::*;
                match (&obj.typ, &method[..]) {
                    (I32, "add") => code.push(instr::I32_ADD),
                    (I32, "sub") => code.push(instr::I32_SUB),
                    (I32, "mul") => code.push(instr::I32_MUL),
                    (I32, "div") => code.push(instr::I32_DIV),
                    _ => panic!("Unknown method {} of {:?}", method, obj.typ),
                }
            }
            FnCall(function, args) => match &function[..] {
                "println" => {
                    for (i, arg) in args.iter().enumerate() {
                        self.lower_expr(arg, code);
                        if arg.typ != yahir::Type::Str {
                            code.push(instr::CALL_FUNC);
                            let f = match &arg.typ {
                                yahir::Type::Unit => unreachable!(),
                                yahir::Type::Bool => "bool_to_str",
                                yahir::Type::I32 => "i32_to_str",
                                yahir::Type::U32 => "u32_to_str",
                                yahir::Type::F32 => "f32_to_str",
                                yahir::Type::Str => unreachable!(),
                                yahir::Type::Array(_) => todo!(),
                            }
                            .into();
                            let f = self.program_lowerer.queue.insert_full(f).0;
                            code.extend_from_slice(&(f as u32).to_ne_bytes());
                        }
                        if i != 0 {
                            self.add_strings(code);
                        }
                    }
                    code.push(instr::CALL_IMPORT);
                    let signature = FuncType {
                        parameters: vec![ValueType::I32],
                        result: vec![ValueType::ExternRef],
                    };
                    let import = Import {
                        signature,
                        namespace: "std".into(),
                        func_name: "string".into(),
                    };
                    let f = self.program_lowerer.imports.insert_full(import).0;
                    code.extend_from_slice(&(f as u32).to_ne_bytes());

                    code.push(instr::CALL_IMPORT);
                    let signature = FuncType {
                        parameters: vec![ValueType::ExternRef],
                        result: vec![],
                    };
                    let import = Import {
                        signature,
                        namespace: "std".into(),
                        func_name: "println".into(),
                    };
                    let f = self.program_lowerer.imports.insert_full(import).0;
                    code.extend_from_slice(&(f as u32).to_ne_bytes());
                }
                _ => {
                    args.iter().for_each(|arg| self.lower_expr(arg, code));
                    code.push(instr::CALL_FUNC);
                    let f = self.program_lowerer.queue.insert_full(function.clone()).0;
                    code.extend_from_slice(&(f as u32).to_ne_bytes());
                }
            },
            Return(value) => {
                self.lower_expr(value, code);
                code.push(instr::RETURN);
            }
            Unreachable => {
                code.push(instr::UNREACHABLE);
            }
        }
        if DEBUG_LOGGING {
            println!(" :: {:X?}", code);
        }
    }

    fn lower_block(&mut self, block: &yahir::Block<yahir::Type>, code: &mut Vec<Instr>) {
        self.locals_i32.push_scope();
        self.locals_f32.push_scope();
        for stmt in &block.stmts {
            match stmt {
                yahir::Stmt::Expr(expr) => {
                    self.lower_expr(expr, code);
                    if expr.typ != yahir::Type::Unit {
                        code.push(instr::DROP);
                    }
                }
                yahir::Stmt::Def(variable, expr) => {
                    self.lower_expr(expr, code);
                    match expr.typ {
                        yahir::Type::Unit => {}
                        yahir::Type::F32 => {
                            code.push(instr::LOCAL_SET);
                            code.extend_from_slice(
                                &self.locals_f32.push(variable.clone()).to_ne_bytes(),
                            );
                        }
                        _ => {
                            code.push(instr::LOCAL_SET);
                            code.extend_from_slice(
                                &self.locals_i32.push(variable.clone()).to_ne_bytes(),
                            );
                        }
                    }
                }
            }
        }
        self.locals_i32.drop_scope();
        self.locals_f32.drop_scope();
    }

    fn add_strings(&mut self, code: &mut Vec<Instr>) {
        let str_add = self.program_lowerer.queue.insert_full("str.add".into()).0;
        code.push(instr::CALL_FUNC);
    }
}

impl Locals {
    fn new() -> Self {
        Self {
            scopes: vec![],
            slots_count: 0,
            free_slots: vec![],
            used_slots: vec![],
            shadowed_vars: vec![],
            vars: HashMap::new(),
        }
    }

    fn push_scope(&mut self) {
        self.scopes
            .push((self.used_slots.len(), self.shadowed_vars.len()));
    }

    fn push(&mut self, variable: Rc<str>) -> u32 {
        let location = self.free_slots.pop().unwrap_or_else(|| {
            let free_slot = self.slots_count;
            self.slots_count += 1;
            free_slot
        });
        if let Some(shadowed_location) = self.vars.insert(variable.clone(), location) {
            self.shadowed_vars.push((variable, shadowed_location));
        }
        self.used_slots.push(location);
        location
    }

    fn find(&self, variable: &str) -> u32 {
        self.vars[variable]
    }

    fn drop_scope(&mut self) {
        let (slots, shadows) = self.scopes.pop().unwrap();
        while slots < self.used_slots.len() {
            self.free_slots.push(self.used_slots.pop().unwrap());
        }
        while shadows < self.shadowed_vars.len() {
            let (name, idx) = self.shadowed_vars.pop().unwrap();
            self.vars.insert(name, idx);
        }
    }

    fn len(&self) -> usize {
        self.slots_count as usize
    }
}
