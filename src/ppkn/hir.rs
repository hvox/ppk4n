use std::{
    cell::Cell,
    collections::HashMap,
    fmt::Debug,
    fs,
    io::Write,
    ops::{Index, IndexMut},
    path::PathBuf,
    rc::Rc,
};

use super::{
    error::{Error, PpknErrorKind},
    parser::*,
};
use PpknErrorKind::*;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Program {
    poisoned: bool,
    pub main: Str,
    pub root: PathBuf,
    pub sources: HashMap<Str, Str>,
    pub modules: HashMap<Str, Module>,

    pub imports: HashMap<Str, FunctionSignature>,
    pub types: HashMap<Str, NamedType>,
    // TODO: Automatically create body based on Type
    pub globals: HashMap<Str, (Type, Option<Body>)>,
    pub functions: HashMap<Str, Function>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum NamedType {
    Foreign,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Module {
    pub name: Str,
    pub ast: Ast,
    pub dependents: Vec<Str>,
    pub poisoned: bool,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Function {
    pub module: Str,
    pub name: Str,
    pub parameters: Vec<(Str, Type)>,
    pub result: Type,
    pub body: Body,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FunctionSignature {
    pub module: Str,
    pub name: Str,
    pub parameters: Vec<(Str, Type)>,
    pub result: Type,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Body {
    pub types: Types,
    pub value: Instr,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Instr {
    pub location: u32,
    pub typ: TypeId,
    pub kind: InstrKind,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum InstrKind {
    String(Str),
    Integer(Str),
    Identifier(Str),
    Tuple(Vec<Instr>),
    Assignment(Str, Rc<Instr>),
    Block(Rc<Block>),
    While(Rc<Instr>, Rc<Instr>),
    If(Rc<Instr>, Rc<Instr>, Rc<Instr>),
    MethodCall(Rc<Instr>, Str, Vec<Instr>),
    FnCall(Str, Vec<Instr>),
    Return(Rc<Instr>),
    Unreachable,
    NoOp,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Block {
    pub stmts: Vec<(Option<(Str, bool)>, Instr)>,
    pub result: Instr,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Type {
    Tuple(Vec<Type>),
    Array(Rc<Type>),
    Name(Str),
    Void,
}

#[allow(dead_code)]
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum InferredType {
    Tuple(Vec<TypeId>),
    Array(TypeId),
    Name(Str),
    Void,
    Bool,
    Char,
    Str,
    Int(u8),
    Uint(u8),
    Float(u8),

    Unknown,
    UnkInt,
    UnkFloat,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Types {
    handles: Vec<Cell<u32>>,
    types: HashMap<u32, InferredType>,
    // TODO: replace these guys with constants
    void: TypeId,
    bool: TypeId,
    char: TypeId,
    string: TypeId,
}

#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub struct TypeId(u32);

type Str = Rc<str>;

impl Program {
    pub fn new<Path>(source_root_path: Path, sources: HashMap<Str, Str>, main: Str) -> Self
    where
        Path: Into<PathBuf>,
    {
        let root = source_root_path.into();
        let modules = HashMap::new();
        let globals = HashMap::new();
        let functions = HashMap::new();
        let imports = HashMap::new();
        let types = HashMap::new();
        let mut sources = sources;
        if !sources.contains_key("std") {
            let std_source = include_str!("std.ppkn");
            sources.insert("std".into(), std_source.into());
        }

        Self {
            poisoned: false,
            main,
            root,
            sources,
            modules,
            globals,
            functions,
            types,
            imports,
        }
    }

    pub fn load_and_typecheck(&mut self, name: Str) -> Result<(), Vec<Error>> {
        let (modules, errors) = self.load(name);
        if !errors.is_empty() {
            return Err(errors);
        }
        let mut errors = vec![];
        for module in modules {
            if let Err(new_errors) = self.typecheck_module(&module) {
                errors.extend(new_errors);
            }
        }
        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }

    pub fn load(&mut self, name: Str) -> (Vec<Str>, Vec<Error>) {
        let mut parse_queue = vec![
            (name.clone(), "".into(), (0, 0)),
            ("std".into(), name, (0, 0)),
        ];
        let mut parsed = vec![];
        let mut errors = vec![];
        while let Some((name, dependent, _location)) = parse_queue.pop() {
            if let Some(module) = self.modules.get_mut(&name) {
                module.dependents.push(name.clone());
                continue;
            }
            let source = match self.sources.get(&name) {
                Some(source) => source.clone(),
                None => {
                    let source_filename = format!("{}.ppkn", name);
                    let source_path = self.root.join(source_filename);
                    // TODO: handle NotFound error
                    let source = Str::from(fs::read_to_string(source_path).unwrap());
                    self.sources.insert(name.clone(), source.clone());
                    source
                }
            };
            let (ast, syntax_errors) = super::parser::parse(&source);
            errors.extend(
                syntax_errors
                    .into_iter()
                    .map(|error| error.into_error(name.clone())),
            );
            for dependency in &ast.dependencies {
                parse_queue.push((dependency.name.clone(), name.clone(), dependency.location));
            }
            let module = Module {
                name: name.clone(),
                ast,
                dependents: vec![dependent],
                poisoned: false,
            };
            self.modules.insert(name.clone(), module);
            parsed.push(name);
        }
        (parsed, errors)
    }

    pub fn typecheck_module(&mut self, module_name: &str) -> Result<(), Vec<Error>> {
        let mut errors = vec![];
        // update self.functions and self.globals
        let ast = &self.modules[module_name].ast;
        // let globals: HashMap<Str, &FunDef> = HashMap::new();
        // let functions: HashMap<Str, &FunDef> = HashMap::new();
        for typ in &ast.imported_types {
            self.types.insert(typ.clone(), NamedType::Foreign);
        }
        for (fname, function) in &ast.imported_functions {
            let name = function.name.clone();
            let params: Vec<_> = function
                .params
                .iter()
                .map(|x| (x.name.clone(), self.resolve_typename(module_name, &x.typ)))
                .collect();
            let result = self.resolve_typename(module_name, &function.result);
            let signature = FunctionSignature {
                module: module_name.into(),
                name,
                parameters: params,
                result,
            };
            self.imports.insert(fname.clone(), signature);
        }
        for (_, global) in &ast.globals {
            let name = global.name.clone();
            let typ = self.resolve_typename(module_name, &global.typename);
            let body = match &global.value {
                Some(expr) => {
                    let typechecker = BodyTypechecker::new(self, module_name, &typ, &[]);
                    let (body, new_errors) = typechecker.typecheck_body(expr);
                    errors.extend(new_errors);
                    Some(body)
                }
                None => None,
            };
            let path = format!("{}:{}", module_name, name).into();
            self.globals.insert(path, (typ, body));
        }
        for (_, function) in &ast.functions {
            let name = function.name.clone();
            let params: Vec<_> = function
                .params
                .iter()
                .map(|x| (x.name.clone(), self.resolve_typename(module_name, &x.typ)))
                .collect();
            let result = self.resolve_typename(module_name, &function.result);
            let typechecker = BodyTypechecker::new(self, module_name, &result, &params[..]);
            let (body, new_errors) = typechecker.typecheck_body(&function.body);
            let function = Function {
                module: module_name.into(),
                name: name.clone(),
                parameters: params,
                result,
                body,
            };
            let path = format!("{}:{}", module_name, name).into();
            self.functions.insert(path, function);
            errors.extend(new_errors);
        }
        if errors.is_empty() {
            Ok(())
        } else {
            self.modules.get_mut(module_name).unwrap().poisoned = true;
            self.poisoned = true;
            Err(errors)
        }
    }

    fn resolve_typename(&self, module: &str, typename: &Typename) -> Type {
        _ = module;
        use TypenameKind::*;
        match &typename.kind {
            Tuple(field_names) => {
                let mut fields = vec![];
                for field in field_names {
                    fields.push(self.resolve_typename(module, field));
                }
                Type::Tuple(fields)
            }
            Array(item) => {
                let item = self.resolve_typename(module, item);
                Type::Array(item.into())
            }
            Name(name) => Type::Name(name.clone()),
            Unknown => Type::Void,
            Void => Type::Void,
        }
    }

    pub fn is_poisoned(&self) -> bool {
        self.poisoned
    }

    pub fn to_bytes(&self) -> Vec<u8> {
        let mut bytes = vec![];
        bytes
            .write_all(&(self.main.len() as u32).to_le_bytes())
            .unwrap();
        bytes.write_all(self.main.as_bytes()).unwrap();
        bytes
            .write_all(&(self.sources.len() as u32).to_le_bytes())
            .unwrap();
        for (module, source) in &self.sources {
            bytes
                .write_all(&(module.len() as u32).to_le_bytes())
                .unwrap();
            bytes.write_all(module.as_bytes()).unwrap();
            bytes
                .write_all(&(source.len() as u32).to_le_bytes())
                .unwrap();
            bytes.write_all(source.as_bytes()).unwrap();
        }
        bytes
    }

    pub fn from_bytes(bytes: &[u8]) -> Self {
        let mut i;
        let mut sources = HashMap::new();
        let main_len = u32::from_le_bytes(bytes[0..4].try_into().unwrap()) as usize;
        let main = String::from_utf8_lossy(&bytes[4..4 + main_len]);
        i = 4 + main_len;
        let sources_count = u32::from_le_bytes(bytes[i..i + 4].try_into().unwrap()) as usize;
        i += 4;
        for _ in 0..sources_count {
            let length = u32::from_le_bytes(bytes[i..i + 4].try_into().unwrap()) as usize;
            let module = String::from_utf8_lossy(&bytes[i + 4..i + 4 + length]);
            i += 4 + length;
            let length = u32::from_le_bytes(bytes[i..i + 4].try_into().unwrap()) as usize;
            let source = String::from_utf8_lossy(&bytes[i + 4..i + 4 + length]);
            i += 4 + length;
            sources.insert(module.into(), source.into());
        }
        Self::new(PathBuf::new(), sources, main.into())
    }
}

impl Types {
    fn new() -> Self {
        use InferredType::*;
        let handles = vec![Cell::new(0), Cell::new(1), Cell::new(2), Cell::new(3)];
        let types = HashMap::from([(0, Void), (1, Bool), (2, Char), (3, Str)]);
        assert!(handles.len() == types.len());

        Self {
            handles,
            types,
            void: TypeId(0),
            bool: TypeId(1),
            char: TypeId(2),
            string: TypeId(3),
        }
    }

    fn insert_specified(&mut self, typ: &Type) -> TypeId {
        use InferredType::*;
        let typ = match typ {
            Type::Tuple(fields) => Tuple(fields.iter().map(|x| self.insert_specified(x)).collect()),
            Type::Array(item_type) => Array(self.insert_specified(item_type)),
            Type::Name(name) => match &name[..] {
                "bool" => return self.bool,
                "char" => return self.char,
                "str" => return self.string,
                "i32" => Int(32),
                "i64" => Int(64),
                "u32" => Uint(32),
                "u64" => Uint(64),
                "f32" => Float(32),
                "f64" => Float(64),
                _ => Name(name.clone()),
            },
            Type::Void => return self.void,
        };
        self.insert(typ)
    }

    fn insert(&mut self, typ: InferredType) -> TypeId {
        let id = self.handles.len() as u32;
        self.handles.push(Cell::new(id));
        self.types.insert(id, typ);
        TypeId(id)
    }

    fn unify(&mut self, u: TypeId, v: TypeId) -> Result<(), (InferredType, InferredType)> {
        let u_hndl = self.get_handle(u);
        let v_hndl = self.get_handle(v);
        if u_hndl == v_hndl {
            return Ok(());
        }

        // TODO: Try pop immediately instead of clone&remove
        let u_type = self.types[&u_hndl].clone();
        let v_type = self.types[&v_hndl].clone();
        use InferredType::*;
        let unified_type = match (u_type, v_type) {
            (Unknown, Unknown) => Unknown,
            (Unknown, x) => x,
            (x, Unknown) => x,
            (x, y) if x == y => x,
            (Int(x), UnkInt) => Int(x),
            (UnkInt, Int(x)) => Int(x),
            (Uint(x), UnkInt) => Uint(x),
            (UnkInt, Uint(x)) => Uint(x),
            (Float(x), UnkFloat) => Float(x),
            (UnkFloat, Float(x)) => Float(x),
            (Tuple(xs), Tuple(ys)) if xs.len() == ys.len() => {
                for (x, y) in xs.iter().zip(&ys) {
                    self.unify(*x, *y)
                        .map_err(|_| (Tuple(xs.clone()), Tuple(ys.clone())))?;
                }
                Tuple(xs)
            }
            (x, y) => return Err((x, y)),
        };
        let (u, v) = (u_hndl.min(v_hndl), v_hndl.max(u_hndl));
        self.types.remove(&u);
        self.types.remove(&v);
        self.handles[v as usize].set(u);
        self.types.insert(u, unified_type);
        Ok(())
    }

    fn get_handle(&self, id: TypeId) -> u32 {
        let id = id.0 as usize;
        let mut leader = self.handles[id].get();
        if self.handles[leader as usize].get() != leader {
            leader = self.get_handle(TypeId(leader));
            self.handles[id].set(leader);
        }
        leader
    }

    pub fn realize(&self, id: TypeId) -> Type {
        use InferredType::*;
        let typ = &self[id];
        match typ {
            Tuple(fields) => Type::Tuple(fields.iter().map(|&x| self.realize(x)).collect()),
            Array(_) => todo!(),
            Name(_) => todo!(),
            Void => Type::Void,
            Bool => Type::Name("bool".into()),
            Char => Type::Name("char".into()),
            Str => Type::Name("str".into()),
            Int(size) => Type::Name(format!("i{}", size).into()),
            Uint(size) => Type::Name(format!("u{}", size).into()),
            Float(size) => Type::Name(format!("f{}", size).into()),
            Unknown => Type::Void,
            UnkInt => Type::Name("i32".into()),
            UnkFloat => Type::Name("f64".into()),
        }
    }
}

impl Index<TypeId> for Types {
    type Output = InferredType;

    fn index(&self, id: TypeId) -> &Self::Output {
        &self.types[&(self.get_handle(id))]
    }
}

impl IndexMut<TypeId> for Types {
    fn index_mut(&mut self, id: TypeId) -> &mut Self::Output {
        self.types.get_mut(&(self.get_handle(id))).unwrap()
    }
}

struct BodyTypechecker<'a> {
    program: &'a Program,
    module: Str,
    result: TypeId,
    locals: Vec<(Str, TypeId)>,
    types: Types,
    errors: Vec<Error>,
}

impl<'a> BodyTypechecker<'a> {
    fn new(
        program: &'a Program,
        module: &'a str,
        result: &'a Type,
        params: &[(Str, Type)],
    ) -> Self {
        let mut types = Types::new();
        let result = types.insert_specified(result);
        let locals = if params.is_empty() {
            vec![]
        } else {
            let mut locals = vec![];
            for (name, typ) in params {
                let type_id = types.insert_specified(typ);
                locals.push((name.clone(), type_id));
            }
            locals
        };
        Self {
            program,
            module: module.into(),
            result,
            locals,
            types,
            errors: vec![],
        }
    }

    fn typecheck_body(mut self, expr: &Expr) -> (Body, Vec<Error>) {
        let instr = self.typecheck(expr, self.result);
        (
            Body {
                types: self.types,
                value: instr,
            },
            self.errors,
        )
    }

    fn typecheck(&mut self, expr: &Expr, typ: TypeId) -> Instr {
        use InstrKind::*;
        let loc = expr.location;
        let (kind, actual_type) = match &expr.kind {
            ExprKind::String(string) => (String(string.clone()), self.types.string),
            ExprKind::Integer(integer) => (
                Integer(integer.clone()),
                self.types.insert(InferredType::UnkInt),
            ),
            ExprKind::Identifier(name) => {
                let (path, typ) = self.vartype(loc, name.clone());
                (Identifier(path), typ)
            }
            ExprKind::Field(_object, _field_name) => {
                todo!()
            }
            ExprKind::Tuple(fields) => {
                let mut instrs = vec![];
                let mut types = vec![];
                for expr in fields {
                    let typ = self.types.insert(InferredType::Unknown);
                    let instr = self.typecheck(expr, typ);
                    instrs.push(instr);
                    types.push(typ);
                }
                let tuple_type = self.types.insert(InferredType::Tuple(types));
                (Tuple(instrs), tuple_type)
            }
            ExprKind::Assignment(name, expr) => {
                let (path, typ) = self.vartype(loc, name.clone());
                let value = self.typecheck(expr, typ);
                (Assignment(path, value.into()), self.types.void)
            }
            // ExprKind::GetLocal(name) => (GetLocal(name.clone()), self.vartype(loc, &name)),
            ExprKind::Block(block) => {
                let block = self.typecheck_block(block, typ);
                (InstrKind::Block(block.into()), typ)
            }
            ExprKind::While(condition, body) => {
                let condition = self.typecheck(condition, self.types.bool);
                let body = self.typecheck(body, self.types.void);
                (While(condition.into(), body.into()), self.types.void)
            }
            ExprKind::If(condition, then, otherwise) => {
                let condition = self.typecheck(condition, self.types.bool);
                let then = self.typecheck(then, typ);
                let otherwise = self.typecheck(otherwise, typ);
                (If(condition.into(), then.into(), otherwise.into()), typ)
            }
            ExprKind::MethodCall(receiver, method, args) => {
                let receiver_typ = self.types.insert(InferredType::Unknown);
                let receiver = self.typecheck(receiver, receiver_typ);
                if let Ok(mut signature) = self.resolve_method(receiver_typ, method) {
                    let result_type = signature.pop().unwrap();
                    let params = signature;
                    if params.len() == args.len() {
                        let args = args
                            .iter()
                            .zip(params)
                            .map(|(x, t)| self.typecheck(x, t))
                            .collect();
                        // let mut checked_args = vec![];
                        // for (arg, typ) in args.iter().zip(params) {
                        //     checked_args.push(self.typecheck(arg, typ));
                        // }
                        (
                            MethodCall(receiver.into(), method.clone(), args),
                            result_type,
                        )
                    } else {
                        let error_message = format!(
                            "this method takes {} arguments but {} were given",
                            params.len(),
                            args.len()
                        );
                        self.error(TypeError, loc, error_message);
                        // TODO: prettify
                        let args = args
                            .iter()
                            .map(|arg| {
                                let typ = self.types.insert(InferredType::Unknown);
                                self.typecheck(arg, typ)
                            })
                            .collect();
                        (
                            MethodCall(receiver.into(), method.clone(), args),
                            result_type,
                        )
                    }
                } else {
                    self.error(
                        NameError,
                        loc,
                        format!("method not found in {:?}", self.types[receiver_typ]),
                    );
                    let args = args
                        .iter()
                        .map(|arg| {
                            let typ = self.types.insert(InferredType::Unknown);
                            self.typecheck(arg, typ)
                        })
                        .collect();
                    (MethodCall(receiver.into(), method.clone(), args), typ)
                }
            }
            ExprKind::FnCall(function, args) => {
                if let Ok((path, params, result_type)) = self.resolve_function(function) {
                    let args = if params.len() == args.len() {
                        args.iter()
                            .zip(params)
                            .map(|(x, t)| self.typecheck(x, t))
                            .collect()
                    } else {
                        let error_message = format!(
                            "expected {} arguments but {} were given",
                            params.len(),
                            args.len()
                        );
                        self.error(TypeError, loc, error_message);
                        args.iter()
                            .map(|arg| {
                                let typ = self.types.insert(InferredType::Unknown);
                                self.typecheck(arg, typ)
                            })
                            .collect()
                    };
                    (FnCall(path, args), result_type)
                } else {
                    let typ = if !matches!(
                        function.as_ref(),
                        "print" | "println" | "eprint" | "eprintln"
                    ) {
                        self.error(NameError, loc, "function not found in this scope".into());
                        typ
                    } else {
                        self.types.void
                    };
                    let args = args
                        .iter()
                        .map(|arg| {
                            let typ = self.types.insert(InferredType::Unknown);
                            self.typecheck(arg, typ)
                        })
                        .collect();
                    (FnCall(function.clone(), args), typ)
                }
            }
            ExprKind::Return(value) => (Return(self.typecheck(value, self.result).into()), typ),
            ExprKind::Unreachable => (Unreachable, typ),
            ExprKind::Nothing => (NoOp, self.types.void),
        };
        if typ != actual_type {
            self.unify(expr.location, typ, actual_type);
        }
        Instr {
            location: expr.location.0,
            typ,
            kind,
        }
    }

    fn typecheck_block(&mut self, block: &Indented, result: TypeId) -> Block {
        let is_expr = self.types[result] != InferredType::Void;
        let locals_count = self.locals.len();
        let block_stmts = &block.stmts[..block.stmts.len() - is_expr as usize];
        let mut stmts = vec![];
        for stmt in block_stmts {
            match stmt {
                Stmt::DefLocal(vardef) => {
                    let name = vardef.name.clone();
                    let typ = self.resolve_typename(&vardef.typename);
                    self.locals.push((name.clone(), typ));
                    let expr = self.typecheck(&vardef.value.clone().unwrap(), typ);
                    stmts.push((Some((name, vardef.mutable)), expr));
                }
                Stmt::Expression(expr) => {
                    let typ = match expr.kind {
                        ExprKind::Block(_) | ExprKind::If(_, _, _) => self.types.void,
                        _ => self.types.insert(InferredType::Unknown),
                    };
                    stmts.push((None, self.typecheck(expr, typ)));
                }
            }
        }
        let result = if self.types[result] == InferredType::Void {
            Instr {
                location: block.location.0,
                typ: self.types.void,
                kind: InstrKind::NoOp,
            }
        } else if let Some(stmt) = block.stmts.last() {
            match stmt {
                Stmt::Expression(expr) => self.typecheck(expr, result),
                Stmt::DefLocal(vardef) => {
                    let name = vardef.name.clone();
                    let location = vardef.location.0;
                    let typ = self.resolve_typename(&vardef.typename);
                    let expr = self.typecheck(&vardef.value.clone().unwrap(), typ);
                    stmts.push((Some((name, vardef.mutable)), expr));
                    self.unify(vardef.location, result, self.types.void);
                    Instr {
                        location,
                        typ: self.types.void,
                        kind: InstrKind::NoOp,
                    }
                }
            }
        } else {
            self.unify(block.location, result, self.types.void);
            Instr {
                location: block.location.0,
                typ: self.types.void,
                kind: InstrKind::NoOp,
            }
        };
        self.locals.truncate(locals_count);
        Block { stmts, result }
    }

    fn resolve_function(&mut self, function: &str) -> Result<(Str, Vec<TypeId>, TypeId), ()> {
        let (module, fname) = if let Some(i) = function.find(':') {
            (&function[..i], &function[i + 1..])
        } else {
            (&self.module[..], function)
        };
        let Some(f) = &self
            .program
            .modules
            .get(module)
            .and_then(|m| m.ast.functions.get(fname))
        else {
            let Some(signature) = &self.program.imports.get(function) else {
                return Err(());
            };
            let params = signature
                .parameters
                .iter()
                .map(|x| self.types.insert_specified(&x.1))
                .collect();
            let result = self.types.insert_specified(&signature.result);
            return Ok((function.into(), params, result));
        };
        let params = f
            .params
            .iter()
            .map(|x| {
                self.types
                    .insert_specified(&self.program.resolve_typename(module, &x.typ))
            })
            .collect();
        let result = self
            .types
            .insert_specified(&self.program.resolve_typename(module, &f.result));
        let path = format!("{}:{}", module, fname).into();
        Ok((path, params, result))
    }

    fn resolve_method(&mut self, receiver: TypeId, method: &str) -> Result<Vec<TypeId>, ()> {
        use InferredType::*;
        let t = receiver;
        // eprintln!("{:?}", t);
        // eprintln!("{:?}", self.types.handles);
        // eprintln!("{:?}", self.types.types);
        let signature = match (&self.types[t], method) {
            (Int(_) | Uint(_) | Float(_) | UnkInt | UnkFloat, "add" | "sub" | "mul" | "div") => {
                vec![t, t]
            }
            (Int(_) | Uint(_) | UnkInt | Bool, "bitadd" | "bitor" | "bitxor") => vec![t, t],
            (Int(_) | Uint(_) | UnkInt, "div_floor" | "rem") => vec![t, t],
            (Str, "add") => vec![t, t],
            (Str, "push") => vec![self.types.char, self.types.void],
            (_, "eq" | "ne" | "lt" | "le" | "gt" | "ge") => vec![t, self.types.bool],
            _ => return Err(()),
        };
        Ok(signature)
    }

    fn unify(&mut self, location: (u32, u32), expected_type: TypeId, actual_type: TypeId) {
        let Err((dst, src)) = self.types.unify(expected_type, actual_type) else {
            return;
        };
        self.errors.push(Error {
            module: self.module.clone(),
            cause_location: location,
            message: format!("expected {:?}, found {:?}", dst, src),
            kind: PpknErrorKind::TypeError,
        });
    }

    fn vartype(&mut self, location: (u32, u32), name: Str) -> (Str, TypeId) {
        for (variable, typ) in self.locals.iter().rev() {
            if *name == **variable {
                return (name.clone(), *typ);
            }
        }

        let path = if name.contains(':') {
            name.clone()
        } else {
            format!("{}:{}", self.module, name).into()
        };
        if let Some((global_typ, _)) = self.program.globals.get(&path) {
            return (path, self.types.insert_specified(global_typ));
        }

        self.errors.push(Error {
            module: self.module.clone(),
            cause_location: location,
            message: "not defined in this scope".to_string(),
            kind: PpknErrorKind::NameError,
        });
        (path, self.types.insert(InferredType::Unknown))
    }

    fn resolve_typename(&mut self, typename: &Typename) -> TypeId {
        let typ = match &typename.kind {
            TypenameKind::Tuple(field_names) => {
                let mut fields = vec![];
                for field in field_names {
                    let typ = self.resolve_typename(field);
                    fields.push(typ);
                }
                // PossiblyUnspecifiedType::Tuple(fields)
                todo!()
            }
            TypenameKind::Array(item) => {
                let item = self.resolve_typename(item);
                InferredType::Array(item)
            }
            TypenameKind::Name(_) => {
                let typ = self.program.resolve_typename(&self.module, typename);
                return self.types.insert_specified(&typ);
            }
            TypenameKind::Unknown => InferredType::Unknown,
            TypenameKind::Void => InferredType::Void,
        };
        self.types.insert(typ)
    }

    fn error(&mut self, kind: PpknErrorKind, location: (u32, u32), message: String) {
        self.errors.push(Error {
            module: self.module.clone(),
            cause_location: location,
            message,
            kind,
        });
        // TODO: figure out how to not forget to set the flag
        // self.program.modules[&self.module].poisoned.set(true);
        // Is using Cell here overkill?
        // self.program.modules[&self.module].poisoned = true;
    }
}
