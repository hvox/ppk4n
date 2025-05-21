#![allow(dead_code)]
use std::{
    borrow::Cow,
    cell::Cell,
    collections::{HashMap, HashSet},
    u32, usize,
};

use indexmap::IndexMap;

use super::common::*;

#[derive(Debug, PartialEq, Eq, Clone, Default)]
pub struct Program {
    pub modules: IndexMap<Str, Module>,
    pub imports: Storage<FunctionImport>,
    pub functions: Storage<Function>,
    pub globals: Storage<Global>,
    pub structs: Storage<Struct>,
}

#[derive(Debug, PartialEq, Eq, Clone, Default)]
pub struct Module {
    pub items: HashMap<Str, Item>,
    pub dependents: HashSet<Str>,
    pub errors: Vec<Error>,
    pub resolved_names: HashMap<Str, Definition>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Struct {
    pub name: Str,
    pub methods: HashMap<Str, Handle<Function>>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Error {
    pub cause: Span,
    pub message: Cow<'static, str>,
    pub kind: ErrorKind,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ErrorKind {
    Syntax,
    Type,
    Name,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Item {
    // pub line: usize,
    pub public: bool,
    pub keyword_span: Span,
    pub kind: ItemKind,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ItemKind {
    Dependency(Str),
    // TODO: use Definition instead of its variants.
    Import(Handle<FunctionImport>),
    Function(Handle<Function>),
    Global(Handle<Global>),
}

#[derive(Copy, Debug, PartialEq, Eq, Clone)]
pub enum Definition {
    Import(Handle<FunctionImport>),
    Function(Handle<Function>),
    Global(Handle<Global>),
    Module(usize),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Dependency {
    pub path: Str,
    pub name: Str,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FunctionImport {
    pub name: Str,
    // pub module: Str,
    pub module: usize,
    pub namespace: Str,
    pub params: Vec<(Str, Type)>,
    pub result: Type,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Global {
    pub name: Str,
    // pub module: Str,
    pub module: usize,
    pub value: Body,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Function {
    pub name: Str,
    // pub module: Str,
    pub module: usize,
    pub params: Vec<(Str, Type)>,
    pub body: Body,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Body {
    pub value: Expr,
    pub types: Types,
    pub typ: Type,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Expr {
    pub span: Span,
    pub typ: TypeId,
    pub kind: InstrKind,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum InstrKind {
    String(Str),
    Integer(Str),
    Boolean(bool),
    Tuple(Vec<Expr>),
    Identifier(Str, Option<Handle<Global>>),
    Assignment(Str, Option<Handle<Global>>, Box<Expr>),
    Block(Box<Block>),
    While(Box<(Expr, Expr)>),
    If(Box<(Expr, Expr, Expr)>),
    // MethodCall(Str, Vec<Instr>),
    // FunctionCall(Handle<Function>, Str, Vec<Instr>),
    Call(Box<FnCall>),
    Return(Box<Expr>),
    Unreachable,
    NoOp,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub result: Option<Expr>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Stmt {
    pub target: Option<VarDef>,
    pub value: Expr,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct VarDef {
    pub name: Str,
    pub mutable: bool,
    pub typ: Option<Spanned<Type>>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FnCall {
    pub has_receiver: bool,
    pub fname_span: Span,
    pub func: Str,
    pub args: Vec<Expr>,
    pub hndl: Option<Handle<Function>>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Types {
    handles: Vec<Cell<u32>>,
    types: HashMap<u32, InferredType>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Type {
    Tuple(Vec<Type>),
    Array(Box<Type>),
    Builtin(BuiltinType),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum InferredType {
    Tuple(Vec<TypeId>),
    Array(TypeId),
    Builtin(BuiltinType),

    Unknown,
    UnkInt,
    UnkFloat,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum BuiltinType {
    Bool,
    Char,
    Str,
    Int8,
    Int16,
    Int32,
    Int64,
    Uint8,
    Uint16,
    Uint32,
    Uint64,
    Float32,
    Float64,
    // TODO: Do we need Void type if we have Tuple(vec![])?
    Void,
}

#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub struct TypeId(u32);
pub const VOID: TypeId = TypeId(0);
pub const BOOL: TypeId = TypeId(1);
pub const CHAR: TypeId = TypeId(2);
pub const STRING: TypeId = TypeId(3);

impl Types {
    pub fn empty() -> Self {
        Self { handles: Vec::new(), types: HashMap::new() }
    }

    pub fn new() -> Self {
        use InferredType::*;
        use BuiltinType::*;

        // let handles = vec![
        //     Cell::new(VOID.as_u32()),
        //     Cell::new(BOOL.as_u32()),
        //     Cell::new(CHAR.as_u32()),
        //     Cell::new(STRING.as_u32()),
        // ];

        let types = HashMap::from([
            (VOID.as_u32(), Tuple(Vec::new())),
            (BOOL.as_u32(), Builtin(Bool)),
            (CHAR.as_u32(), Builtin(Char)),
            (STRING.as_u32(), Builtin(Str)),
        ]);

        // debug_assert!(handles.len() == types.len());
        // debug_assert!(handles.iter().enumerate().all(|(i, x)| i as u32 == x.get()));

        let handles: Vec<_> = (0..types.len() as u32).map(|i| Cell::new(i)).collect();

        debug_assert!(handles.iter().all(|i| types.contains_key(&i.get())));
        Self { handles, types }
    }

    pub fn insert_concrete(&mut self, typ: &Type) -> TypeId {
        debug_assert!(!self.types.is_empty());
        use InferredType::*;
        use BuiltinType::*;
        let typ = match typ {
            Type::Tuple(fields) => Tuple(fields.iter().map(|x| self.insert_concrete(x)).collect()),
            Type::Array(item_type) => Array(self.insert_concrete(item_type)),
            Type::Builtin(typ) => match typ {
                Void => return VOID,
                Bool => return BOOL,
                Char => return CHAR,
                Str => return STRING,
                typ => Builtin(*typ),
            },
        };
        self.insert(typ)
    }

    pub fn insert(&mut self, typ: InferredType) -> TypeId {
        debug_assert!(!self.types.is_empty());
        let id = self.handles.len() as u32;
        self.handles.push(Cell::new(id));
        self.types.insert(id, typ);
        TypeId(id)
    }

    pub fn unknown(&mut self) -> TypeId {
        self.insert(InferredType::Unknown)
    }

    pub fn unify(&mut self, u: TypeId, v: TypeId) -> Result<(), (InferredType, InferredType)> {
        debug_assert!(!self.types.is_empty());
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
            // (Int(x), UnkInt) => Int(x),
            // (UnkInt, Int(x)) => Int(x),
            // (Uint(x), UnkInt) => Uint(x),
            // (UnkInt, Uint(x)) => Uint(x),
            // (Float(x), UnkFloat) => Float(x),
            // (UnkFloat, Float(x)) => Float(x),
            (Tuple(xs), Tuple(ys)) if xs.len() == ys.len() => {
                for (x, y) in xs.iter().zip(&ys) {
                    self.unify(*x, *y).map_err(|_| (Tuple(xs.clone()), Tuple(ys.clone())))?;
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

    pub fn get_type(&self, id: TypeId) -> &InferredType {
        &self.types[&self.get_handle(id)]
    }

    fn get_handle(&self, id: TypeId) -> u32 {
        debug_assert!(!self.types.is_empty());
        let id = id.0 as usize;
        let mut leader = self.handles[id].get();
        if self.handles[leader as usize].get() != leader {
            leader = self.get_handle(TypeId(leader));
            self.handles[id].set(leader);
        }
        leader
    }
}

// impl Type {
// 	#[allow(non_upper_case_globals)]
// 	pub const Unit: Type = Type::Tuple(Vec::new());
// }

impl InferredType {
    #[allow(non_upper_case_globals)]
    pub const Void: InferredType = InferredType::Tuple(Vec::new());
}

impl TypeId {
    const fn as_u32(self) -> u32 {
        self.0
    }
}

impl Expr {
    pub const fn empty() -> Self {
        Self::noop(Span::new(Position::new(0, 0), Position::new(0, 0)))
    }

    pub const fn noop(span: Span) -> Self {
        Expr { span, typ: VOID, kind: InstrKind::NoOp }
    }
}
