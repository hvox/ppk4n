#![allow(unused)]
use std::cell::Cell;
use std::collections::HashMap;
use std::marker::PhantomData;
use std::ops::Index;
use std::ops::IndexMut;
use std::rc::Rc;

use indexmap::IndexMap;

use super::ast::*;
use super::mir;
use super::mir::*;
use crate::utils;
use crate::utils::try_map;

pub fn typecheck(program: Block<()>) -> Result<Program, TypeError> {
    Typechecker::new().typecheck(program)
}

struct Typechecker<'a> {
    // ast: Block<'a, ()>,
    signatures: IndexMap<Str, FnSign>,
    structs: IndexMap<Str, StructSign>,
    _dummy: PhantomData<&'a ()>,
}

struct StructSign {
    name: Str,
    params: Vec<Str>,
    methods: IndexMap<Str, FnSign>,
}

struct FnSign {
    name: Str,
    params: Vec<(Str, Type)>,
    result: Type,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct TypeError<'a> {
    pub location: &'a str,
    pub message: &'static str,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum UnresolvedType {
    Unknown,
    Unit,
    Bool,
    I64,
    U64,
    F64,
    Str,
    Vec(TypeId),
}

impl<'a> Typechecker<'a> {
    fn new() -> Self {
        let stdlib_structs = IndexMap::from([(
            "Array".into(),
            StructSign {
                name: "Array".into(),
                params: vec!["T".into()],
                methods: IndexMap::from([(
                    Str::from("push"),
                    FnSign {
                        name: "push".into(),
                        params: vec![],
                        result: Type::Unit,
                    },
                )]),
            },
        )]);
        Self {
            signatures: IndexMap::new(),
            structs: stdlib_structs,
            _dummy: PhantomData,
        }
    }

    fn test_mut(&mut self) {}

    fn typecheck(mut self, ast: Block<'a, ()>) -> Result<Program<'a>, TypeError<'a>> {
        let mut sources = vec![];

        for stmt in ast.stmts.into_iter() {
            let ExprKind::Function(name, params, _, body) = stmt.kind else {
                return Err(TypeError {
                    location: stmt.source,
                    message: "Expected function definition",
                });
            };
            let name = Str::from(name);
            let params = try_map(params, |(name, t)| {
                Ok((Str::from(name), self.parse_type(t)?))
            })?;
            let result = Type::Unit;
            self.signatures.insert(
                name.clone(),
                FnSign {
                    name,
                    params,
                    result,
                },
            );
            sources.push(body);
        }
        let mut functions = vec![];
        for (signature, source) in self.signatures.values().zip(sources) {
            let fn_typechecker = FunctionTypechecker::new(&self, signature);
            functions.push(fn_typechecker.typecheck(source)?);
        }
        Ok(Program {
            structs: vec![],
            functions,
        })
    }

    fn parse_type(&self, typename: &'a str) -> Result<Type, TypeError<'a>> {
        match typename {
            "int" => Ok(Type::I64),
            "uint" => Ok(Type::U64),
            "float" => Ok(Type::F64),
            "str" => Ok(Type::Str),
            "unit" => Ok(Type::Unit),
            _ => Err(TypeError {
                location: typename,
                message: "Unknown type",
            }),
        }
    }
}

struct FunctionTypechecker<'a, 'b> {
    typechecker: &'b Typechecker<'a>,
    signature: &'b FnSign,
    locals: Vec<(Str, TypeId)>,
    scope: HashMap<Str, usize>,
    types: TypesDsu,
    _dummy: PhantomData<&'a ()>,
}

impl<'a, 'b> FunctionTypechecker<'a, 'b> {
    fn new(typechecker: &'b Typechecker<'a>, signature: &'b FnSign) -> Self {
        let mut locals = vec![];
        let mut scope = HashMap::new();
        let mut types = TypesDsu::new();
        for (i, (name, real_type)) in signature.params.iter().enumerate() {
            let type_id = types.add_type(real_type.clone());
            locals.push((name.clone(), type_id));
            scope.insert(name.clone(), i);
        }
        Self {
            typechecker,
            signature,
            locals,
            scope,
            types,
            _dummy: PhantomData,
        }
    }

    fn typecheck(mut self, body: Block<'a, ()>) -> Result<Function<'a, ()>, TypeError<'a>> {
        let mut stmts = vec![];
        for stmt in body.stmts {
            stmts.push(self.annotate(stmt)?);
        }
        for typ in self.types.types.values_mut() {
            if typ == &UnresolvedType::Unknown {
                *typ = UnresolvedType::Unit;
            }
        }
        let mut typechecked_body = vec![];
        for stmt in stmts {
            typechecked_body.push(self.typecheck_stmt(stmt)?);
        }
        Ok(Function {
            name: self.signature.name.clone(),
            params: self.signature.params.clone(),
            result: self.signature.result.clone(),
            locals: self
                .locals
                .into_iter()
                .map(|(name, typ)| (name, self.types.actualize_type(typ)))
                .collect(),
            body: typechecked_body,
            extra: (),
        })
    }

    fn typecheck_stmt(
        &mut self,
        expr: Expr<'a, ExprDataWithTypeId>,
    ) -> Result<InstrCntrl<'a>, TypeError<'a>> {
        let source = expr.source;
        use InstrKindCntrl::*;
        let kind = match expr.kind {
            ExprKind::Print(expr) => PrintStr(converted_to_str(self.typecheck_expr(*expr)?)),
            ExprKind::Println(expr) => PrintlnStr(converted_to_str(self.typecheck_expr(*expr)?)),
            ExprKind::Definition(name, _, expr) => match Type::from(&self.types[expr.type_id]) {
                Type::Unit => Drop(self.typecheck_expr(*expr)?),
                Type::Bool => DefBool(self.scope[name], self.typecheck_bool(*expr)?),
                Type::I64 => DefI64(self.scope[name], self.typecheck_i64(*expr)?),
                Type::U64 => DefU64(self.scope[name], self.typecheck_u64(*expr)?),
                Type::F64 => DefF64(self.scope[name], self.typecheck_f64(*expr)?),
                Type::Str => DefStr(self.scope[name], self.typecheck_str(*expr)?),
                Type::Vec(t) => DefVec(self.scope[name], self.typecheck_vec(*expr)?, (*t).clone()),
            },
            ExprKind::Assignment(name, expr) => match Type::from(&self.types[expr.type_id]) {
                Type::Unit => Drop(self.typecheck_expr(*expr)?),
                Type::Bool => SetBool(self.scope[name], self.typecheck_bool(*expr)?),
                Type::I64 => SetI64(self.scope[name], self.typecheck_i64(*expr)?),
                Type::U64 => SetU64(self.scope[name], self.typecheck_u64(*expr)?),
                Type::F64 => SetF64(self.scope[name], self.typecheck_f64(*expr)?),
                Type::Str => SetStr(self.scope[name], self.typecheck_str(*expr)?),
                Type::Vec(_) => SetVec(self.scope[name]),
            },
            ExprKind::If(expr, block, block1) => todo!(),
            ExprKind::While(expr, block) => todo!(),
            ExprKind::Return(expr) => todo!(),
            ExprKind::Variable(_) => todo!(),
            ExprKind::Grouping(expr) => todo!(),
            ExprKind::FunctionCall(name, params) => {
                Call(self.typechecker.signatures.get_index_of(name).unwrap(), {
                    let mut typed_params = vec![];
                    for param in params {
                        typed_params.push(self.typecheck_expr(param)?);
                    }
                    typed_params
                })
            }
            ExprKind::MethodCall(obj, method, vec) => {
                assert!(method == "push");
                Push(self.scope[obj.source], self.typecheck_expr(vec[0].clone())?)
            }
            _ => unreachable!(),
        };
        Ok(InstrCntrl::new(source, kind))
    }

    fn typecheck_bool(
        &mut self,
        expr: Expr<'a, ExprDataWithTypeId>,
    ) -> Result<InstrBool<'a>, TypeError<'a>> {
        let source = expr.source;
        let kind = match expr.kind {
            ExprKind::Variable(name) => InstrKindBool::Variable(self.scope[&*name]),
            ExprKind::Grouping(expr) => *self.typecheck_bool(*expr)?.kind,
            ExprKind::Unary(unary_op, expr) => match unary_op.kind {
                UnaryOpKind::Bang => InstrKindBool::Not(self.typecheck_bool(*expr)?),
                _ => unreachable!(),
            },
            ExprKind::Binary(expr, bin_op, expr1) => todo!(),
            ExprKind::FunctionCall(expr, vec) => todo!(),
            _ => unreachable!(),
        };
        Ok(InstrBool {
            source,
            kind: Box::new(kind),
        })
    }

    fn typecheck_u64(
        &mut self,
        expr: Expr<'a, ExprDataWithTypeId>,
    ) -> Result<InstrU64<'a>, TypeError<'a>> {
        use InstrKindU64::*;
        let source = expr.source;
        let kind = match expr.kind {
            ExprKind::Integer(value) => Value(value),
            ExprKind::Variable(name) => Variable(self.scope[&name[..]]),
            ExprKind::Grouping(expr) => *self.typecheck_u64(*expr)?.kind,
            ExprKind::Unary(unary_op, expr) => todo!(),
            ExprKind::Binary(lhs, op, rhs) => match op.kind {
                BinOpKind::Minus => Sub(self.typecheck_u64(*lhs)?, self.typecheck_u64(*rhs)?),
                BinOpKind::Plus => Add(self.typecheck_u64(*lhs)?, self.typecheck_u64(*rhs)?),
                BinOpKind::Slash => Div(self.typecheck_u64(*lhs)?, self.typecheck_u64(*rhs)?),
                BinOpKind::Star => Mult(self.typecheck_u64(*lhs)?, self.typecheck_u64(*rhs)?),
                _ => unreachable!(),
            },
            ExprKind::FunctionCall(expr, vec) => todo!(),
            _ => unreachable!(),
        };
        Ok(InstrU64 {
            source,
            kind: Box::new(kind),
        })
    }

    fn typecheck_i64(
        &mut self,
        expr: Expr<'a, ExprDataWithTypeId>,
    ) -> Result<InstrI64<'a>, TypeError<'a>> {
        use InstrKindI64::*;
        let source = expr.source;
        let kind = match expr.kind {
            ExprKind::Integer(value) => Value(value as i64),
            ExprKind::Variable(name) => Variable(self.scope[&name[..]]),
            ExprKind::Grouping(expr) => *self.typecheck_i64(*expr)?.kind,
            ExprKind::Unary(unary_op, expr) => todo!(),
            ExprKind::Binary(lhs, op, rhs) => match op.kind {
                BinOpKind::Minus => Sub(self.typecheck_i64(*lhs)?, self.typecheck_i64(*rhs)?),
                BinOpKind::Plus => Add(self.typecheck_i64(*lhs)?, self.typecheck_i64(*rhs)?),
                BinOpKind::Slash => Div(self.typecheck_i64(*lhs)?, self.typecheck_i64(*rhs)?),
                BinOpKind::Star => Mult(self.typecheck_i64(*lhs)?, self.typecheck_i64(*rhs)?),
                _ => unreachable!(),
            },
            ExprKind::FunctionCall(expr, vec) => todo!(),
            _ => unreachable!(),
        };
        Ok(InstrI64 {
            source,
            kind: Box::new(kind),
        })
    }

    fn typecheck_f64(
        &mut self,
        expr: Expr<'a, ExprDataWithTypeId>,
    ) -> Result<InstrF64<'a>, TypeError<'a>> {
        use InstrKindF64::*;
        let source = expr.source;
        let kind = match expr.kind {
            ExprKind::Float(value) => Value(value),
            ExprKind::Variable(name) => Variable(self.scope[&name[..]]),
            ExprKind::Grouping(expr) => *self.typecheck_f64(*expr)?.kind,
            ExprKind::Unary(unary_op, expr) => todo!(),
            ExprKind::Binary(lhs, op, rhs) => match op.kind {
                BinOpKind::Minus => Sub(self.typecheck_f64(*lhs)?, self.typecheck_f64(*rhs)?),
                BinOpKind::Plus => Add(self.typecheck_f64(*lhs)?, self.typecheck_f64(*rhs)?),
                BinOpKind::Slash => Div(self.typecheck_f64(*lhs)?, self.typecheck_f64(*rhs)?),
                BinOpKind::Star => Mult(self.typecheck_f64(*lhs)?, self.typecheck_f64(*rhs)?),
                _ => unreachable!(),
            },
            ExprKind::FunctionCall(expr, vec) => todo!(),
            _ => unreachable!(),
        };
        Ok(InstrF64 {
            source,
            kind: Box::new(kind),
        })
    }

    fn typecheck_str(
        &mut self,
        expr: Expr<'a, ExprDataWithTypeId>,
    ) -> Result<InstrStr<'a>, TypeError<'a>> {
        let source = expr.source;
        let kind = match expr.kind {
            ExprKind::String(literal) => InstrKindStr::Value(literal.into()),
            ExprKind::Variable(_) => todo!(),
            ExprKind::Grouping(expr) => todo!(),
            ExprKind::Binary(expr, bin_op, expr1) => todo!(),
            ExprKind::FunctionCall(expr, vec) => todo!(),
            _ => unreachable!(),
        };
        Ok(InstrStr {
            source,
            kind: Box::new(kind),
        })
    }
    fn typecheck_vec(
        &mut self,
        expr: Expr<'a, ExprDataWithTypeId>,
    ) -> Result<InstrVec<'a>, TypeError<'a>> {
        use InstrKindVec::*;
        let source = expr.source;
        let kind = match expr.kind {
            ExprKind::Variable(name) if &*name == "[]" => Empty,
            ExprKind::Variable(name) => Variable(self.scope[&name[..]]),
            _ => unreachable!(),
        };
        Ok(InstrVec {
            source,
            kind: Box::new(kind),
        })
    }

    fn typecheck_expr(
        &mut self,
        expr: Expr<'a, ExprDataWithTypeId>,
    ) -> Result<Instr<'a>, TypeError<'a>> {
        let source = expr.source;
        let kind = match Type::from(&self.types[expr.type_id]) {
            Type::Unit => InstrKind::Cntrl(self.typecheck_stmt(expr)?),
            Type::Bool => todo!(),
            Type::I64 => InstrKind::I64(self.typecheck_i64(expr)?),
            Type::U64 => todo!(),
            Type::F64 => InstrKind::F64(self.typecheck_f64(expr)?),
            Type::Str => InstrKind::Str(self.typecheck_str(expr)?),
            Type::Vec(_) => {
                let Type::Vec(typ) = self.types.actualize_type(expr.type_id) else {
                    unreachable!()
                };
                InstrKind::Vec(self.typecheck_vec(expr)?, (*typ).clone())
            }
        };
        Ok(Instr { source, kind })
    }

    fn annotate(
        &mut self,
        expr: Expr<'a, ()>,
    ) -> Result<Expr<'a, ExprDataWithTypeId>, TypeError<'a>> {
        let expr = expr
            .add_annotations(|| self.types.new_type())
            .try_foreach(&mut |t, source, kind| {
                match &kind {
                    ExprKind::Print(expr) => self.types[t] = UnresolvedType::Unit,
                    ExprKind::Println(expr) => self.types[t] = UnresolvedType::Unit,
                    ExprKind::Definition(name, typename, expr) => {
                        self.types[t] = UnresolvedType::Unit;
                        let name = *name;
                        let type_id = self.types.new_type();
                        if !typename.is_empty() {
                            let typ = self.typechecker.parse_type(typename).unwrap();
                            self.types[t] = UnresolvedType::from(&typ);
                        }
                        self.scope.insert(name.into(), self.locals.len());
                        self.locals.push((name.into(), type_id));
                        if let Err((t1, t2)) = self.types.merge(type_id, expr.annotations) {
                            return Err(TypeError {
                                location: expr.source,
                                message: Box::leak(
                                    format!("Expected {:?}, found {:?}", t1, t2).into(),
                                ),
                            });
                        }
                    }
                    ExprKind::Assignment(name, expr) => {
                        self.types[t] = UnresolvedType::Unit;
                        let type_id = self.locals[self.scope[*name]].1;
                        if let Err((t1, t2)) = self.types.merge(type_id, expr.annotations) {
                            return Err(TypeError {
                                location: expr.source,
                                message: Box::leak(
                                    format!("Expected {:?}, found {:?}", t1, t2).into(),
                                ),
                            });
                        }
                    }
                    ExprKind::If(expr, block, block1) => todo!(),
                    ExprKind::While(expr, block) => todo!(),
                    ExprKind::Return(expr) => todo!(),
                    ExprKind::Integer(_) => self.types[t] = UnresolvedType::I64,
                    ExprKind::Float(_) => self.types[t] = UnresolvedType::F64,
                    ExprKind::String(_) => self.types[t] = UnresolvedType::Str,
                    ExprKind::Variable(name) => match self.scope.get(&name[..]) {
                        Some(variable_idx) => {
                            self.types.merge(t, self.locals[*variable_idx].1).unwrap();
                        }
                        None if &name[..] == "[]" => {
                            let array = self.types.new_type();
                            self.types[array] = UnresolvedType::Vec(self.types.new_type());
                            self.types.merge(t, array).unwrap();
                        }
                        None => {
                            return Err(TypeError {
                                location: source,
                                message: Box::leak(Box::from(format!(
                                    "Variable {} is not found in current scope",
                                    name
                                ))),
                            });
                        }
                    },
                    ExprKind::Grouping(expr) => todo!(),
                    ExprKind::Unary(unary_op, expr) => todo!(),
                    ExprKind::Binary(lhs, op, rhs) => {
                        let lhs_type = lhs.annotations;
                        let rhs_type = rhs.annotations;
                        self.types
                            .merge(lhs_type, rhs_type)
                            .map_err(|(t1, t2)| TypeError {
                                location: op.source,
                                message: "Operands have incompatible types",
                            })?;
                        self.types.merge(t, lhs_type).unwrap();
                    }
                    ExprKind::FunctionCall(name, args) => {
                        let typ = self.typechecker.signatures[*name].result.clone();
                        let expected_typ = self.types.add_type(typ);
                        self.types.merge(t, expected_typ).unwrap()
                    }
                    ExprKind::MethodCall(obj, method, args) => {
                        let Some(variable_idx) = self.scope.get(obj.source) else {
                            return Err(TypeError {
                                location: source,
                                message: Box::leak(Box::from(format!(
                                    "Variable {} is not found in current scope",
                                    obj.source
                                ))),
                            });
                        };
                        let cls = self.types[self.locals[*variable_idx].1].clone();
                        let UnresolvedType::Vec(inner_type) = cls else {
                            unreachable!();
                        };
                        let typ = &self.typechecker.structs[0].methods[*method].result;
                        let expected_typ = self.types.add_type(typ.clone());
                        self.types.merge(t, expected_typ).unwrap();
                        for arg in args {
                            self.types.merge(inner_type, arg.annotations).unwrap();
                        }
                    }
                    _ => unreachable!(),
                };
                Ok(())
            })?
            .map_annotations(&mut |t, _| ExprDataWithTypeId { type_id: t });
        Ok(expr)
    }
}

impl<'a> Expr<'a, ExprDataWithTypeId> {
    fn typed(
        source: &'a str,
        kind: ExprKind<'a, ExprDataWithTypeId>,
        type_id: TypeId,
    ) -> Expr<'a, ExprDataWithTypeId> {
        Expr::from(source, kind, ExprDataWithTypeId { type_id })
    }
}

// fn expr<'a>(
// 	source: &'a str,
// 	kind: ExprKind<'a, TypeId>,
// 	type_id: DsuElementId,
// ) -> Expr<'a, TypeId> {
// 	Expr::from(source, kind, TypeId { type_id })
// }

#[derive(Clone, Debug)]
struct ExprDataWithTypeId {
    pub type_id: TypeId,
}

struct TypesDsu {
    leaders: Vec<Cell<u32>>,
    types: HashMap<u32, UnresolvedType>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct TypeId(u32);

impl TypesDsu {
    fn new() -> Self {
        Self {
            leaders: vec![],
            types: HashMap::new(),
        }
    }

    fn new_type(&mut self) -> TypeId {
        let id = self.leaders.len() as u32;
        self.leaders.push(Cell::new(id));
        self.types.insert(id, UnresolvedType::Unknown);
        TypeId(id)
    }

    fn add_type(&mut self, typ: Type) -> TypeId {
        let id = self.leaders.len() as u32;
        self.leaders.push(Cell::new(id));
        self.types.insert(id, UnresolvedType::from(&typ));
        TypeId(id)
    }

    fn merge(&mut self, u: TypeId, v: TypeId) -> Result<(), (UnresolvedType, UnresolvedType)> {
        let u = self.resolve_leader(u);
        let v = self.resolve_leader(v);
        if u == v {
            return Ok(());
        }
        let t1 = self.types.remove(&(u as u32)).unwrap();
        let t2 = self.types.remove(&(v as u32)).unwrap();
        use UnresolvedType::*;
        let union_type = match (t1, t2) {
            (Unknown, Unknown) => Unknown,
            (Unknown, t) | (t, Unknown) => t,
            (t1, t2) => {
                if (t1 == t2) {
                    t1
                } else {
                    return Err((t1, t2));
                }
            }
        };
        let union_id = self.merge_sets(u, v) as u32;
        self.types.insert(union_id, union_type);
        Ok(())
    }

    fn resolve_leader(&self, id: TypeId) -> usize {
        let i = id.0 as usize;
        let mut leader = self.leaders[i].get();
        if self.leaders[leader as usize].get() != leader {
            leader = self.resolve_leader(TypeId(leader)) as u32;
            self.leaders[i].set(leader);
        }
        leader as usize
    }

    fn merge_sets(&mut self, mut u: usize, mut v: usize) -> usize {
        // TODO: Rank system?
        if u > v {
            std::mem::swap(&mut u, &mut v);
        }
        self.leaders[v] = Cell::new(u as u32);
        u
    }

    fn actualize_type(&self, id: TypeId) -> Type {
        use UnresolvedType::*;
        match self[id] {
            Unknown | Unit => Type::Unit,
            Bool => Type::Bool,
            I64 => Type::I64,
            U64 => Type::U64,
            F64 => Type::F64,
            Str => Type::Str,
            Vec(t) => Type::Vec(Rc::new(self.actualize_type(t))),
        }
    }
}

impl Index<TypeId> for TypesDsu {
    type Output = UnresolvedType;

    fn index(&self, index: TypeId) -> &Self::Output {
        &self.types[&(self.resolve_leader(index) as u32)]
    }
}

impl IndexMut<TypeId> for TypesDsu {
    fn index_mut(&mut self, index: TypeId) -> &mut Self::Output {
        self.types
            .get_mut(&(self.resolve_leader(index) as u32))
            .unwrap()
    }
}

impl From<&Type> for UnresolvedType {
    fn from(value: &Type) -> Self {
        use UnresolvedType::*;
        match value {
            Type::Unit => Unit,
            Type::Bool => Bool,
            Type::I64 => I64,
            Type::U64 => U64,
            Type::F64 => F64,
            Type::Str => Str,
            Type::Vec(_) => unreachable!(),
        }
    }
}

impl From<&UnresolvedType> for Type {
    fn from(value: &UnresolvedType) -> Self {
        use UnresolvedType::*;
        match value {
            Unknown | Unit => Type::Unit,
            Bool => Type::Bool,
            I64 => Type::I64,
            U64 => Type::U64,
            F64 => Type::F64,
            Str => Type::Str,
            Vec(_) => Type::Vec(Rc::new(Type::U64)),
        }
    }
}

fn converted_to_str(anything: Instr) -> InstrStr {
    use InstrKind::*;
    let kind = match anything.kind {
        Cntrl(instr_cntrl) => todo!(),
        Bool(instr_bool) => todo!(),
        I64(instr_i64) => InstrKindStr::CastI64(instr_i64),
        U64(instr_u64) => todo!(),
        F64(instr_f64) => InstrKindStr::CastF64(instr_f64),
        Str(instr_str) => *instr_str.kind,
        Vec(instr_vec, typ) => InstrKindStr::CastVec(instr_vec, typ),
    };
    InstrStr {
        source: anything.source,
        kind: Box::new(kind),
    }
}
