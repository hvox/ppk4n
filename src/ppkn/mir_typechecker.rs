#![allow(unused)]
use std::borrow::Cow;

use super::mir::*;
use super::common::*;

impl Program {
    pub fn typecheck_function(&mut self, id: Handle<Function>) {
        let function = &mut self.functions[id];
        let module = function.module;
        let checker = Typechecker::new(&mut function.body, function.params.clone());
        let (body, errors) = checker.typecheck(&self, module);
        self.modules[module].errors.extend(errors);
        self.functions[id].body = body;
    }

    pub fn typecheck_global(&mut self, id: Handle<Global>) {
        let global = &mut self.globals[id];
        let module = global.module;
        let checker = Typechecker::new(&mut global.value, vec![]);
        let (body, errors) = checker.typecheck(&self, module);
        self.modules[module].errors.extend(errors);
        self.globals[id].value = body;
    }
}

struct Typechecker {
    params: Vec<(Str, Type)>,
    result: Type,
    body: Expr,
}

struct TypecheckerCtx<'p> {
    program: &'p Program,
    module: &'p Module,
    params: Vec<(Str, Type)>,
    locals: Vec<(Str, TypeId)>,
    result: Type,
    types: Types,
    errors: Vec<Error>,
}

impl Typechecker {
    fn new(body: &mut Body, params: Vec<(Str, Type)>) -> Self {
        let expr = std::mem::replace(&mut body.value, Expr::empty());
        Self { body: expr, result: body.typ.clone(), params }
    }

    fn typecheck<'p>(self, program: &Program, module: usize) -> (Body, Vec<Error>) {
        let mut types = Types::new();
        let locals: Vec<_> = self
            .params
            .iter()
            .filter_map(
                |(name, typ)| {
                    if name.is_empty() {
                        None
                    } else {
                        Some((name.clone(), STRING))
                    }
                },
            )
            .collect();
        let mut locals = Vec::new();
        for (name, typ) in self.params.iter().filter(|x| !x.0.is_empty()) {
            locals.push((name.clone(), types.insert_concrete(typ)));
        }
        let mut typechecker = TypecheckerCtx {
            program,
            module: &program.modules[module],
            params: self.params,
            locals,
            result: self.result,
            types,
            errors: Vec::new(),
        };
        let mut body = self.body;
        typechecker.typecheck_body(&mut body);
        let body = Body { value: body, types: typechecker.types, typ: typechecker.result };
        (body, typechecker.errors)
    }
}

impl<'p> TypecheckerCtx<'p> {
    fn typecheck_body(&mut self, body: &mut Expr) {}

    fn typecheck_expr(&mut self, expr: &mut Expr, expected_type: TypeId) {
        use InstrKind::*;
        expr.typ = expected_type;
        let actual_type = match &mut expr.kind {
            String(_) => STRING,
            Integer(_) => self.types.insert(InferredType::UnkInt),
            Boolean(_) => BOOL,
            Identifier(name, global) => {
                if let Some(typ) = self.resolve_local(&name) {
                    *global = None;
                    typ
                } else if let Some((id, typ)) = self.resolve_global(&name) {
                    *global = Some(id);
                    typ
                } else {
                    return self.name_error(expr.span);
                }
            }
            Assignment(name, global, value) => {
                if let Some(typ) = self.resolve_local(&name) {
                    self.typecheck_expr(value, typ);
                    *global = None;
                } else if let Some((id, typ)) = self.resolve_global(&name) {
                    self.typecheck_expr(value, typ);
                    *global = Some(id);
                } else {
                    self.name_error(expr.span);
                    let typ = self.types.insert(InferredType::Unknown);
                    self.typecheck_expr(value, typ);
                }
                VOID
            }
            Tuple(fields) => {
                // TODO: More friendly error messages
                // for wrong number of fields or
                // invalid individual field type
                let mut types = vec![];
                for field in fields {
                    let typ = self.types.insert(InferredType::Unknown);
                    self.typecheck_expr(field, typ);
                    types.push(typ);
                }
                self.types.insert(InferredType::Tuple(types))
            }
            Block(block) => return self.typecheck_block(block, expected_type),
            While(exprs) => {
                let (condition, body) = &mut **exprs;
                self.typecheck_expr(condition, BOOL);
                self.typecheck_expr(body, VOID);
                // TODO: non-void while loops
                VOID
            }
            If(exprs) => {
                let (condition, then, otherwise) = &mut **exprs;
                self.typecheck_expr(condition, BOOL);
                self.typecheck_expr(then, expected_type);
                self.typecheck_expr(otherwise, expected_type);
                return;
            }
            Call(call) if call.has_receiver => {
                let receiver = self.types.unknown();
                self.typecheck_expr(&mut call.args[0], receiver);
                // if let Some(class) = self.resolve_typename(typ) {
                //	todo!();
                // } else {
                // }
                if let Some((params, result)) = self.resolve_method(receiver, &call.func) {
                    // TODO: More friendly error messages
                    // for wrong number of arguments or
                    // invalid individual argumnt type
                    let mut args = Vec::with_capacity(params.len() + 1);
                    args.push(receiver);
                    for arg in &mut call.args[1..] {
                        let typ = self.types.unknown();
                        self.typecheck_expr(arg, typ);
                        args.push(typ);
                    }
                    if args.len() != params.len() {
                        self.type_error(expr.span, "wrong number of arguments");
                    }
                    for (arg, param) in args.iter().zip(params) {
                        // TODO: fix span
                        self.unify_types_or_cast(expr.span, param, *arg);
                    }
                    result
                } else {
                    self.errors.push(Error {
                        cause: expr.span,
                        message: "method not found".into(),
                        kind: ErrorKind::Name,
                    });
                    for arg in &mut call.args[1..] {
                        let typ = self.types.unknown();
                        self.typecheck_expr(arg, typ);
                    }
                    return;
                }
            }
            Call(call) => {
                if let Some((params, result, hndl)) = self.resolve_function(&call.func) {
                    call.hndl = hndl;
                    for (arg, &typ) in call.args.iter_mut().zip(&params) {
                        self.typecheck_expr(arg, typ);
                    }
                    if call.args.len() > params.len() {
                        let start = call.args[params.len()].span.start;
                        let end = call.args.last().unwrap().span.end;
                        let error = format!(
                            "expected only {} arguments but got {}",
                            params.len(),
                            call.args.len()
                        );
                        self.type_error(Span::new(start, end), error);
                        for arg in &mut call.args[params.len()..] {
                            let typ = self.types.unknown();
                            self.typecheck_expr(arg, typ);
                        }
                    } else if call.args.len() < params.len() {
                        let error = format!(
                            "function takes {} arguments but got only {}",
                            call.args.len(),
                            params.len(),
                        );
                        self.type_error(call.fname_span, error);
                    }
                    result
                } else {
                    self.errors.push(Error {
                        cause: expr.span,
                        message: "function not found in this scope".into(),
                        kind: ErrorKind::Name,
                    });
                    for arg in &mut call.args {
                        let typ = self.types.unknown();
                        self.typecheck_expr(arg, typ);
                    }
                    expected_type
                }
            }
            Return(expr) => {
                let result = self.types.insert_concrete(&self.result);
                return self.typecheck_expr(expr, result);
            }
            Unreachable => return,
            NoOp => VOID,
        };
        self.unify_types_or_cast(expr.span, expected_type, actual_type);
    }

    fn typecheck_block(&mut self, block: &mut Block, expected_type: TypeId) {
        let outer_scope_size = self.locals.len();
        for stmt in &mut block.stmts {
            if let Some(target) = &stmt.target {
                let typ = if let Some(typ) = &target.typ {
                    self.types.insert_concrete(&typ.value)
                } else {
                    self.types.insert(InferredType::Unknown)
                };
                self.locals.push((target.name.clone(), typ));
                self.typecheck_expr(&mut stmt.value, typ);
            } else {
                let typ = self.types.insert(InferredType::Unknown);
                self.typecheck_expr(&mut stmt.value, typ);
            }
        }
        if let Some(result) = &mut block.result {
            self.typecheck_expr(result, expected_type);
        } else if expected_type != VOID {
            // TODO: Use span of whole statement instead of last expression
            self.unify_types_or_cast(block.stmts.last().unwrap().value.span, expected_type, VOID);
        }
        self.locals.truncate(outer_scope_size);
    }

    fn resolve_method(&mut self, receiver: TypeId, method: &str) -> Option<(Vec<TypeId>, TypeId)> {
        use InferredType::*;
        use BuiltinType::*;
        let t = receiver;

        #[rustfmt::skip]
        let signature = match (self.types.get_type(t), method) {
            (UnkInt | UnkFloat | Builtin(Int8 | Int16 | Int32 | Int64 |Uint8 | Uint16 | Uint32 | Uint64 | Float32 | Float64),
                "add" | "sub" | "mul" | "div") => (vec![t], t),
            (UnkInt | Builtin(Int8 | Int16 | Int32 | Int64 | Uint8 | Uint16 | Uint32 | Uint64 | Bool),
                "bitadd" | "bitor" | "bitxor") => (vec![t], t),
            (UnkInt | Builtin(Int8 | Int16 | Int32 | Int64 | Uint8 | Uint16 | Uint32 | Uint64),
                "div_floor" | "rem") => (vec![t], t),
            (Builtin(Str), "add") => (vec![t], t),
            (Builtin(Str), "push") => (vec![CHAR], VOID),
            (_, "eq" | "ne" | "lt" | "le" | "gt" | "ge") => (vec![t], BOOL),
            _ => return None,
        };
        Some(signature)
    }

    fn resolve_function(
        &mut self,
        function: &str,
    ) -> Option<(Vec<TypeId>, TypeId, Option<Handle<Function>>)> {
        let Some(item) = self.module.resolved_names.get(function) else { return None };
        let func_id = match item {
            Definition::Function(handle) => *handle,
            _ => return None,
        };
        let func = &self.program.functions[func_id];
        let params = func.params.iter().map(|(_, t)| self.types.insert_concrete(t)).collect();
        let result = self.types.insert_concrete(&func.body.typ);
        Some((params, result, Some(func_id)))
    }

    fn resolve_global(&mut self, name: &str) -> Option<(Handle<Global>, TypeId)> {
        let Some((name, def)) = self.module.resolved_names.get_key_value(name) else { return None };
        match def {
            Definition::Global(id) => {
                let typ = &self.program.globals[*id].value.typ;
                let typ = self.types.insert_concrete(typ);
                Some((*id, typ))
            }
            _ => None,
        }
    }

    fn resolve_local(&mut self, name: &str) -> Option<TypeId> {
        for (local, typ) in self.locals.iter().rev() {
            if name == local.as_ref() {
                return Some(*typ);
            }
        }
        None
    }

    fn resolve_typename(&mut self, typ: TypeId) -> Option<Handle<Struct>> {
        todo!()
    }

    fn resolve_name(&mut self, span: Span, name: &str) -> (InstrKind, TypeId) {
        for (local, typ) in self.locals.iter().rev() {
            if name == local.as_ref() {
                return (InstrKind::Identifier(local.clone(), None), *typ);
            }
        }
        if let Some((name, def)) = self.module.resolved_names.get_key_value(name) {
            match def {
                Definition::Global(id) => {
                    let typ = &self.program.globals[*id].value.typ;
                    let typ = self.types.insert_concrete(typ);
                    return (InstrKind::Identifier(name.clone(), Some(*id)), typ);
                }
                _ => (),
            }
        }
        self.errors.push(Error {
            cause: span,
            message: "not defined in this scope".into(),
            kind: ErrorKind::Name,
        });
        (InstrKind::Identifier(name.into(), None), self.types.insert(InferredType::Unknown))
    }

    fn unify_types_or_cast(&mut self, span: Span, expected: TypeId, actual: TypeId) {
        let Err((dst, src)) = self.types.unify(expected, actual) else {
            return;
        };
        if dst != InferredType::Void {
            self.errors.push(Error {
                message: format!("expected {:?}, found {:?}", dst, src).into(),
                kind: ErrorKind::Type,
                cause: span,
            });
        }
    }

    fn unify(&mut self, span: Span, expected: TypeId, actual: TypeId) {
        let Err((dst, src)) = self.types.unify(expected, actual) else {
            return;
        };
        self.errors.push(Error {
            message: format!("expected {:?}, found {:?}", dst, src).into(),
            kind: ErrorKind::Type,
            cause: span,
        });
    }

    fn type_error(&mut self, span: Span, message: impl Into<Cow<'static, str>>) {
        self.errors.push(Error { cause: span, message: message.into(), kind: ErrorKind::Type });
    }

    fn name_error(&mut self, span: Span) {
        self.errors.push(Error {
            cause: span,
            message: "not found in this scope".into(),
            kind: ErrorKind::Name,
        });
    }
}
