#![allow(unused)]
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
                    return self.not_found_error(expr.span);
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
                    self.not_found_error(expr.span);
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
            Block(block) => {
                self.typecheck_block(block, expected_type);
                expected_type
            }
            While(_) => todo!(),
            If(_) => todo!(),
            Call(fn_call) => todo!(),
            Return(expr) => todo!(),
            Unreachable => todo!(),
            NoOp => todo!(),
        };
        self.unify(expr.span, expected_type, actual_type);
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
            if expected_type == VOID {
                let expected_type = self.types.unknown();
                self.typecheck_expr(result, expected_type);
            } else {
                self.typecheck_expr(result, expected_type);
            }
        } else if expected_type != VOID {
            todo!()
        }
        self.locals.truncate(outer_scope_size);
    }

    fn resolve_local(&mut self, name: &str) -> Option<TypeId> {
        for (local, typ) in self.locals.iter().rev() {
            if name == local.as_ref() {
                return Some(*typ);
            }
        }
        None
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

    fn not_found_error(&mut self, span: Span) {
        self.errors.push(Error {
            cause: span,
            message: "not found in this scope".into(),
            kind: ErrorKind::Name,
        });
    }
}
