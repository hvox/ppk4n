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

    // TODO: try to modify expr in-place.
    // fn typecheck_expr(&mut self, expr: &mut Expr, expected_type: TypeId)
    fn typecheck_expr(&mut self, mut expr: Expr, expected_type: TypeId) -> Expr {
        use InstrKind::*;
        let (kind, actual_type) = match expr.kind {
            x @ String(_) => (x, STRING),
            x @ Integer(_) => (x, self.types.insert(InferredType::UnkInt)),
            x @ Boolean(_) => (x, BOOL),
            Identifier(name) | Global(name, _) => {
                if let Some((name, typ)) = self.resolve_local(&name) {
                    (Identifier(name), typ)
                } else if let Some((name, id, typ)) = self.resolve_global(&name) {
                    (Global(name, id), typ)
                } else {
                    self.not_found_error(expr.span);
                    (Identifier(name), self.types.insert(InferredType::Unknown))
                }
            }
            Assignment(name, value) | AssignmentGlobal(name, _, value) => {
                if let Some((name, typ)) = self.resolve_local(&name) {
                    (Assignment(name, value), typ)
                } else if let Some((name, id, typ)) = self.resolve_global(&name) {
                    (AssignmentGlobal(name, id, value), typ)
                } else {
                    self.not_found_error(expr.span);
                    (Assignment(name, value), self.types.insert(InferredType::Unknown))
                }
            }
            Tuple(fields) => {
                // TODO: More friendly error messages
                // for wrong number of fields or
                // invalid individual field type
                let mut types = vec![];
                let fields = fields
                    .into_iter()
                    .map(|field| {
                        let typ = self.types.insert(InferredType::Unknown);
                        let field = self.typecheck_expr(field, typ);
                        types.push(typ);
                        field
                    })
                    .collect();
                (Tuple(fields), self.types.insert(InferredType::Tuple(types)))
            }
            Block(block) => todo!(),
            While(_) => todo!(),
            If(_) => todo!(),
            Call(fn_call) => todo!(),
            Return(expr) => todo!(),
            Unreachable => todo!(),
            NoOp => todo!(),
        };
        expr.kind = kind;
        self.unify(expr.span, expected_type, actual_type);
        expr
    }

    fn resolve_local(&mut self, name: &str) -> Option<(Str, TypeId)> {
        for (local, typ) in self.locals.iter().rev() {
            if name == local.as_ref() {
                return Some((local.clone(), *typ));
            }
        }
        None
    }

    fn resolve_global(&mut self, name: &str) -> Option<(Str, Handle<Global>, TypeId)> {
        let Some((name, def)) = self.module.resolved_names.get_key_value(name) else { return None };
        match def {
            Definition::Global(id) => {
                let typ = &self.program.globals[*id].value.typ;
                let typ = self.types.insert_concrete(typ);
                Some((name.clone(), *id, typ))
            }
            _ => None,
        }
    }

    fn resolve_name(&mut self, span: Span, name: &str) -> (InstrKind, TypeId) {
        for (local, typ) in self.locals.iter().rev() {
            if name == local.as_ref() {
                return (InstrKind::Identifier(local.clone()), *typ);
            }
        }
        if let Some((name, def)) = self.module.resolved_names.get_key_value(name) {
            match def {
                Definition::Global(id) => {
                    let typ = &self.program.globals[*id].value.typ;
                    let typ = self.types.insert_concrete(typ);
                    return (InstrKind::Global(name.clone(), *id), typ);
                }
                _ => (),
            }
        }
        self.errors.push(Error {
            cause: span,
            message: "not defined in this scope".into(),
            kind: ErrorKind::Name,
        });
        (InstrKind::Identifier(name.into()), self.types.insert(InferredType::Unknown))
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
