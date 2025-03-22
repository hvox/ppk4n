#![allow(unused)]

use indexmap::IndexMap;

use super::mir;
use super::yair;

impl mir::Program<'_> {
    fn to_yair(self) -> yair::Program {
        let mut functions = IndexMap::new();
        for f in self.functions {
            functions.insert(
                f.name.clone(),
                yair::Function {
                    name: f.name.clone(),
                    type_params: vec![],
                    params: f
                        .params
                        .into_iter()
                        .map(|(name, typ)| yair::Variable {
                            name: name.clone(),
                            typ: translate_type(typ),
                        })
                        .collect(),
                    result: yair::Variable {
                        name: "result".into(),
                        typ: translate_type(f.result),
                    },
                    body: translate_body(f.body),
                },
            );
        }
        yair::Program {
            types: IndexMap::new(),
            fns: functions,
        }
    }
}

fn translate_body(mir_stmts: Vec<mir::InstrCntrl>) -> yair::Expr {
    let mut stmts = vec![];
    for stmt in mir_stmts {
        stmts.push(translate_stmt(stmt));
    }
    yair::Expr {
        kind: yair::ExprKind::Block(stmts),
        typ: "".into(),
    }
}

fn translate_stmt(stmt: mir::InstrCntrl) -> yair::Expr {
    use mir::InstrKindCntrl::*;
    match *stmt.kind {
        Call(_, instrs) => todo!(),
        DefI64(_, instr_i64) => todo!(),
        DefU64(_, instr_u64) => todo!(),
        DefF64(_, instr_f64) => todo!(),
        DefStr(_, instr_str) => todo!(),
        DefBool(_, instr_bool) => todo!(),
        DefVec(_, instr_vec, _) => todo!(),
        SetI64(_, instr_i64) => todo!(),
        SetU64(_, instr_u64) => todo!(),
        SetF64(_, instr_f64) => todo!(),
        SetStr(_, instr_str) => todo!(),
        SetBool(_, instr_bool) => todo!(),
        SetVec(_) => todo!(),
        PrintStr(instr_str) => todo!(),
        PrintlnStr(instr_str) => todo!(),
        While(instr_bool, instr_cntrls) => todo!(),
        Block(instr_cntrls) => todo!(),
        Return(instr) => todo!(),
        Drop(instr) => todo!(),
        Push(_, instr) => todo!(),
    }
}

fn translate_type(typ: mir::Type) -> yair::Type {
    match typ {
        mir::Type::Unit => "()".into(),
        mir::Type::Bool => "bool".into(),
        mir::Type::I64 => "i64".into(),
        mir::Type::U64 => "u64".into(),
        mir::Type::F64 => "f64".into(),
        mir::Type::Str => "str".into(),
        mir::Type::Vec(typ) => {
            let typ: mir::Type = (*typ).clone();
            yair::Type {
                path: "[]".into(),
                args: vec![translate_type(typ)],
            }
        }
    }
}
