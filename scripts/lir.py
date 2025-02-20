#!/usr/bin/env python3
import sys
from pathlib import Path
from collections import namedtuple

Instr = namedtuple("Instr", "name args wat stack code")
TSV = Path(__file__).resolve().parent / "lir.tsv"
INSTRS = [Instr(*line.split("\t")) for line in TSV.read_text().splitlines()[1:]]


def into(expr: str, typ: str) -> str:
    if typ in "i32 u32 i64 u64":
        return f"({expr}) as {typ}"
    if typ in "f32 f64":
        return f"{typ}::from_bits({expr} as {typ.replace('f', 'u')})"
    return {"bool": expr + "!= 0", "t": expr, "f32": f"f32::from_bits({expr} as u32)", "u64": expr}[typ]


def frm(expr: str, typ: str) -> str:
    return f"({expr}).to_bits() as u64" if typ[0] == "f" else f"({expr}) as u64"


def instrs():
    next_code = 0
    codes = {}
    for instr in INSTRS:
        instr_body = (instr.args, instr.wat, instr.code)
        if instr_body in codes and "checked" not in instr.code and "call" not in instr.name:
            yield (codes[instr_body], instr, False)
        else:
            codes[instr_body] = next_code
            yield (next_code, instr, True)
            next_code += 1


if sys.argv[1:] == ["definition"]:
    for i, instr, _ in instrs():
        print("pub const", instr.name.replace(".", "_").upper() + f":Instr= 0x{i:02X};")
    print(f"pub const MAX_CODE:u8 = {hex(max(i for i,_,_ in instrs()))};")
elif sys.argv[1:] == ["names"]:
    print("[", ", ".join('"' + instr.name + '"' for _, instr, ok in instrs() if ok), "];")
elif sys.argv[1:] == ["run"]:
    for _, instr, _ in filter((lambda instr: instr[2] and instr[1].code != "manual!"), instrs()):
        code = instr.code.replace("checked", "wrapping")
        xs, ys = [x.split(", ") if x else [] for x in instr.stack[1:-1].split("] -> [")]
        line = [f"let {x}={into('stack.pop().unwrap()',t)};" for x, t in zip("xyz", xs)]
        line += [f"stack.push({frm(code, ys[0])})"] if ys else [code + ";"]
        print(instr.name.replace(".", "_").upper(), "=>{", *line, "}")
elif sys.argv[1:] == ["wat"]:
    for instr in INSTRS:
        print(instr.name.replace(".", "_").upper(), f'=> "{instr.wat}"')
elif sys.argv[1:] == ["wasm"]:
    pass  # TODO
else:
    raise ValueError("Invalid script arguments")
