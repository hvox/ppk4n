import sys
from pathlib import Path
from collections import namedtuple

Instr = namedtuple("Instr", "name imms params code")


def parse_field(field: str) -> (str, str):
    name, typ = field.split(":")
    return (name, ("BlockType" if typ == "Type" else typ))


def parse_instr(line: str) -> (str, Instr):
    name, imms, params, code = line.split("\t")
    imms = [parse_field(imm) for imm in imms.split(" ") if imm]
    params = [parse_field(param) for param in params.split(" ") if param]
    return (name, Instr(name, imms, params, code))


def pascal(name: str) -> str:
    return "".join(w.capitalize() for w in name.replace(".", "_").replace(":", "_").split("_"))


def wasmed(name: str) -> str:
    name = name.split(":")[0].replace("jump", "br")
    if (sign := name[0]) == "u" and (name.startswith("u32") or name.startswith("u64")):
        name = "i" + name[1:]
    if name[0] == "i" and name.split(".")[-1] in "div rem shr ge gt le lt":
        name += "_" + {"i": "s", "u": "u"}[sign]
    return name


def from_u64(expr: str, typ: str) -> str:
    return {
        "u64": f"{expr}",
        "bool": f"{expr} != 0",
        "usize": f"{expr} as usize",
        "f32": f"f32::from_bits({expr} as u32)",
        "f64": f"f64::from_bits({expr})",
    }.get(typ, f"{expr} as {typ}")


def into_u64(expr: str, typ: str) -> str:
    return {
        "u64": f"{expr}",
        "f32": f"{expr}.to_bits() as u64",
        "f64": f"{expr}.to_bits() as u64",
    }.get(typ, f"{expr} as u64")


INSTRUCTIONS_TSV = Path(__file__).resolve().parent / "instructions.tsv"
INSTRUCTIONS = dict(parse_instr(line) for line in INSTRUCTIONS_TSV.read_text().splitlines()[1:])
WASM_TSV = Path(__file__).resolve().parent / "wasm.tsv"
WASM = {name: op.replace(" ", ", ") for name, op in (line.split("\t") for line in WASM_TSV.read_text().splitlines())}

if sys.argv[1] == "definition":
    for name, imms, *_ in INSTRUCTIONS.values():
        print(pascal(name) + (("(" + ", ".join(t for _, t in imms) + "),") if imms else ","))
elif sys.argv[1] == "execution":
    for name, imms, params, code in (x for x in INSTRUCTIONS.values() if x.code != "manual!"):
        instr_typ = "bool" if name.split(".")[-1] in "eqz eq ge gt le lt ne" else name.split(".")[0]
        case = pascal(name) + (("(" + ", ".join(name for name, _ in imms) + ")") if imms else "")
        print(case, "=> {")
        for name, typ in reversed(params):
            offset = " + offset" if name == "i" else ""
            print(f"\tlet {name}: {typ} =", from_u64("self.stack.pop().unwrap()", typ) + f"{offset};")
        code = code.replace("?", ".unwrap()")
        if code.endswith(";"):
            print(f"\t{code}")
        else:
            print(f"\tlet result = {code};\n\tself.stack.push({into_u64('result', instr_typ)});")
        print("}")
elif sys.argv[1] == "wasm":
    for name, imms, params, code in INSTRUCTIONS.values():
        case = pascal(name) + (("(" + ", ".join({"size": "_size"}.get(x, x) for x, _ in imms) + ")") if imms else "")
        opcode = WASM[wasmed(name)]
        print(case, "=> {")
        print("\tcode" + (f".extend([{opcode}]);" if "," in opcode else f".push({opcode});"))
        for imm, typ in [(name, typ) for name, typ in imms if name not in "size location"]:
            imm = f"{name} as i64" if typ in "i32 u32 u64" else imm
            print(f"\tcode.encode(*{imm});" if typ not in "Type Vec<usize>" else "MANUAL")
        print("}")
else:
    raise Exception(sys.argv[1])
