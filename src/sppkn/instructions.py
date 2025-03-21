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


INSTRUCTIONS_TSV = Path(__file__).resolve().parent / "instructions.tsv"
INSTRUCTIONS = dict(parse_instr(line) for line in INSTRUCTIONS_TSV.read_text().splitlines()[1:])

if sys.argv[1] == "definition":
    for name, imms, *_ in INSTRUCTIONS.values():
        print(pascal(name) + (("(" + ", ".join(t for _, t in imms) + "),") if imms else ","))
else:
    raise Exception(sys.argv[1])
