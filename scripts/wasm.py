#!/usr/bin/env python3
import sys
from contextlib import suppress
from pathlib import Path

SNOWFLAKES = {"block", "loop", "if", "else", "end", "call", "select"}
INSTRUCTIONS_TSV = Path(__file__).resolve().parent / "wasm-instructions.tsv"
INSTRUCTIONS = {
    int(opcode, 16): (instr, immediates.split(", ") if immediates else [])
    for instr, opcode, immediates in (line.split("\t") for line in INSTRUCTIONS_TSV.read_text().splitlines()[1:])
}
RUST_TYPES = {
    **{name: "u32" for name in ["offset", "align", "label", "table", "func_type"]},
    "labels": "Vec<u32>",
}


def main(script_name: str, *script_args: str):
    match script_args:
        case ["variants"]:
            generate_variants_definition()
        case ["to_wat"]:
            generate_to_wat_match_arms()
        case ["to_wasm"]:
            generate_to_wasm_match_arms()
        case _:
            print(f"Invalid arguments: {script_args}")


def generate_variants_definition():
    lines = ["\t"]
    for opcode, (instr, immediates) in INSTRUCTIONS.items():
        if instr in SNOWFLAKES:
            continue
        variant = pascalcase(instr)
        if instr.endswith("const"):
            typ = instr.split(".")[0]
            if typ in ("f32", "f64"):
                typ = f"NotNan<{typ}>"
            variant += "(" + typ + ")"
        elif immediates:
            variant += " { " + ", ".join(imm + ": " + RUST_TYPES[imm] for imm in immediates) + " }"
        line = lines[-1] + " " * (not lines[-1].endswith("\t")) + variant + ","
        if len(line.replace("\t", 4 * " ")) <= 100:
            lines[-1] = line
        else:
            lines.append("\t" + variant + ",")
    for line in lines:
        print(line)


def generate_to_wat_match_arms():
    for opcode, (instr, immediates) in INSTRUCTIONS.items():
        if instr in SNOWFLAKES:
            continue
        variant = paterncase(instr, immediates)
        if immediates:
            arm = (
                f'Instr::{variant} => format!("{instr} '
                + " ".join(["{}"] * len(immediates))
                + '", '
                + ", ".join(
                    f'{imm}.iter().map(|label| label.to_string()).collect::<Vec<_>>().join(" ")'
                    if RUST_TYPES.get(imm, "").startswith("Vec")
                    else imm
                    for imm in immediates
                )
                + "),"
            )
        else:
            arm = f'Instr::{variant} => "{instr}".into(),'
        print("\t" * 3 + arm)


def generate_to_wasm_match_arms():
    for opcode, (instr, immediates) in INSTRUCTIONS.items():
        if instr in SNOWFLAKES:
            continue
        variant = paterncase(instr, immediates)
        if immediates:
            arm = (
                f"Instr::{variant} => "
                + "{\n\t\t\t\t"
                + f"wasm.push(0x{opcode:02X});"
                + "\n\t\t\t\t"
                + "\n\t\t\t\t".join(
                    f"wasm.extend({imm}.to_bits().to_ne_bytes());"
                    if imm == "value" and instr.startswith("f")
                    else f"wasm.pack(*{imm});"
                    if imm != "labels"
                    else "\n\t\t\t\t".join(
                        [f"wasm.pack({imm}.len() - 1);", f"{imm}.iter().for_each(|idx| wasm.pack(*idx));"]
                    )
                    for imm in immediates
                )
                + "\n\t\t\t}"
            )
        else:
            arm = f"Instr::{variant} => wasm.push(0x{opcode:02X}),"
        print("\t" * 3 + arm)


def pascalcase(camel_case: str):
    words = camel_case.replace(".", "_").split("_")
    return "".join(word.capitalize() for word in words)


def paterncase(variant: str, fields: list[str]):
    variant = pascalcase(variant)
    if fields == ["value"]:
        variant += "(value)"
    elif fields:
        variant += " { " + ", ".join(fields) + " }"
    return variant


if __name__ == "__main__":
    with suppress(KeyboardInterrupt):
        main(sys.argv[0], *sys.argv[1:])
