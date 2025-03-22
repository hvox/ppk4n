from pathlib import Path

INDENTATION = 2 * " "
MOD_INDENTATION = "  "
ROOT = Path(__file__).resolve().parent.parent / "src"


def merge_module(receiver: Path, module: str) -> list[str]:
    path = (receiver.parent if receiver.stem == "main" else receiver.with_suffix("")) / f"{module}.rs"
    lines = []
    for line in path.read_text().splitlines():
        code = line.lstrip()
        if code.startswith("//"):
            continue
        elif (code.startswith("mod ") or code.startswith("pub mod ")) and code.endswith(";"):
            child_module = code.removeprefix("pub ").removeprefix("mod ")[:-1]
            lines.append(f"pub mod {child_module} " "{")
            for line in merge_module(path, child_module):
                lines.append(MOD_INDENTATION + line)
            lines.append("}")
            continue
        indent = line[: len(line) - len(code)].replace("    ", INDENTATION).replace("\t", INDENTATION)
        line = indent + code
        lines.append(line)
    return lines


for line in merge_module(ROOT, "main"):
    print(line)
