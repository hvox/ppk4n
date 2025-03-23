#!/usr/bin/env python3
from pathlib import Path

INDENTATION = 2 * " "
MOD_INDENTATION = "  "
ROOT = Path(__file__).resolve().parent.parent / "src"
QUEUE = [(ROOT, "main")]


def process_module(receiver: Path, module: str) -> list[str]:
    path = (receiver.parent if receiver.stem == "main" else receiver.with_suffix("")) / f"{module}.rs"
    lines = []
    for line in path.read_text().splitlines():
        code = line.lstrip()
        if code.startswith("//"):
            continue
        elif (code.startswith("mod ") or code.startswith("pub mod ")) and code.endswith(";"):
            child_module = code.removeprefix("pub ").removeprefix("mod ")[:-1]
            child = (path, child_module)
            if child not in QUEUE:
                QUEUE.append(child)
        indent = line[: len(line) - len(code)].replace("    ", INDENTATION).replace("\t", INDENTATION)
        line = indent + code
        if line.strip():
            lines.append(line)
    return lines


for (path, module) in QUEUE:
    module_path = (path.parent if path.stem == "main" else path.with_suffix("")) / f"{module}.rs"
    print(f"\n// Содержимое файла {module_path.relative_to(ROOT.parent)}")
    for line in process_module(path, module):
        print(line)
