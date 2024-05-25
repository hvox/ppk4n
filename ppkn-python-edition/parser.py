from scanner import scan_tokens, Token, SyntaxError
from ast import Import, Path


def parse_ast(source: str | list[Token]):
    tokens = list(scan_tokens(source)) if isinstance(source, str) else source
    imports = []
    i = 0
    while tokens[i].typ != "eof":
        token = tokens[i]
        match token.typ:
            case "import":
                path, i = parse_path(tokens, i + 1)
                if tokens[i].typ == "as":
                    alias = parse_token("identifier", tokens, i + 1)
                    i += 2
                else:
                    alias = path.items[-1]
                imports.append(Import(path, alias))
            case "export":
                path, i


def parse_path(tokens: list[Token], start: int = 0) -> tuple[Path, int]:
    path = [parse_token(["identifier", "string"], tokens, start).value]
    i = start + 1
    while tokens[i].typ == ".":
        path.append(parse_token(["identifier", "string"], tokens, i + 1).value)
        i += 2
    return Path(path), i


def parse_token(typ: str | list[str], tokens: list[Token], start: int = 0):
    token = next(tokens)
    if isinstance(typ, str) and typ != token.typ or token.typ not in typ:
        if isinstance(typ, str):
            raise SyntaxError(f"Expected {typ}", token.start)
        typs = ", or ".join(typ)
        raise SyntaxError(f"Expected something like {typs}", token.start)
    return token


print(parse_ast('print("hello world")'))
