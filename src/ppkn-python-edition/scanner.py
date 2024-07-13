from typing import NamedTuple, Any


KEYWORDS = {
    "and",
    "as",
    "class",
    "else",
    "false",
    "for",
    "if",
    "import",
    "or",
    "print",
    "return",
    "super",
    "this",
    "true",
    "var",
    "while",
}


class Token(NamedTuple):
    typ: str
    start: int
    end: int
    value: Any = None


class SyntaxError(Exception):
    def __init__(self, reason, position):
        super().__init__(f"{reason} at position {position}")
        self.reason = reason
        self.position = position


def scan_tokens(source: str):
    indentations = [0]
    i = 0
    while i < len(source):
        match source[i]:
            case " " | "\r":
                i += 1
            case "#":
                while i < len(source) and source[i] != "\n":
                    i += 1
            case "\n":
                yield Token("eol", i, i)
                indent = 0
                i += 1
                while i < len(source) and source[i] == "\t":
                    indent += 1
                    i += 1
                if i < len(source) and source[i] != "\n":
                    while indent < indentations[-1]:
                        indentations.pop()
                        yield Token("dedent", i, i)
                    if indent > indentations[-1]:
                        indentations.append(indent)
                        yield Token("indent", i, i)
            case "+" | "-" | "*" | "/" | "." | "(" | ")" as op:
                yield Token(op, i, i + 1)
                i += 1
            case '"':
                start = i
                while source[i] != '"':
                    i += 1
                string = source[start + 1: i]
                yield Token("string", start, i + 1, string)
                i = min(start + 1, len(source))
            case digit if digit in "0123456789":
                start = i
                while i < len(source) and source[i] in ".0123456789":
                    i += 1
                number = source[start:i]
                yield Token("number", start, i, number)
            case char if char.isalnum():
                start = i
                while source[i].isalnum():
                    i += 1
                word = source[start:i]
                yield Token(word, start, i) if word in KEYWORDS else Token("identifier", start, i, word)
            case unexpected:
                raise SyntaxError(f"Unexpected character {unexpected!r}", i)
    for _ in range(len(indentations) - 1):
        yield Token("dedent", i, i)
    yield Token("eof", i, i)


# for token in scan_tokens("( . )#( . )\n\t\n\t+-hea\n"):
#     typ, start, end, value = token
#     print(f"{start:3} {typ}" + (f" {value!r}" if value is not None else ""))
