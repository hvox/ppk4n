from __future__ import annotations
from typing import NamedTuple


class Module(NamedTuple):
    imports: list[Import]


class Import(NamedTuple):
    path: Path
    alias: str | None


class Export(NamedTuple):
    function: str
    alias: str


class Path(NamedTuple):
    items: list[str]
