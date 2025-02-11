# Ppkn Programming Language
*Name is temporary and might change in the future*

Ppkn is a new programming language currently in its early stages of development. 
Everything about the language: its syntax, semantics, and features, --
is subject to change as the project evolves.


## Inspiration

This project initially was inspired by simple idea of a very portable
language for writing libraries that can be used from many languages
without rewriting anything. The idea is well expressed in
[this post](https://www.reddit.com/r/ProgrammingLanguages/comments/1c5uh56/is_there_a_programming_language_for_functions/). 
But at some point in development, it became clear that transpiling into
different languages while preserving code semantics is almost impossible
due to small overlap in capabilities among currently popular programming
languages.

The current plan is to implement a language that can be:
* interpreted;
* transpiled into C, Zig, Python or Rust;
* compiled into executable or Wasm;


## Main design principles

* **Strong static typing with local type inference** 
  `x := "Bubik" # Type of x is inferred to be Str`
* **Concise and readable syntax** 
  `map := {0: "zero", 42: "?"} # Hashmap definition`
* **Automatic memory management** 
  `x = y = "Heh" # Memory for "Heh" is freed when both x and y die`
* **Reasonableness above conventions**
  `123 & 321 == 65 # Evaluates to True, not 0`
  `(1 - 2) % 3 # Evaluates to 2, not -1`
* **Portability**
  Sources can be interpreted, compiled or transpiled without modifying the code.


## Questionable design decisions that should be reconsidered

* **Syntax Errors** 
  Parser error reporting is limited to the first error of first failed
  pattern, which in some cases leads to suggesting users to change code
  to more invalid one. 
  Error reporting should be redesigned and rewritten,
  and not only in parser!
* **Variable Declaration** 
  Variables are declared by syntax `foo: Bar = Something` with optional
  type, but not optional value, which makes it impossible to declare
  variable and initialize it later. 
  Also lack of keywords in the syntax might lead to
  hard to see definitions. 
  Adding keyword to the definition syntax should improve all specified
  aspects.
* **Unpredictable Type Guessing** 
  Final operands of bodies of `if` and `block` are dropped if they are
  typechecked after being discovered to be of `Unit` type.
  In other words, semantic of the language changes based on current
  state of typechecker, which might lead to random type errors occurring
  unpredictably for users. 
  This remark applies only to the typechecker in `hir_gen.rs`, other
  implemented typecheckers don't have that problem, but they also don't
  fully support `if` and `block` inside expressions yet, so they still
  might face this problem in the future. 
  This problem can be completely solved by introducing *semicolons* and
  forcing them at the end of statements in order to distinguish
  statement from expressions at body's ends.
* **Lack of semicolons**
  Lack of forced semicolons leads to ambiguity in some cases, which are
  currently usually resolved in more or less reasonable fashion, but
  sometimes some obscure errors occur, 
  for example see **Unpredictable Type Guessing**.
