# Ppkn Programming Language
*Name is temporary and might change in the future*

Ppkn is a new programming language currently in its early stages of development. 
Everything about the language: its syntax, semantics, and features, --
is subject to change as the project evolves.


## Project inspiration

This project initially was inspired by simple idea of a very portable language
for writing libraries that can be used from many languages without rewriting anything.
The idea is well expressed in [this
post](https://www.reddit.com/r/ProgrammingLanguages/comments/1c5uh56/is_there_a_programming_language_for_functions/). 
But at some point in development, it became clear that transpiling into different
languages while preserving code semantics is almost impossible due to small overlap
in capabilities among currently popular programming languages.

The current plan is to implement a language that can be: 
* Interpreted;
* Transpiled into C, Zig, Python or Rust;
* Compiled into executable or Wasm;


## Main design principles

* **Strong static typing with local type inference** `x := "Bubik" # Type of x is inferred to be Str`
* **Concise and readable syntax** `map := {0: "zero", 42: "?"} # Hashmap definition`
* **Automatic memory management** `x = y = "Heh" # Memory for "Heh" is freed when both x and y die`
* **Reasonableness above conventions** `123 & 321 == 65 # Evaluates to True, not 0`, `(1 - 2) % 3 # Evaluates to 2, not -1`
* **Portability** Sources can be interpreted, compiled or transpiled without modifying the code.
