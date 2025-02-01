# Ppkn Programming Language
*Name is temporary and might change in the future*

Ppkn is a new programming language currently in its early stages of development. 
Everything about the language: its syntax, semantics, and features, --
is subject to change as the project evolves.


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
