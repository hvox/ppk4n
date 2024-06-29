This project was inspired by [this post](https://www.reddit.com/r/ProgrammingLanguages/comments/1c5uh56/is_there_a_programming_language_for_functions/),
but at some point in development it became clear that translation into different languages with persistent semantics is almost impossible due to too little overlap of possibilities in different currently popular programming languages.

The current plan is to implement a language that can be:
* interpreted;
* translated into Rust, Zig or C;
* compiled into executable or Wasm;
