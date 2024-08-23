# Introduction

The egon language is a toy statically typed interpreted language. It exists purely as a learning project.

## Project Goals

- Design a language's grammar from the ground up
- To learn various forms of static code analyisis
  - Type Checking
  - Type Inference
  - Linting
  - Formatting
  - Identifier resolution
  - etc...
- Implementing a language server with more than error diagnostics
  - Go to definition
  - Go to usages
  - Semantic tokens
  - Refactoring
  - etc...

## Language Goals

- Errors should be compile/analyze time as much as possible
- Prefer expressions over statements
- Exhaustive pattern matching
- No exceptions/handling e.g. `Result`
- No null types e.g. `Option`
- Expressive macros e.g. Rust's `macro_rules!`
- Default immutability with explicit mutability
- Traits/Typeclasses
- Algebraic Data Types e.g. Rust's `enum`
- Destructuring
- Async/Coroutines
- Doc comments
- Modules

## Inspirations

### Rust

Rust is wonderful language and a defining influence on the Egon language. The language is challenging (e.g. traits vs interfaces/classes, borrowing, lifetimes) but with experience those things fade away and the simplicity of the language really shines. It's the simplicity of the language compiled with it's 

### Typescript

Typescript is a modern wonder providing an expressive type system for code that will run in a dynamically typed runtime evironment and has to interact with dynamically typed code. It's that type system that the Egon language draws inspiration from but to provide it in a statically typed runtime environment. 

## Resources

Here are some resources around designing and implementing a programming language.

- https://www.craftinginterpreters.com/
- https://www.reddit.com/r/ProgrammingLanguages/
- https://www.reddit.com/r/Compilers/