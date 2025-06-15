<div align="center">
  <img src="https://github.com/user-attachments/assets/c897b8e9-a218-47a9-a25e-d19add09c36e" alt="Fox" width="100%" />

  ![Rust version](https://img.shields.io/badge/cargo-v1.86.0-f64d00)
</div>

## Overview
`Fox` is a fast Lox interpreter written in rust based from the specifications in the [Lox](https://craftinginterpreters.com/the-lox-language.html) programming language by Robert Nystrom. Lox is known as a dynamic scripting language but with some OOP aspects like classes and inheritance. Imports are **not** included in this interpreter, however it's easy to add given the structure of the code.

## Features
- [x] High Performance Lexer and Parser
- [x] Evaluating Expressions with Order (using Recursive Descent Parsing Algorithm)
- [x] Useful Error Messages
- [ ] Variable declaration
- [ ] Scoping
- [ ] Functions
- [ ] Classes & OOP
- [ ] REPL

## Setup
1. Clone this repository.
2. Install [the rust toolchain](https://www.rust-lang.org/tools/install) version `1.86.0`.
3. Build the projcet with `cargo build` or if you are in release mode, `cargo build --release`.
4. Run the interpreter with either the following subcommands depending on the output that you want: `tokenize` (lexer), `parse` (parser), or `evaluate` (evaluator). `cargo run <subcommand> <file_name>`.

## WIP
While this is a working interpreter at the moment, I wanted to finish it as a usable complete day-to-day interpreter that you would actually use. Providing a good developer experience is key to a good interpreter. Some features that I will definitely implement are pretty error messages, interactive REPL, and maybe in the future an LSP server with treesitter.
