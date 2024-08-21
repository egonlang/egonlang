# Egon Language Development Guide

## Requirements

- [rust](https://www.rust-lang.org/)
- [nextest](https://nexte.st/)
- [just](https://just.systems/)

## Technologies

- [lalrpop](https://lalrpop.github.io/lalrpop/)
- [logos](https://docs.rs/logos/latest/logos/)

## Project Structure

- [core](./egonlang-core/): Library responsible for lexing, and parsing egon code
- [verifier](./egonlang-verifier/): Library responsible verifying AST (e.g. syntax errors, type checking)
- [lsp](./egonlang-lsp/) Language server
- [cli](./egonlang-cli/): Command line tool to lex and parse egon code files 
- [vsc](./vsc/): VS Code language extension

## Build, test, verify, etc...

| Command                        | Description                                                                                        |
| ------------------------------ | -------------------------------------------------------------------------------------------------- |
| `just test`                    | Run all the tests                                                                                  |
| `just test-trace`              | Run all the tests and log using `verify-trace!` macro                                              |
| `just lint`                    | Run lint and any report errors                                                                     |
| `just verify`                  | Runs tests and linting                                                                             |
| `just verify-trace`            | Runs tests (logging with `verify-trace!` macro) and linting                                        |
| `just verify-with-logs`        | Runs tests and linting but dumps stdout and stderr to `out.log` & `err.log` respectively           |
| `just format`                  | Run formatting                                                                                     |
| `just build`                   | Build the project (debug)                                                                          |
| `just build-release`           | Build the project (release)                                                                        |
| `just clean`                   | Clean build artifacts                                                                              |
| `just cli COMMAND PATH`        | Run the egon command with the `COMMAND` (`lex`, `parse`, or `verify`) and the file `PATH` in `PWD` |
| `just cli-trace COMMAND PATH`  | Run the egon command (logging with `verify-trace!` macro)                                          |
| `just build-docker`            | Build the docker image for the egon language                                                       |
| `just run-docker COMMAND PATH` | Run the egon command in docker e.g. `$ just run docker parse ./path/to/file.eg`                    |
| `just lsp`                     | Start the egon language server                                                                     |

## CLI

### Lex

Lex an input file in to a list of tokens.

```shell
just cli lex ./res/examples/valid/range_expression_start_only.eg 
```

### Parse

Parse an input file in to an AST (abstract syntax tree).

```shell
just cli parse ./res/examples/valid/range_expression_start_only.eg 
```

### Verify

Parse an input file and verify the AST (e.g. syntax errors, type checking).

```shell
just cli verify ./res/examples/valid/range_expression_start_only.eg 
```

#### With Verify Tracing

Enable trace logs to help debug the verification command.

```shell
just cli-trace verify ./res/examples/valid/range_expression_start_only.eg 
```

## Run Single Test

```shell
just test path_056__UP_res_examples_invalid_infix_type_mismatch_string_eg
```

### With Verify Tracing

This will enable trace logs from the verifier.

```shell
just test-trace path_056__UP_res_examples_invalid_infix_type_mismatch_string_eg
```

## VS Code Extension

The [VS Code extension](./vsc/) supports syntax highlighting, snippets, and integration with the [language server](./egonlang-lsp/).

### Usage

1. Uninstall any previous version of the extension
2. Run `just clean build`
3. Install the latest build `./vsc/out/egon-language-*.vsix`
4. Open any egon file `*.eg`

### Commands

- Start/stop/restart language server
- Lex/parse/verify a file
