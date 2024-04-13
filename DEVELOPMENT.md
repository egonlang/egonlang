# Egon Language Development Guide

## Requirements

- [rust](https://www.rust-lang.org/)
- [just](https://just.systems/)

## Build, test, verify, etc...

| Command                 | Description                                                                              |
| ----------------------- | ---------------------------------------------------------------------------------------- |
| `just test`             | Run all the tests                                                                        |
| `just lint`             | Run lint and any report errors                                                           |
| `just verify`           | Runs tests and linting                                                                   |
| `just verify-with-logs` | Runs tests and linting but dumps stdout and stderr to `out.log` & `err.log` respectively |
| `just format`           | Run formatting                                                                           |
| `just build`            | Build the project (debug)                                                                |
| `just build-release`    | Build the project (release)                                                              |
| `just clean`            | Clean build artifacts                                                                    |
