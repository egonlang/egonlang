# Egon Language Development Guide

## Requirements

- [rust](https://www.rust-lang.org/)
- [nextest](https://nexte.st/)
- [just](https://just.systems/)

## Technologies

- [lalrpop](https://lalrpop.github.io/lalrpop/)
- [logos](https://docs.rs/logos/latest/logos/)

## Build, test, verify, etc...

| Command                        | Description                                                                              |
| ------------------------------ | ---------------------------------------------------------------------------------------- |
| `just test`                    | Run all the tests                                                                        |
| `just lint`                    | Run lint and any report errors                                                           |
| `just verify`                  | Runs tests and linting                                                                   |
| `just verify-with-logs`        | Runs tests and linting but dumps stdout and stderr to `out.log` & `err.log` respectively |
| `just format`                  | Run formatting                                                                           |
| `just build`                   | Build the project (debug)                                                                |
| `just build-release`           | Build the project (release)                                                              |
| `just clean`                   | Clean build artifacts                                                                    |
| `just cli COMMAND PATH`        | Run the egon command with the `COMMAND` (`lex` or `parse`) and the file `PATH` in `PWD`  |
| `just build-docker`            | Build the docker image for the egon language                                             |
| `just run-docker COMMAND PATH` | Run the egon command in docker. This mounts `PWD`.                                       |
