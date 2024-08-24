# CLI

[Source](https://github.com/egonlang/egonlang/tree/main/egonlang-cli)

## Command

The `egon` command can lex, parse, and verify Egon code. If working from the repo then `just cli` can be used.

## Lexing

Returns a list of tokens from Egon code.

```shell
egon lex ./path/to/file.eg
```

## Parsing

Returns an AST from Egon code.

```shell
egon parse ./path/to/file.eg
```

## Verify

Analyze Egon code and return any errors.

```shell
egon verify ./path/to/file.eg
```