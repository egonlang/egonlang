# tracelog

Log messages with line/column info attached.

## Docs

https://egonlang.github.io/egonlang/docs/tracelog/index.html

## Usage

```rust
tracelog::tracelog!("Hello {}", "World");
// Hello World
// at file.rs:10:25

tracelog::tracelog!(label=foo, bar; "Hello {}", "World");
// [foo, bar] Hello World
// at file.rs:10:25
```