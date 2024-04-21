# 👻 The Egon Language

## Introduction

This will serve as an introduction to the language. Please see [examples](./egonlang-core/res/examples) to better understand things.

### Variables

Variables can be declared using the `let` keyword, their type and value. Variable values can be reassigned.

```
let a: number = 10;

a = 50;
```

The type can be left off if a value is provided.

```
let a = 10;

a = 50;
```

The value can be left off if a type is provdied.

```
let a: number;

a = 100;
```

### Constants

Constants can be declared using the `const` keyword, a value, and an optional type. Constant values can not be reassigned.

```
const a = 100;
const b: number = 0.5;

a = 50;

// out: SyntaxError: `a` is a const value and can't be reassigned

const result = a * b;
```

### Staticly Typed

Egon is a statically typed language.

```
let a = 5;
let b = "foo";

let result = a + b;
// out: TypeError: mismatched types: expected type `number` but received `string`

b = 10;
// out: TypeError: mismatched types: expected type `string` but received `number`

let c = if (true) { 123 } else { "foo" };
// out: TypeError: mismatched types: expected type `number` but received `string`

let d = if (true) { 123 } else { 456; };
// out: TypeError: mismatched types: expected type `number` but received `()`
```

### Types

| Type                             | Description                                      |                                      Example |
| -------------------------------- | ------------------------------------------------ | -------------------------------------------: |
| `()`                             | A type only containing the value `()`            |                                         `()` |
| `number`                         | 64bit floating point                             |                                    `123.456` |
| `string`                         | `"Hello World"`                                  |                                      `"foo"` |
| `bool`                           | `true`, `false`                                  |                                      `false` |
| `list<T>`                        | A growable sequence of `T`                       |                                  `[3, 2, 1]` |
| `tuple<T, U...>`                 | A fixed lenth sequence of `T, U, ...`            |                          `(true, 0, "foo",)` |
| `range`                          | A range between two numbers                      |                                      `0..10` |
| `function<tuple<P0, P1, ...> R>` | Function params (e.g. `P0`, `P1`) and return `R` | `(a: number, b: number): number => { a + b}` |


### Expressions

Egon supports the usual cast of expressions (e.g. literals, infix/prefix operations) in a C like language but also supports expression blocks, if/else, functions.

#### Literal Expressions

```
const literals: tuple<
    number,
    string,
    bool,
> = (
    123,
    "foo",
    false,
);
```

#### Collection Expressions

```
const collections: tuple<
    list<number>,
    tuple<string, string, number>
> = (
    [1, 2, 3],
    ("a", "b", 0,),
);
```

##### List Expressions

A growable sequence of `T`. Lists can only contain items of the same type.

```
[1, 2, 3]
```

##### Tuple Expressions

A fixed lenth sequence of `T, U, ...`. Used to group a small amount of related types together.

```
("foo", 2, false,)
```

#### Block Expressions

Blocks are code delimited by braces `{}`. Blocks can contain multiple statements and an optional return expression (by omitting the semicolon). Blocks without a return expression return `()`.

```
let result: number = { 
    let n = 5;

    { n * n } // Wrapped in a block expression that will return `n * n`
 };
```

#### If/Else Expressions

If/Else can be written as expressions it's then/else blocks are block expressions.

```
const is = true;

const value = if (is) { 123 } else { 456 };
```

#### Function Expressions

Functions can be written as expressions. The function body is a block expression.

```
// No op function returning `()`
(): () => { };

// Function to sum
(a: number, b: number): number => { a + b };

// Curried function to sum
(a: number): function<tuple<number>, number> =>
        (b: number): number => { a + b };
```

### Statements

#### Expressions vs Statements?

Expressions produce a value while statements do not.

```
let a: number = 123;
                ^-^  Expression `123` produces 123
    ^-------------^  Expression `a: number = 123` produces 123
^------------------^ Statement

123;
^-^  Expression `123` produces 123
^--^ Statement

["foo", "bar"];
        ^---^   Expression `"bar"` produces "bar"
 ^---^          Expression `"foo"` produces "foo"
^------------^  Expression `["foo", "bar"]` produces ["foo", "bar"]
^-------------^ Statement
```

Statements generally fall in to one of these categories:

- Expression Statements
- Declaration Statements
- Syntactical Statements

#### Expression Statements

Expression statements are expressions ending with a semicolon.

```
123;
"foo";
true;
[1, 2, 3];
(true, 0, 1,);
```

#### If/Else Statements

If/Else can be written as expression statements.

```
const is = true;
let value: number;

if (is) {
    value = 123;
} else {
    value = 456;
};
```

#### Variable Declaration Statements

...

#### Function Statements

Functions can be declared using a statement using the `fn` keyword and a function name.

```
fn sum (a: number, b: number): number => { a + b };
```

#### Type Alias Statements

Type aliases are a declaration statement allowing you to alias type names

```
type NumberList = list<number>;

let numbers: NumberList = ["foo"];

// out: TypeError: mismatched types: expected type `list<number>` but received `list<string>`
```

You can alias type aliases as well.

```
type NumberList = list<number>;
type ListOfNumbers = NumberList;

let numbers: ListOfNumbers = ["foo"];

// out: TypeError: mismatched types: expected type `list<number>` but received `list<string>`
```