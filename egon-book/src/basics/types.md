# Types

Egon is a statically typed language providing type errors at compile time.

```egon
let a = 123;

a = "testing";
// TypeError: mismatched types: expected type `number` but received `string`
```

The type was inferred when declaring `a` but an explicit type can be defined.

```egon
let b: string = 123;

a = "testing";
// TypeError: mismatched types: expected type `string` but received `number`
```

## Types

- `number`
- `string`
- `bool`
- `()`
- `list<T>`
- `tuple<T, U, ...>`
- `function<tuple<Arg0, Arg1, ...>, R>`
- `range`