# Blocks

Block expressions (delimited by `{}`) create a new scope containing statements and an optional return expression.

```egon
let a = 123;

let result: number = {
    let b = 100;

    a + b
};

b; // TypeError: `b` is not defined
```

## Return Value

The last expression of a block is the block's implicit return value. An explicit value can be returned using the `return` statement.

```egon
// Implicit return
{
    [1, 2, 3]
};

// Explicit return
{
    return [1, 2, 3];
};
```