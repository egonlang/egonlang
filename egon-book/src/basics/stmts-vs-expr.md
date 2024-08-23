# Statements vs Expressions

So far we've seen assignment statements, value expressions, block expressions. How does Egon define statements vs expressions?

## Expressions

An expression is code that executes and generates a value. The most common expressions are literal expressions such as a `number` (example: `123`), `string` (example: `"Hello"`), and `bool` (example: `false`).

### Unit Expression

TODO

### Literal Expression

TODO

### Identifier Expression

TODO

### Block Expression

TODO

### List Expression

TODO

### Tuple Expression

TODO

### Infix Expression

TODO

### Prefix Expression

TODO

### Assign Expression

TODO

### If Expression

TODO

### Function Expression

TODO

### Range Expression

TODO

### Type Expression

TODO

## Statements

A statement is code that executes but doesn't generate a value. Statements are often postfixed with a semicolon but not always.

### Expression Statement

A common statement is an expression statement. It executes the expression then disposes of it's value.

```egon
123;
"example";
true;
false;
```

### Assignment Statement

Assignment statements declare [`variables`](./variables.md) and [`constants`](./constants.md).

```egon
let greeting = "Hello";
```

### Return Statement

A `return` statement executes the return value expression then set's it as it's containing block's return value.

```egon
{
    return true;
};
```

`return` statements can not be used outside of block expressions.

## Function Statement

TODO

## Type Alias Statement

TODO

## Assert Type Statement

TODO
