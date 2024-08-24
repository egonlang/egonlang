# Statements vs Expressions

So far we've seen assignment statements, value expressions, block expressions. How does Egon define statements vs expressions?

## Expressions

An expression is code that executes and generates a value.

### Unit Expression

The `()` "unit" expression is a type with a singular value. It represents when there is no value present. Blocks without a return value return the value of `()`.

```egon
let a: number = {};
// TypeError: mismatched types: expected type `number` but received `()`
```

### Literal Expression

Literal expressions generate the value types: `number` (example: `123`), `string` (example: `"Hello"`), and `bool` (example: `false`).

```egon
true;
false;
100;
15.25;
-60;
-123.456;
"Hello!";
```

### Identifier Expression

TODO

### Block Expression

Block expressions (delimited by `{}`) create a new scope containing statements and an optional return expression.

```egon
let a = 123;

let result: number = {
    let b = 100;

    a + b
};

b; // TypeError: `b` is not defined
```

#### Return Value

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

### List Expression

List expressions generate a growable collection of same typed values.

```egon
[1, 2, 3, false];
// TypeError: mismatched types: expected type `number` but received `bool`
```

### Tuple Expression

Tuple expressions generate a fixed sized collection of mixed typed values.

```egon
(1, "foo", true,);
```

<div class="warning">
The trailing comma is required at the moment.
</div>

### Infix Expression

Infix expressions operate on two expressions (left and right) then return result value.

```egon
// Return `number`
10 + 10;
1 / 10;
5 * 20;
20 - 10;
10 % 5;

// Return `bool`
10 > 2;
5 < 50;
1 <= 10;
10 >= 1;
false != true;
"foo" == "foo";
```

### Prefix Expression

Prefix expressions operate on a single expression (right) then return resulting value.

```
// Return `bool`
!true;
!false;

// Return `number`
-10;
```


### Assign Expression

Assign expressions assign a value to an identifier then return the value.

```
let a = 123;

a = 456; // This is the assign expression
```

### If Expression

If expressions evalutate a condition expression then return either one value or the `else` value.

```egon
let is_less = if 10 < 100 { true } else { false };
```

<div class="warning">
Semicolons are required when using if expressions as statements.
</div>

```egon
if true { 
} else { 
}; // Semicolon is required
```

### Function Expression

Function expressions return a new anonynous function.

```egon
(a: number, b: number): number => {
    a + b
}
```

<div class="warning">
Explicit return types are required at this time!
</div>

### Range Expression

Range expressions return a value with inclusive `start` and exclusive `end` values.

```egon
1..25;
..5;
2..;
```

Range expressions can also use inclusive `end` values but prepending `=`;

```egon
1..=25;
```

### Type Expression

TODO

## Statements

A statement is code that executes but doesn't generate a value. Statements are often postfixed with a semicolon but not always.

### Expression Statement

A common statement is an [expression](#expressions) statement. It executes the expression then disposes of it's value.

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
const value = 123;
```

Types are inferred but can be explicited specified.

```egon
let is_example: bool = true;
const values: list<number> = [123, 456, 789];
```

<div class="warning">
If types can't be inferred the type becomes <code>unknown</code>, causing a compile error.
</div>

### Return Statement

A `return` statement executes the return value expression then set's it as it's containing block's return value.

```egon
{
    return true;
};
```

<div class="warning">
<code>return</code> statements can not be used outside of block expressions.
</div>

## Function Statement

`fn` statements declare a new named function.

```egon
fn sum (a: number, b: number): number => {
    a + b
}
```

<div class="warning">
Explicit return types are required at this time!
</div>

## Type Alias Statement

`type` statements allow aliasing types with new names.

```egon
type NumberList = list<number>;
```

<div class="warning">
Alias names must be capitalized!
</div>

## Assert Type Statement

`assert_type` statements perform compile time type assertions. This is mostly for testing type inference while developing the language.

```egon
let a = 123;
assert_type a, number;

let b = { true };
assert_type b, string;
// TypeError: mismatched types: expected type `string` but received `bool`
```
