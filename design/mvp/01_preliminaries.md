# 01 Preliminaries

## Semicolons

- semicolons are not automatically inserted
- semicolons are required in between statements

## `let`-binding

The behavior of `let` has been modified so that it:

- Allows redeclarations (like `var`)
- Prohibits reassignment (like `const`) without redeclaration. See
  [Mutability](06_mutability.md) about how reassignment is handled.

```typescript
let x = "hello";
let x = x.length; // x is now a number
```

## Removal of Legacy Syntax

- `with`: no one should be using this
- `eval`: no one should be using this
- `ternary`: superseded by `if`-`else` bing converted to an expression
- `switch`-`case`: superseded by [Pattern Matching](05_pattern_matching.md)
- `function`: arrow syntax is used exclusively. This will make supporting
  generators interesting.
