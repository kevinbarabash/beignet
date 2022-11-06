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
