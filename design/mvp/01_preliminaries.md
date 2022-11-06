# 01 Preliminaries

## Semicolons

- semicolons are not automatically inserted
- semicolons are required in between statements

## `let`-binding

The behavior of `let` has been modified so that it:

- allows redeclarations (like `var`)
- doesn't allow reassignment (like `const`) without redeclaration

**Crochet**

```typescript
let x = "hello";
let x = x.length; // x is now a number
```
