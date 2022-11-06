# 08 Other Differences

## Removal of Legacy Syntax

- `with`: no one should be using this
- `eval`: no one should be using this
- `ternary`: superseded by `if`-`else` bing converted to an expression
- `switch`-`case`: superseded by pattern matching
- `function`: arrow syntax is used exclusively. This will make supporting
  generators interesting.

## No Default imports/exports

Default imports/exports make it difficult to maintain consistent naming of
variables across large projects. You'll still be able to import `default` using
a named import to support TypeScript interop.
