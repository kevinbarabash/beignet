# 03 Extensibility

## Extensible Type System

There are lots of APIs that make use of data within strings, e.g. regexes, `document.createElement`,
`document.querySelector`, `react-router`, etc. TypeScript is able to infer types from some of these
APIs such as `createElement` and `querySelector`. This capability is likely to expand over time
now that TypeScript has conditional types, template literal types, and `infer` . That being said,
implementing type inference of this data within TypeScript's type system is complicated and much
slower than it could be.

Type inference in some cases may require access to external data, e.g. inferring the type of GraphQL
queries requires accessing a schema. There are plugins for VSCode that provide editor support for
GraphQL and SQL. These plugins provide validation of queries in the editor and can also generate
type definitions for the queries. It's a workable solution, but it results in a lot of generated
files that need to be kept up to date and may result in merge conflicts with other generated types.

Crochet will solve these issues by providing a plugin API for the type system that can:

- infer structured types based on literals (strings, regexes, tagged template literals)
- access external resources (file system, network, etc.) to support this type inference

## Linting

Crochet can provide a typed AST to a future linter. This would allow us to craft more powerful rules
than is currently possible with `eslint`.

## Code Highlighting

We can also leverage a typed AST to support better code highlighting. `rust-analyzer` provides
syntax highlighting that differentiates mutable and immutable variables. We could do the same
with Crochet.

## Hygienic Macros

TODO
