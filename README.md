# Escalier

[![CI](https://github.com/escalier-lang/escalier/actions/workflows/ci.yml/badge.svg)](https://github.com/escalier-lang/escalier/actions/workflows/ci.yml)
[![codecov](https://codecov.io/gh/escalier-lang/escalier/branch/main/graph/badge.svg?token=0r6KE7snsL)](https://codecov.io/gh/escalier-lang/escalier)

A compile-to-JavaScript language with tight TypeScript integration. See
[design/mvp/README.md](design/mvp/README.md) for information about planned
features.

## Development

External Requirements:

- rust with the `wasm32-wasi` target installed
- [wasi-sdk](https://github.com/WebAssembly/wasi-sdk)
- [cargo-watch](https://github.com/watchexec/cargo-watch)
- node

### Demo

To run the demo, run the following commands in seperate terminals:

- `yarn watch-wasm`
- `yarn serve`

### Testing

- `cargo insta test`

### Parser

The parser is broken down into two parts:

- a tree-sitter parser which generates a CST
- a rust-based parser which walks the CST and produces an AST

NOTE: The AST models functions as lambdas whereas the CST does not.

See [crates/tree_sitter_escalier/README.md](crates/tree_sitter_escalier/README.md)
for details on how to modify the tree-sitter parser.
