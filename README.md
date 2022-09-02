# crochet 🧣

[![CI](https://github.com/crochet-lang/crochet/actions/workflows/ci.yml/badge.svg)](https://github.com/crochet-lang/crochet/actions/workflows/ci.yml)
[![codecov](https://codecov.io/gh/crochet-lang/crochet/branch/main/graph/badge.svg?token=0r6KE7snsL)](https://codecov.io/gh/crochet-lang/crochet)

A compile-to-JavaScript language with tight TypeScript integration.

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

See [crates/tree_sitter_crochet/README.md](crates/tree_sitter_crochet/README.md)
for details on how to modify the tree-sitter parser.
