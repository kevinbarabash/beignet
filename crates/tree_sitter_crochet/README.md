# tree_sitter_crochet

Defines a parser for the Crochet programming language as a tree-sitter grammar.

## Development

### `yarn generate`

Regenerates the files in the `src/` and `bindings/` directories.
This must be run after any changes to grammar.js.

### `yarn playground`

Creates a `wasm` build of the parser and starts a server which provides
a web based playground for manual testing of the parser.

### `yarn test`

Runs the tree-sitter tests in the `corpus/` directory.
