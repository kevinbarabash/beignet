# 07 Modules and Scripts

Modules should not have any side-effects. This is not the case in JavaScript
and TypeScript which can make writing tests and other activities where you might
want to reload a fresh copy of a module very difficult.

At the same time, we still want to allow people to use Crochet for scripts which
necessitates side-effects. One option is to require some sort of entry point
function like `main()` to be in a script. This feels at odds with how people
traditionally write scripts. Also, we'd like people to be able to copy/paste
code from a REPL (post-MVP) into a script and vice versa. Requiring a `main()`
function to be in scripts would be at odds with this.

To this end, there will be a way to specify whether a .crochet file is a script
or a module. This may be a different file extension or compiler directive in
the file itself.

Crochet scripts will be allowed to run code that has side-effects at the
top-level while modules will not. Modules be allowed to declare functions and
literals (including objects and tuples) at the top-level. Post-MVP we'll be
adding support for const expressions. These will be also be allowed at the
top-level of modules once support has been added. Top-level symbols in modules
can't be shadowed.

## Import and Exports

Modules can have imports and exports, but scripts can only have imports.

Imports/exports come in two varieties: named and namespace. There are no
default imports/exports. The reason for decision is that default exports makes
it hard to use search. While the LSP can mitigate this, but there can be
certain situations where it may not be feasible to use it.

TODO: define syntax for imports
