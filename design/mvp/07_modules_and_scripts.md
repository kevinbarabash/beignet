# 07 Modules and Scripts

Modules should not have any side-effects. This is not the case in JavaScript
and TypeScript which can make writing tests and other activities where you might
want to reload a fresh copy of a module very difficult.

At the same time, we still want to allow people to use Escalier for scripts which
necessitates side-effects. Post-MVP we'll be adding a REPL and we want people
to be able to copy/paste code between REPL and a script file (and vice versa).
To this end, there will be a way to specify whether a .esc file is a script
or a module. This may be a different file extension or compiler directive in the
file itself.

Escalier scripts will be allowed to run code that has side-effects at the
top-level while modules will not. Modules will be allowed to declare functions
and literals (including objects and tuples) at the top-level. Post-MVP we'll be
adding support for const expressions. These will be also be allowed at the
top-level of modules once support has been added. Top-level symbols in modules
can't be shadowed.

## Import and Exports

Modules can have imports and exports, but scripts can only have imports.

The import/export systax will be roughly the same as what's used by JavaScript
and TypeScript, but default imports/exports will not be allowed. To import a
default export from a TypeScript file you'll need to do the following:

```ts
import { default as Foo } from "./foo";
```

The reason for this is that default imports/exports makes it hard to use search.
While the LSP can mitigate this, but there can be certain situations where it
may not be feasible to use it.
