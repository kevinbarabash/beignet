# 00 Misc Post-MVP Ideas

## Visibility Controls (MVP?)

Right now all top-level symbols are exported by default. There should probably
be a way to control this.

## Different Scripts and Modules (MVP?)

Modules with side-effects are a bad thing, so it would be good to have a way to
prevent that from happening.

## Const Expressions

If an expression is composed of literals and operations that can be computed at
compile time, e.g. math operators, `.length` on string literals, etc. then we
can compute the result of the expression at compile time.

## Pure/Side-Effect Tracking

This can be seen as an extension of exception tracking from the MVP. There are
several benefits of knowing whether a function is pure or not. If a function is
pure we can safely memoize. If we know which side-effects a function can
generate we could use that information when writing tests to indicate to the
operator what mocking should be done.

## TypeScript-to-Crochet Converter

While Crochet has good TypeScript interop, it has a number of features not
available in TypeScript. Being able to easily convert TypeScript code to
Crochet would allow more code within existing projects to benefit from these
features.

## Range Types and Range Checking

The MVP types index access operations on `T[]` as `T | undefined`. If we had a
range type we could use it ensure index access on immutable arrays was safe. We
already gaurantee that index access on tuples is safe.

Having arithmetic const expressions in place already would be a pre-req for this
feature.

## Hygienic Macros

[Hygienic Macros](https://en.wikipedia.org/wiki/Hygienic_macro) are macros that
work on the AST instead of on the source text. This avoids issues like variable
capture.

These would be implemented as plugins to begin with. The plugin would
accept an AST iterator and return a valid statement or expression. The string
would be parsed and the resulting AST would replace the AST nodes consumed from
the iterator.

This process would occur before type checking which could allow the output to
have its types inferred and checked just like any other code.

[sweet.js](https://www.sweetjs.org) implements this for JavaScript, but without
the type checking.

Questions:

- How do we report type checking errors that occur in the generated code?

## Pipeline Operator

See [TC39 Proposal](https://github.com/tc39/proposal-pipeline-operator).

## Support for Mocking

In JavaScript it currently isn't possible to mock exports in such a way that
they're also mocked for the functions within the same module that use them.
Private functions that aren't exported aren't mockable at all. We could have a
`debug` mode for compilation that makes this easier. In particular such a mode
would do the following:

- Export a single object `exports` containing all top-level symbols
- Update all uses of top-level symbols to access them using `exports`
- Export all private top-level symbols
- Update all module imports to import `exports` and use it correctly

## Interpreter

One of the great things about JavaScript is that it can be used interactively in
the browser and in node. Crochet could benefit from having an interpreter as
well.

## Runtime Type Information

Having type information and associated docs could be very helpful for people
using the interpreter.

[typescript-rtti](https://github.com/typescript-rtti/typescript-rtti) provides
this functionality for TypeScript but requires
[ttypescript](https://github.com/cevek/ttypescript) which patches `tsc` on the
fly enabling it to run third-party transformers as part of the compilation
process.

## Integer Type

Right now all numbers are typed `number` which is a floating point number.
There are certain situations where having an explicit `int` type would be safer,
e.g. array indexing.

## Operator Overloading

https://github.com/tc39/proposal-operator-overloading

## Numeric Literal Suffixes

https://github.com/tc39/proposal-extended-numeric-literals
