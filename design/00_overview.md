# 00 Overview

Crochet is a compile-to-JavaScript language with tight TypeScript interop and
modern language features.

## Why Crochet?

### Productivity

- performance
- language constructs like pattern matching, more expressions,
- Hindley-Milner style type inference reduces the need to explicitly provide
  types in many situations

### Safety

- readonly by default with opt-in mutability
- tracking whether a function can through an exception
- proper handling of variance

### Interoperability

After experimenting with different strongly typed compile-to-JS languages, one
common downside was that they all required bindings to be written to existing
JavaScript/TypeScript libraries. Crochet's type system is compatible with
TypeScript's type system. Crochet can parse .d.ts files and convert the types
into Crochet types. It can also output .d.ts files when compiling .crochet
source code. This provides automatic, bidirectional, interop with TypeScript.

## Design Docs

1. [Type Safety](01_type_safety.md)
2. [Mutability](02_mutability.md)
3. [Extensibility](03_extensibility.md)
4. [Differences from JavaScript](04_differences_from_javascript.md)
5. [Meeting Developer Expectations](05_meeting_developer_expectations.md)
6. [Bonus Features](06_bonus_features.md)
