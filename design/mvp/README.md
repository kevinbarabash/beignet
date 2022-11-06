# Crochet: MVP 🧣

Crochet is a compile-to-JavaScript language with tight TypeScript interop and
modern language features.

## Why Crochet?

### Productivity

- Performance - the language is implemented in Rust.
- Language constructs like pattern matching and more expressions make the
  language more flexible and easier to use.
- Hindley-Milner style type inference reduces the need to explicitly specify
  types in many situations.

### Safety

- Readonly by default with opt-in mutability
- Index access returns optional types.
- Function signatures include information about what exceptions the function
  throws.
- Proper handling of variance.

### Interoperability

After experimenting with different strongly typed compile-to-JS languages, one
common downside was that they all required bindings to be written to existing
JavaScript/TypeScript libraries. Crochet's type system is compatible with
TypeScript's type system. Crochet can parse .d.ts files and convert the types
into Crochet types. It can also output .d.ts files when compiling .crochet
source code. This provides automatic, bidirectional, interop with TypeScript.

## Design Docs

1.  [Preliminaries](01_preliminaries.md)
2.  [Expressions](02_expressions.md)
3.  [Type Safety](03_type_safety.md)
4.  [Type Narrowing](04_type_narrowing.md)
5.  [Pattern Matching](05_pattern_matching.md)
6.  [Mutability](06_mutability.md)
7.  [Bonus Features](07_bonus_features.md)
8.  [Other Differences](08_other_differences.md)
9.  [Meeting Developer Expectations](09_meeting_developer_expectations.md)
10. [Extensibility](10_extensibility.md)
