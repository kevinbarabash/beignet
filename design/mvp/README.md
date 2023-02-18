# Escalier: MVP 🧣

Escalier is a compile-to-JavaScript language with tight TypeScript interop and
modern language features.

## Why Escalier?

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
JavaScript/TypeScript libraries. Escalier's type system is compatible with
TypeScript's type system. Escalier can parse .d.ts files and convert the types
into Escalier types. It can also output .d.ts files when compiling .esc source
code. This provides automatic, bidirectional, interop with TypeScript.

## Design Docs

0.  [Developer Expectations](00_developer_expectations.md)
1.  [Preliminaries](01_preliminaries.md)
2.  [Expressions](02_expressions.md)
3.  [Type Safety](03_type_safety.md)
4.  [Type Narrowing](04_type_narrowing.md)
5.  [Pattern Matching](05_pattern_matching.md)
6.  [Mutability](06_mutability.md)
7.  [Modules and Scripts](07_modules_and_scripts.md)
8.  [Visibility and Privacy](08_visibility_and_privacy.md)
9.  [Testing Target](09_testing_mode.md)
10. [Extensibility](10_extensibility.md)
11. [Bonus Features](11_bonus_features.md)
