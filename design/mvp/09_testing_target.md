# 09 Testing Target

Mocking top-level symbols in a module is a useful tool when writing tests.
JavaScript and TypeScript make this difficult for a number of reasons:

- Symbols have to be exported in order to be mocked, but exporting things just
  for testing isn't great since then that symbol could potentially be used is
  code that's shipped when it wasn't meant to be used
- If a symbol is used within the module where it's defined, the mocked version
  won't be used in that file. This can be worked around by moving those symbols
  to a separate file, but this is not a great solution since the limitations of
  mocking are driving code organization.

Escalier gets around these issues by having a "testing" compilation target. When
compiling for this target, Escalier will modify the code generated in the
following ways:

- All top-level symbols are assigned to properties on an `exports` object, even
  those that are private
- To prevent leaking private symbols to other modules, the `exports` object set
  private properties to be non-enumerable (using `Object.defineProperty`).
- All uses of those symbols within the file will be replaces by accessing them
  from the `exports` object
- The `exports` object will be exported as the default export
- Namespace imports will be converted to default imports
- Named imports will be converted to a default import and all references to the
  imported symbols will be replaced by accessing them on default import

This will make it easy for `jest` and other testing libraries to mock all
top-level symbols in a module with having to structure the code under test in
specific ways to support mocking.
