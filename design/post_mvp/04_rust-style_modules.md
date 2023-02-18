# 04 Rust-style Modules

TODO:

- Change module imports to work more like Rust's `use` keyword
  - Modules aren't imported by path
  - Use `pkg` instead of `crate`
  - Have a `module.esc` file to control visibility of modules in the current
    folder
    - Can we do better by not having to do `mod foo` for each modules in the
      folder? We really only care about making things public or re-exporting
      symbols.
  - We can drop the `export` keyword and use `pub` for symbols we want to export
    instead
