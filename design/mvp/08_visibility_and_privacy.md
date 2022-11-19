# 08 Visibility and Privacy

- prevent deep imports (tooling can handle tree-shaking)

  - how does this interact with TypeScript modules that might not care?

- `pub` keyword from Rust is great because it's used for both visibility in
  terms of what's being exported from the component, but also in terms of what
  fields in an class are public or not

- `pub` is better than `private` since it makes everything private by default
  and users have to opt-in to make things public.

TODO: talk about how to prevent deep imports
