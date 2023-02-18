# 05 Pattern Matching

Pattern matching is like a `switch`-`case` but you can match against structured
data and the case statements can use the same patterns that are use with
`if`-`let`. Here's one example of what this looks like in Escalier:

```ts
type Event =
  | {type: "mousedown", x: number, y: number}
  | {type: "keydown", key: string}
  ;

declare let event: Event;

let result = match (event) {
    {type: "mousedown", x, y} -> \`mousedown: (\${x}, \${y})\`,
    {type: "keydown", key} -> \`keydown: \${key}\`
};
```

The reason for using `->` instead of `=>` was to differentiate the arms of the
`match` expression from arrow functions. The latter introduces a new stack frame
whereas the former does not.

Pattern matching is syntactic sugar for `if`-`let`-`else`. The example above can
be rewritten as the following:

```ts
type Event =
  | {type: "mousedown", x: number, y: number}
  | {type: "keydown", key: string}
  ;

declare let event: Event;

let result = if (let {type: "mousedown", x, y} = e) {
    \`mousedown: (\${x}, \${y})\`
} else if (let {type: "keydown", key} = e) {
    \`keydown: \${key}\
};
```

NOTES:

- All of the same patterns that can be use with [Type
  Narrowing](04_type_narrowing.md) can be used with pattern matching.
- While there is a [TC39
  Proposal](https://github.com/tc39/proposal-pattern-matching) for pattern
  matching, it feels overly complex. This is why an alternate syntax was adopted.
