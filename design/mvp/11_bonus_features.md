# 11 Bonus Features

There are lots of great features from other languages that we can bring to
Escalier to make it even more of a joy to use.

## Ranges

Range types could be very handy for working with arrays safely without having to
check if each element is defined or not.

Additionally, we could adopt Rust's syntax (which is very similar to Python's
syntax) for slices as syntactic sugar for calls to `.slice()`, e.g.

**Escalier**

```typescript
let tuple = [1, 2, 3, 4, 5];

let slice1 = [:3];  // tuple.slice(0, 3);  === [1, 2, 3]
let slice2 = [1:3]; // tuple.slice(1, 3);  === [2, 3]
let slice3 = [1:];  // tuple.slice(1);     === [2, 3, 4, 5]
let slice4 = [:-1]; // tuple.slice(0, -1); === [1, 2, 3, 4]
let slice5 = [:];   // tuple.slice();      === [1, 2, 3, 4, 5]
```

Post-MVP, I'd like to make ranges a first-class concept in Escalier along with an
associated type and type checking.

## Typed holes

This is a feature from Haskell/PureScript. It allows you to prefix an identifier
with `?` and it will return what its type should be in order for the code to
typecheck. In Haskell/PureScript this can be used to get a list of suggestions
for expressions to replace the typed hole with.

Question: What happens when you try to compile code that contains a typed hole?

Answer: We do the following:

- infer the type of the hole and report it
- output a .d.ts file for the file containing a typed hole
- output a .js file contain all top-level declarations except those that contain
  typed holes.

## Autobinding

One of the confusing aspects of using JavaScript is passing methods as
callbacks. To avoid this confusion when a method is passed as a callback,
Escalier will autobind it.

```tsx
// autobinding.esc
class MyComponents extends React.Component {
  constructor(props) {
    super(props);
    this.state = { count: 0 };
  }

  handleClick() {
    this.state.count++;
  }

  render() {
    return (
      <div>
        <h1>Click count: {this.state.count}</h1>
        <button onClick={this.handleClick}>Click me!</button>
      </div>
    );
  }
}

// autobinding.js
class MyComponents extends React.Component {
  constructor(props) {
    super(props);
    this.state = { count: 0 };
  }

  handleClick() {
    this.state.count++;
  }

  render() {
    return (
      <div>
        <h1>Click count: {this.state.count}</h1>
        <button onClick={(...args) => this.handleClick(...args)}>
          Click me!
        </button>
      </div>
    );
  }
}
```

NOTE: The reason we don't use `.bind` in the JavaScript output is that it's
slow.
