# 08 Visibility and Privacy

Escalier will have support for classes. These will roughly be the same as
TypeScript classes, but with the following changes:

- properties will be readonly and private by default (same for `static`
  properties)
- methods will be non-mutating (of the instance) and private by default
- defining callbacks as class properties will not be supported since autobinding
  will fill this need, see [Bonus Features](11_bonus_features.md).

```ts
// my_component.esc
type Props = { ... };

class MyComponent extends React.Component<Props> {
  apple: number;
  mut banana: number;
  pub orange: number;
  pub mut pear: number;

  // Even though state is mutable, we don't want to allow it to be mutated
  // directly so we omit the `mut` keyword on this instance variable.
  state: State;

  constructor(props: Props) {
    super(props);
    this.state = { ... };
  }

  pub getApple() {
    return this.apple; // It's okay for public methods to return private data
  }

  pub mut incrementBanana() {
    this.banana += 1;
  }

  // .setState() is be marked as `mut` on the React.Component interface so we
  // need to mark `handleClick` as mutating as well.
  mut handleClick(e: React.MouseEvent<>) {
    this.setState({ ... });
  }

  render() {
    return <button onClick={this.handleClick}>Click me!</button>;
  }
};

// my_component.js
class MyComponent extends React.Component {
  // non-public properties become private
  #apple: number;
  #banana: number;
  orange: number;
  pear: number;

  constructor(props) {
    super(props);
  }

  getApple() {
    return this.apple;
  }

  incrementBanana() {
    this.banana += 1;
  }

  #handleClick(e: React.MouseEvent<>) {
    this.setState({ ... });
  }

  render() {
    return <button onClick={(...args) => this.handleClick(...args)}>
      Click me!
    </button>;
  }
}

// my_component.d.ts
interface MyComponent {
  // non-public properties are omitted
  orange: number;
  pear: number;
  getApple(): number;
  incrementBanana(): number;
}

interface ReadonlyMyComponent {
  // mutable properties are omitted
  orange: number;
  // mutating methods are omitted
  getApple(): number;
}

interface MyComponentConstructor {
  new(props: Props): MyComponent;
}

declare export const MyComponent: MyComponentConstructor;
```
