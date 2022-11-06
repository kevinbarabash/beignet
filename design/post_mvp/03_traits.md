# 24 Traits

## Motivation

JavaScript and TypeScript use mixins as a way of sharing functionality across
multiple classes that don't share a common superclass (without infecting
`Object`).

Mixins are bad for a number of reasons:

- In the past, there hasn't been a way of isolating mixins (this can be done now
  using `Symbol`s, but requires the developer to know about Symbols and use
  them)
- A new class is created for each mixin that is used with a class. This results
  in a bunch of extra constructor calls when creating an instance as well as
  extra links in the prototype chain to access properties/methods on the
  original class

Traits avoid these issues by:

- Adding trait methods directly to the struct class
- Using `Symbol`s to isolate traits from one another

```ts
// serialization.crochet
trait Serializable {
    static fromJSON(entries: [string, Serializable]): Result<Self, string>;
    toJSON(): string;
}

// serialization.js
export const Serializable = {
    fromJSON: Symbol(),
    toJSON: Symbol(),
};

// user.crochet
impl Serializable for User {
    static fromJSON(json: string) {
        // TODO
    }
    toJSON() {
        let mut result = `{"name": ${name.toJSON()}`;
        result += `, "age": ${age.toJSON()}`;
        result += `, "phones": ${phones.toJSON()}}`;
        return result;
    }
}

// user.js
import { Serializable } from "./serialization";

User[Serializable.fromJSON] = () => {
    // implementation goes here
};
User.prototype[Serializable.toJSON] = function () {
    // function() is used here instead of () => {} so that we have access to
    // the correct `this`
};

// user.d.ts
// TBD: look at lib.es5.d.ts to see how TypeScript uses interfaces to declare
// static methods on classes in the standard library
```

NOTES:

- traits can be used as types
- trait implementations must appear in either:
  - the file where the trait is defined
  - the file where the type implementing the trait is defined

## Deriving Traits with Default Implementations

In some cases, it's possible for traits to have a default implemention. In this
case, the trait can be added to any struct (or class) that fulfills and type
constraints that the trait requires. This is done using the `@derive()`
decorator.

Questions:

- What does the default implementation of `.fromJSON()` look like?
- How we can we compose nested Serializable structs when deserializing?

```ts
// user.crochet
import { Serializable } from "./serialization";

@derive(Serializable)
struct User {
    name: string,
    age: number,
    phones: string[],
}
```

## Using traits

In order to use a trait, it must be imported. This serves a multiple purposes:

- It reduces the amount of code
- If multiple traits on implemented for the same type and if there's overlap in
  the method names, importing the trait specifies which methods are available to
  use in this file.

```ts
// write_user.crochet
import { Serializable } from "./serialization";

declare let user: User;

user.toJSON();
```

NOTES:

- The trait needs to be imported

TODO: talk about using traits, we need to import the trait to be able to use
the methods that the trait provides. Also, what happens when two traits are
in scope that provide a method with the same name.
