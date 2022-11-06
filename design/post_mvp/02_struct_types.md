# 02 Struct Types

In JavaScript and TypeScript we often use a `.type` or `.kind` field on objects
as a way of identifying an object type. This is great from a data serialization
and deserialization aspect because there's nothing to do. There's a drawback
though, there isn't a way to attach methods to the data because everything's
just an `Object`.

Struct types can solve this by replacing an object type with a simple class
where the constructor takes a single argument which is a POJO with all of the
struct's properties.

NOTE: This example was adapted from an example in the serde_json documentation.

```ts
// user.crochet
struct User {
    name: string,
    age: number,
    phones: string[],
};

let user = User {
    name: "John Doe",
    age: 43,
    phones: [
        "+44 1234567",
        "+44 2345678"
    ]
};

// user.js
class User {
    constructor(values: {name: string, age: number, phones: string[]}) { ... }
}

const user = new User({
    name: "John Doe",
    age: 43,
    phones: [
        "+44 1234567",
        "+44 2345678"
    ]
});
```

The JavaScript class that is generated for the struct could be implemented to
optimize speed (by copying the values over) or memory (creating getters/setters
for each property).

Methods can be attached to the struct type using traits which are described in
the next section.

One of the benefits of POJOs was that serialization/deserialization required no
additional work. In order for structs to be serialized/deserialized, we will
need to implement a `.toJSON()` method on the struct and implement a `reviver`
that can be passed to `JSON.parse()`.

Since the `.type` or `.kind` is no longer present in the object itself we'll
need to tell the deserializer what type we're trying to parse. Each struct
capable of deserialization needs to implement a `.fromJSON(entries)` method.
This will be called by the reviver that `deserialize` passed to `JSON.parse()`.

```ts
// read_user.crochet
let user = User.fromJSON(jsonStr)?;
```

NOTES:

- If `User.fromJSON()` fails, then a `Result.Err` will be returned
- Structs produce both value-level and type-level entries in the type system,
  this allows us to attach "static" methods like `.fromJSON(entries)` to it
