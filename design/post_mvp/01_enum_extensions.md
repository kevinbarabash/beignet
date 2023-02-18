# 01 Enums with Payloads

Many other languages allow arbitrary data to be attached to enum variants. This
provides a convenient way to model sum types. In JavaScript and TypeScript it's
common to use `.kind` or `.type` properties to differentiate related object
types. This has some drawbacks, see [Struct Types](#struct-types) below.

This feature extends the `enum` keyword to allow the follwoing:

NOTE: This example was adapted from an example in "The Rust Programming
Language" book.

```ts
// network.esc
enum IpAddr {
    V4(string),
    V6(string),
}

let home = IpAddr.V4("127.0.0.1");
let loopback = IpAddr.V6("::1");

declare let addr: IpAddr;

let addrStr: string = match addr {
    IpAddr.V4(address) -> address,
    IpAddr.V6(address) -> address,
};

// network.js
const IpAddr = {
    V4: class V4 { constructor(payload) { this.payload = payload } }
    V6: class V6 { constructor(payload) { this.payload = payload } }
}

const home = new IpAddr.V4("127.0.0.1");
const loopback = new IpAddr.V6("::1");

let address;
if (addr instanceof IpAddr.V4) {
    address = addr.payload;
} else if (addr instanceof IpAddr.V6) {
    address = addr.payload;
}

// network.d.ts
// TBD
```

Interop with TypeScript shouldn't be an issue since v5.0 will be unifying enums
and unions. We can represent a Escalier enum in TypeScript as a union of
qualified classes, e.g.

```ts
type IpAddrEnum = IpAddr.V4 | IpAddr.V6;
```

TODO: Add an example that shows enums with type variables
