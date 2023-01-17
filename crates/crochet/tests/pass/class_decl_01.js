class Foo {
    static tag = "Foo";
    constructor(){
        return undefined;
    }
    bar() {
        return "hello";
    }
    add(x, y) {
        return x + y;
    }
    baz() {
        return "world!";
    }
    qux(msg) {
        return undefined;
    }
}
export const f1 = new Foo();
export const msg1 = f1.bar();
export const f2 = new Foo();
f2.qux = "goodbye";
export const msg2 = f2.bar();
