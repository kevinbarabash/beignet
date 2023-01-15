declare type Foo = {
    readonly msg: string;
    bar(): "hello";
    get baz(): "world!";
    set qux(msg: string);
};
declare type ReadonlyFoo = {
    readonly msg: string;
    bar(): "hello";
    get baz(): "world!";
};
declare type FooConstructor = {
    readonly tag: "Foo";
    new(): Foo;
    add(x: number, y: number): number;
};
export declare const Foo: FooConstructor;
