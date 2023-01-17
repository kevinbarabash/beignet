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
    new(): ReadonlyFoo;
    add(x: number, y: number): number;
};
export declare const Foo: FooConstructor;
export declare const f1: ReadonlyFoo;
export declare const f2: Foo;
export declare const msg1: "hello";
export declare const msg2: "hello";
