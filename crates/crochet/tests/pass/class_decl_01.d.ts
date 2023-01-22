declare type Foo = {
    readonly msg: string;
    bar(): string;
    get baz(): string;
    set qux(msg: string);
};
declare type ReadonlyFoo = {
    readonly msg: string;
    bar(): string;
    get baz(): string;
};
declare type FooConstructor = {
    readonly tag: "Foo";
    new(): ReadonlyFoo;
    add(x: number, y: number): number;
};
export declare const Foo: FooConstructor;
export declare const f1: ReadonlyFoo;
export declare const f2: Foo;
export declare const msg1: string;
export declare const msg2: string;
