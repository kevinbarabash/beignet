declare type Foo = {
    readonly msg: string;
    readonly tag: "Foo";
    bar(): "hello";
    add(x: number, y: number): number;
};
declare type FooConstructor = {
    new(): Foo;
};
declare type ReadonlyFoo = {
    readonly msg: string;
    readonly tag: "Foo";
    bar(): "hello";
    add(x: number, y: number): number;
};
export declare const Foo: FooConstructor;
