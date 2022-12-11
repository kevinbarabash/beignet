declare type Custom<A> = {
    -readonly [P in keyof A]+/: A[P];
};
declare type Obj = {
    readonly a: number;
    readonly b?: string;
    c: boolean;
    d?: number;
};
declare type PartialObj = Partial<Obj>;
export declare const custom_obj: Custom<Obj>;
export declare const partial_obj: PartialObj;
