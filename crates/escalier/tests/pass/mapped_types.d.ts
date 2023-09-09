declare type Custom<T> = {
    [P in keyof T]: T[P];
};
declare type Obj = {
    a: number;
    b?: number;
    c: boolean;
    d?: number;
};
declare type ReadonlyObj = {
    readonly a: number;
    readonly b?: number;
    readonly c: boolean;
    readonly d?: number;
};
declare type PartialObj = Partial<ReadonlyObj>;
export declare const custom_obj: {
    a?: number;
    b?: number;
    c?: boolean;
    d?: number;
};
export declare const partial_obj: {
    a?: number;
    b?: number;
    c?: boolean;
    d?: number;
};
