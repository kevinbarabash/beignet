declare type Custom<T> = {
    [P in keyof T]: T[P];
};
declare type Obj = {
    a: number;
    b?: string;
    c: boolean;
    d?: number;
};
declare type ReadonlyObj = {
    readonly a: number;
    readonly b?: string;
    readonly c: boolean;
    readonly d?: number;
};
declare type PartialObj = Partial<ReadonlyObj>;
export declare const custom_obj: Custom<ReadonlyObj>;
export declare const partial_obj: PartialObj;
