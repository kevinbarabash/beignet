declare type Obj = {
    readonly a: number;
    readonly b?: string;
    c: boolean;
    d?: number;
};
declare type PartialObj = Partial<Obj>;
export declare const partial_obj: PartialObj;
