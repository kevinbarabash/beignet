declare type GetTypeName<T extends boolean | number | string> = T extends boolean ? "boolean" : T extends number ? "number" : T extends any ? "string" : never;
export declare const a: "boolean";
export declare const b: "number";
export declare const c: "string";
