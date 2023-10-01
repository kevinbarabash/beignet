declare type GetTypeName<T extends boolean | number | string> = T extends boolean ? "boolean" : T extends number ? "number" : T extends any ? "string" : never;
export declare const a: GetTypeName<true>;
export declare const b: GetTypeName<5>;
export declare const c: GetTypeName<"hello">;
