declare type GetTypeName<A extends boolean | number | string> = A extends boolean ? "boolean" : A extends number ? "number" : "string";
export declare const a: GetTypeName<true>;
export declare const b: GetTypeName<5>;
export declare const c: GetTypeName<"hello">;
