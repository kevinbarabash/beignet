declare type GetTypeName<A extends number | string> = A extends number ? "number" : "string";
export declare const a: GetTypeName<5>;
export declare const b: GetTypeName<"hello">;
