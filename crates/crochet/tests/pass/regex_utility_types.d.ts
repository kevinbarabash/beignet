declare type Regex = {
};
declare type RegexMatch = {
};
export declare const matchAll: <A>(str: string, regex: A) => RegexMatch<A>;
export declare const regex: Regex<"(foo)(bar)", "g">;
export declare const result: readonly [string, string, string];
