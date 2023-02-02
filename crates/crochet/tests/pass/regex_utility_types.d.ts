declare type Regex = {
};
declare type RegexMatch = {
};
export declare const matchAll: <A>(str: string, regex: A) => RegexMatch<A>;
export declare const regex: Regex<"(foo)(bar)", "g">;
export declare const result: {
    readonly 0: string;
    readonly 1: string;
    readonly 2: string;
};
