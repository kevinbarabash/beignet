export declare const regex: ReadonlyRegExp<"(?<foo>foo)|(?<bar>bar)", "g">;
export declare const result: readonly [string, string | undefined, string | undefined] & {
    readonly groups: {
        readonly foo?: string;
        readonly bar?: string;
    };
} | null;
