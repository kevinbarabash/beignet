export const regex_g = /(?<foo>foo)|(?<bar>bar)/g;
export const result_g = "foobarbaz".match(regex_g);
export const regex = /(?<foo>foo)|(?<bar>bar)/;
export const result = "foobarbaz".match(regex);
let $temp_0;
if (regex.test("foo")) {
    $temp_0 = undefined;
}
$temp_0;
