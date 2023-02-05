export const regex = /(?<foo>foo)|(?<bar>bar)/g;
export const result = "foobarbaz".match(regex);
let $temp_0;
if (regex.test("foo")) {
    $temp_0 = undefined;
}
$temp_0;
