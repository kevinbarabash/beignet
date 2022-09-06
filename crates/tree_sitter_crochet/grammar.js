const tsx = require("tree-sitter-typescript/tsx/grammar.js");

module.exports = grammar(tsx, {
  name: "crochet",

  rules: {
    // remove sequence expression and optional flow-style type assertion
    parenthesized_expression: ($, previous) => {
      return seq("(", $.expression, ")");
    },
    // remove sequence expression
    _expressions: ($, previous) => {
      return $.expression;
    },
    expression: ($, previous) => {
      const choices = previous.members.filter(
        (member) => member.name !== "ternary_expression"
      );
      return choice(...choices);
    },
  },
});
