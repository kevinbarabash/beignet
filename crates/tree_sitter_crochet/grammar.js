const tsx = require("tree-sitter-typescript/tsx/grammar.js");

module.exports = grammar(tsx, {
  name: "crochet",

  rules: {
    // removes sequence expression and optional flow-style type assertion
    parenthesized_expression: ($, previous) => {
      return seq("(", $.expression, ")");
    },
    // removes sequence expression
    _expressions: ($, previous) => {
      return $.expression;
    },
    // removes ternary expression
    expression: ($, previous) => {
      const choices = previous.members.filter(
        (member) => member.name !== "ternary_expression"
      );
      return choice(...choices);
    },
    // removes with statement
    statement: ($, previous) => {
      const choices = previous.members.filter(
        (member) => member.name !== "with_statement"
      );
      return choice(...choices);
    },
  },
});
