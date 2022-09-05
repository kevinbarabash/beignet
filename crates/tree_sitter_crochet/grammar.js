const javascript = require("./grammar/tsx");

module.exports = grammar(javascript, {
  name: "crochet",

  // _expressions: ($, previous) => {
  //   const choices = previous.choices.filter(
  //     (choice) => choice === $.expression
  //   );
  //   return choice(...choices);
  // },

  rules: {
    // remove sequence expression and optional flow-style type assertion
    parenthesized_expression: ($, previous) => {
      return seq("(", $.expression, ")");
    },
    // remove sequence expression
    _expressions: ($, previous) => {
      return $.expression;
    },
  },
});
