const tsx = require("tree-sitter-typescript/tsx/grammar.js");

module.exports = grammar(tsx, {
  name: "crochet",

  rules: {
    // Removes sequence expression and optional flow-style type assertion
    parenthesized_expression: ($, previous) => {
      return seq("(", $.expression, ")");
    },

    // Removes sequence expression
    _expressions: ($, previous) => {
      return $.expression;
    },

    expression: ($, previous) => {
      // Removes ternary expression
      const choices = previous.members.filter(
        (member) => member.name !== "ternary_expression"
      );
      // Makes if-else an expression
      choices.push($.if_expression);
      return choice(...choices);
    },

    // Removes with statement
    statement: ($, previous) => {
      const choices = previous.members.filter(
        (member) =>
          member.name !== "with_statement" && member.name !== "if_statement"
      );
      return choice(...choices);
    },

    else_clause: ($, previous) => {
      // Always require the alternative to be in a block
      return seq("else", choice($.if_expression, $.statement_block));
    },
    if_expression: ($, previous) => {
      return prec.right(
        seq(
          "if",
          field("condition", $.parenthesized_expression),
          // Always require the alternative to be in a block
          field("consequence", $.statement_block),
          optional(field("alternative", $.else_clause))
        )
      );
    },
  },
});
