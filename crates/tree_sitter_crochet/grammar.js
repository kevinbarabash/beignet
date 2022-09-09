const tsx = require("tree-sitter-typescript/tsx/grammar.js");

const replaceField = (prev, name, replacement) => {
  const members = prev.members.map((member) => {
    if (member.type === "FIELD" && member.name === name) {
      return field(name, replacement);
    } else {
      return member;
    }
  });

  return seq(...members);
};

module.exports = grammar(tsx, {
  name: "crochet",

  rules: {
    // Removes sequence expression and optional flow-style type assertion
    parenthesized_expression: ($, prev) => {
      return seq("(", $.expression, ")");
    },

    // Removes sequence expression
    _expressions: ($, prev) => {
      return $.expression;
    },

    expression: ($, prev) => {
      // Removes ternary expression
      const choices = prev.members.filter(
        (member) => member.name !== "ternary_expression"
      );

      choices.push(alias($.if_statement, $.if_expression));
      choices.push($.do_expression);
      // choices.push($.try_statement);

      return choice(...choices);
    },

    // Removes with statement
    statement: ($, prev) => {
      const choices = prev.members.filter(
        (member) =>
          ![
            "with_statement",
            "if_statement",
            // "try_statement",
            "statement_block",
          ].includes(member.name)
      );
      return choice(...choices);
    },

    // Removes the optional semicolon
    statement_block: ($, prev) =>
      prec.right(seq("{", repeat($.statement), "}")),

    do_expression: ($) => seq("do", "{", repeat($.statement), "}"),

    for_statement: ($, prev) => replaceField(prev, "body", $.statement_block),
    for_in_statement: ($, prev) =>
      replaceField(prev, "body", $.statement_block),
    while_statement: ($, prev) => replaceField(prev, "body", $.statement_block),
    do_statement: ($, prev) => replaceField(prev, "body", $.statement_block),
    switch_case: ($, prev) => replaceField(prev, "body", $.statement_block), // replaces repeate($.statement)
    switch_default: ($, prev) => replaceField(prev, "body", $.statement_block), // replaces repeate($.statement)

    if_statement: ($, prev) =>
      prec.right(replaceField(prev.content, "consequence", $.statement_block)),
    else_clause: ($, prev) =>
      // Always require the alternative to be a block
      seq(
        "else",
        choice(alias($.if_statement, $.if_expression), $.statement_block)
      ),
  },
});
