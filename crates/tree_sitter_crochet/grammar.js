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

const dropLastMember = (node) => {
  return node.type === "SEQ" ? seq(...node.members.slice(0, -1)) : node;
};

module.exports = grammar(tsx, {
  name: "crochet",

  rules: {
    // Removes sequence expression and optional flow-style type assertion
    parenthesized_expression: ($, prev) => seq("(", $.expression, ")"),

    // Removes sequence expression
    _expressions: ($, prev) => $.expression,

    // Removes automatic semicolon insertion
    _semicolon: ($, prev) => ";",

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

    // Removes the automatic semicolon insertion from these nodes
    statement_block: ($, prev) => prec.right(dropLastMember(prev.content)),
    class_declaration: ($, prev) =>
      prec("declaration", dropLastMember(prev.content)),
    function_declaration: ($, prev) =>
      prec.right("declaration", dropLastMember(prev.content)),
    generator_function_declaration: ($, prev) =>
      prec.right("declaration", dropLastMember(prev.content)),

    // Requires these statements ot use { }
    for_statement: ($, prev) => replaceField(prev, "body", $.statement_block),
    for_in_statement: ($, prev) =>
      replaceField(prev, "body", $.statement_block),
    while_statement: ($, prev) => replaceField(prev, "body", $.statement_block),
    do_statement: ($, prev) => replaceField(prev, "body", $.statement_block),
    switch_case: ($, prev) => replaceField(prev, "body", $.statement_block), // replaces repeate($.statement)
    switch_default: ($, prev) => replaceField(prev, "body", $.statement_block), // replaces repeate($.statement)

    // Add do-expressions
    do_expression: ($) => seq("do", "{", repeat($.statement), "}"),

    // Make if-else an expression
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
