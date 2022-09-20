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

      choices.push($.do_expression);
      choices.push($.if_expression);
      choices.push($.match_expression);
      choices.push($.try_statement);

      return choice(...choices);
    },

    // Removes with statement
    statement: ($, prev) => {
      const choices = prev.members.filter(
        (member) =>
          ![
            "with_statement",
            "if_statement",
            "try_statement",
            "throw_statement",
            "statement_block",
          ].includes(member.name)
      );
      return choice(...choices);
    },

    // Removes the automatic semicolon insertion from these nodes
    class_declaration: ($, prev) =>
      prec("declaration", dropLastMember(prev.content)),
    function_declaration: ($, prev) =>
      prec.right("declaration", dropLastMember(prev.content)),
    generator_function_declaration: ($, prev) =>
      prec.right("declaration", dropLastMember(prev.content)),
    lexical_declaration: ($) =>
      seq(
        // TODO: get rid of "const"
        field("kind", choice("let", "const")),
        optional(field("rec", "rec")),
        field("decl", $.variable_declarator),
        $._semicolon
      ),

    statement_block: ($, prev) =>
      seq(
        "{",
        repeat($.statement),
        // We define an alias here so that it's easyt to check if the
        // last named child in the block is an expression or not.
        optional(alias($.expression, $.expression)),
        "}"
      ),

    // Requires these statements ot use { }
    for_statement: ($, prev) => replaceField(prev, "body", $.statement_block),
    for_in_statement: ($, prev) =>
      replaceField(prev, "body", $.statement_block),
    while_statement: ($, prev) => replaceField(prev, "body", $.statement_block),
    do_statement: ($, prev) => replaceField(prev, "body", $.statement_block),
    switch_case: ($, prev) => replaceField(prev, "body", $.statement_block), // replaces repeate($.statement)
    switch_default: ($, prev) => replaceField(prev, "body", $.statement_block), // replaces repeate($.statement)

    if_expression: ($) =>
      prec.right(
        seq(
          "if",
          "(",
          field("condition", choice($.let_expression, $.expression)),
          ")",
          field("consequence", $.statement_block),
          optional(field("alternative", $.else_clause))
        )
      ),
    else_clause: ($, prev) =>
      seq("else", choice($.if_expression, $.statement_block)),

    // Adds `throw` to the list of unary expressions
    unary_expression: ($) =>
      prec.left(
        "unary_void",
        seq(
          field(
            "operator",
            choice("!", "~", "-", "+", "typeof", "void", "delete", "throw")
          ),
          field("argument", $.expression)
        )
      ),

    //
    // New expressions
    //

    do_expression: ($) => seq("do", $.statement_block),

    let_expression: ($) =>
      seq("let", field("name", $.refutable_pattern), $._initializer),

    match_expression: ($) =>
      seq(
        "match",
        "(",
        field("expression", $.expression),
        ")",
        field("arms", $.match_arms)
      ),

    match_arms: ($) => seq("{", commaSep($.match_arm), "}"),

    match_arm: ($) =>
      seq(
        field("pattern", $.refutable_pattern),
        optional(
          seq(
            "if",
            "(",
            field("condition", choice($.let_expression, $.expression)),
            ")"
          )
        ),
        // NOTE: we use "->" instead of "=>" to help indicate that there isn't
        // a new stack frame when stepping through match expressions like there
        // is with an arrow expression.
        "->",
        field("value", choice($.expression, $.statement_block))
      ),

    //
    // Refutable patterns
    //

    refutable_pattern: ($) =>
      choice(
        // literals
        $.number,
        $.string,
        $.true,
        $.false,
        // TOOD: support regex literals

        $._identifier, // identifiers + undefined

        $.refutable_array_pattern,
        $.refutable_is_pattern,
        $.refutable_object_pattern
      ),

    refutable_array_pattern: ($) =>
      seq(
        "[",
        commaSep(choice($.refutable_pattern, $.refutable_rest_pattern)),
        "]"
      ),

    refutable_is_pattern: ($) => seq($.identifier, "is", $.identifier),

    refutable_object_pattern: ($) =>
      prec(
        "object",
        seq(
          "{",
          commaSep(
            optional(
              choice(
                $.refutable_pair_pattern,
                $.refutable_rest_pattern,
                alias(
                  choice($.identifier, $._reserved_identifier),
                  $.shorthand_property_identifier_pattern
                )
              )
            )
          ),
          "}"
        )
      ),

    refutable_pair_pattern: ($) =>
      seq(
        field("key", $._property_name),
        ":",
        field("value", $.refutable_pattern)
      ),

    refutable_rest_pattern: ($) =>
      seq(
        "...",
        choice(
          $.identifier,
          $.refutable_array_pattern,
          $.refutable_object_pattern
        )
      ),
  },
});

function commaSep1(rule) {
  return seq(rule, repeat(seq(",", rule)));
}

function commaSep(rule) {
  return optional(commaSep1(rule));
}
