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

  // Always pick $.binding_identifer last when there's a conflict
  conflicts: ($, previous) =>
    previous.concat([
      [$.assignment_expression, $.binding_identifier],

      [$.primary_expression, $.binding_identifier],
      [$._primary_type, $.binding_identifier],
      [$.primary_expression, $._primary_type, $.binding_identifier],

      [$.object, $.binding_identifier],
      [$.object, $._property_name, $.binding_identifier],
      [$._property_name, $.binding_identifier],

      [$.literal_type, $.binding_identifier],
      [$.predefined_type, $.binding_identifier],
      [$.primary_expression, $.literal_type, $.binding_identifier],
      [$.primary_expression, $.predefined_type, $.binding_identifier],

      [$.export_statement, $.binding_identifier],
    ]),

  rules: {
    // Removes sequence expression and optional flow-style type assertion
    parenthesized_expression: ($, prev) => seq("(", $.expression, ")"),

    // Removes sequence expression
    _expressions: ($, prev) => $.expression,

    // Removes automatic semicolon insertion
    _semicolon: ($, prev) => ";",

    // Replaces 'readonly' with 'mut'
    _type: ($, prev) => {
      const choices = prev.members.map((member) => {
        return member.name == "readonly_type"
          ? alias($.readonly_type, $.mutable_type)
          : member;
      });
      return choice(...choices);
    },
    readonly_type: ($, prev) => seq("mut", $._type),

    // Replaces `optional("readonly")` in sequence with `optional("mut")`
    property_signature: ($, prev) => {
      const members = prev.members.map((member) => {
        if (
          member.type === "CHOICE" &&
          member.members[0].value === "readonly"
        ) {
          return optional("mut");
        }
        return member;
      });

      return seq(...members);
    },

    // Replaces `optional("readonly")` in sequence with `optional("mut")`
    method_signature: ($, prev) => {
      const members = prev.members.map((member) => {
        if (
          member.type === "CHOICE" &&
          member.members[0].value === "readonly"
        ) {
          return optional("mut");
        }
        return member;
      });

      return seq(...members);
    },

    // Replaces `optional("readonly")` in sequence with `optional("mut")`
    // TODO: Do the same for public_field_definition
    // TODO: Create a function that can replace all nodes of a certain type
    // in a rule declaration.
    method_definition: ($, prev) => {
      const members = prev.content.members.map((member) => {
        if (
          member.type === "CHOICE" &&
          member.members[0].value === "readonly"
        ) {
          return optional("mut");
        }
        return member;
      });

      return prec.left(seq(...members));
    },

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

    binding_identifier: ($) =>
      seq(
        optional(field("mut", "mut")),
        field(
          "name",
          // NOTE: I thought about disallowing reserved identifiers here, but there
          // are too many reserved identifiers and some of them are just too handy,
          // e.g. let set = new Set();
          choice($._identifier, alias($._reserved_identifier, $.identifier))
        )
      ),

    // Adapted from define-grammar.js
    // TODO: Create a function that can replace all nodes of a certain type
    // in a rule declaration.
    variable_declarator: ($) =>
      choice(
        seq(
          // field("name", choice($.identifier, $._destructuring_pattern)),
          field(
            "name",
            choice($.binding_identifier, $._binding_destructuring_pattern)
          ),
          field("type", optional($.type_annotation)),
          optional($._initializer)
        ),
        prec(
          "declaration",
          seq(
            // field("name", $.identifier),
            field("name", $.binding_identifier),
            "!",
            field("type", $.type_annotation)
          )
        )
      ),

    // TODO: Create a function that can remove all nodes of a certain type
    // in a rule declaration.
    for_statement: ($) =>
      seq(
        "for",
        "(",
        field(
          "initializer",
          choice(
            $.lexical_declaration,
            // $.variable_declaration,
            $.expression_statement,
            $.empty_statement
          )
        ),
        field("condition", choice($.expression_statement, $.empty_statement)),
        field("increment", optional($._expressions)),
        ")",
        field("body", $.statement)
      ),

    // TODO: Create a function that can remove all nodes of a certain type
    // in a rule declaration.
    declaration: ($, previous) => {
      const choices = previous.members.filter(
        (choice) => choice.name != "variable_declaration"
      );
      return choice(...choices);
    },

    // NOTE: we can't use prev.members.filter here b/c define-grammar.js does
    // ($, previous) => choice(previous, ...).
    // TODO: write a function to flatten choices
    _lhs_expression: ($) =>
      choice(
        $.member_expression,
        $.subscript_expression,
        $._identifier,
        alias($._reserved_identifier, $.identifier),
        // We disallow destructing in assignment expressions since it isn't used
        // very often and the fact that `let`-binding can shadow variables reduces
        // the need for it even further.
        // $._destructuring_pattern,
        $.non_null_expression
      ),

    _for_header: ($) =>
      seq(
        "(",
        choice(
          field("left", choice($._lhs_expression, $.parenthesized_expression)),
          // We remove this choice b/c we don't want to support `var`
          // seq(
          //   field("kind", "var"),
          //   field("left", choice($.identifier, $._destructuring_pattern)),
          //   optional($._initializer)
          // ),
          seq(
            // TODO: get rid of `const`
            field("kind", choice("let", "const")),
            field(
              "left",
              choice($.binding_identifier, $._binding_destructuring_pattern)
            )
          )
        ),
        field("operator", choice("in", "of")),
        field("right", $._expressions),
        ")"
      ),

    statement_block: ($, prev) =>
      seq(
        "{",
        repeat($.statement),
        // We define an alias here so that it's easy to check if the
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
    switch_case: ($, prev) => replaceField(prev, "body", $.statement_block), // replaces repeat($.statement)
    switch_default: ($, prev) => replaceField(prev, "body", $.statement_block), // replaces repeat($.statement)

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

    // Adapted from define-grammar.js
    // TODO: Create a function that can replace all nodes of a certain type
    // in a rule declaration.
    catch_clause: ($) =>
      seq(
        "catch",
        optional(
          seq(
            "(",
            field(
              "parameter",
              choice($.binding_identifier, $._binding_destructuring_pattern)
            ),
            optional(
              // only types that resolve to 'any' or 'unknown' are supported
              // by the language but it's simpler to accept any type here.
              field("type", $.type_annotation)
            ),
            ")"
          )
        ),
        field("body", $.statement_block)
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
        field("expression", choice($.expression, $.statement_block)),
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

        $.binding_identifier, // identifiers + undefined

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

    refutable_is_pattern: ($) => seq($.binding_identifier, "is", $.identifier),

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
                  $.binding_identifier,
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
          $.binding_identifier,
          $.refutable_array_pattern,
          $.refutable_object_pattern
        )
      ),

    //
    // Patterns
    //
    // We reimplement $.pattern and its dependencies here so that:
    // - bindings that are introduced can now have an optional `mut` modifier, e.g.
    //   let {x, mut y} = point;
    // - LHS expressions are no longer allowed valid patterns
    ///

    pattern: ($) =>
      prec.dynamic(
        -1,
        choice(
          $.binding_identifier,
          $.array_pattern,
          $.object_pattern,
          $.rest_pattern
        )
      ),

    array_pattern: ($) =>
      seq("[", commaSep(choice($.pattern, $.assignment_pattern)), "]"),

    assignment_pattern: ($) =>
      seq(field("left", $.pattern), "=", field("right", $.expression)),

    object_pattern: ($) =>
      prec(
        "object",
        seq(
          "{",
          commaSep(
            optional(
              choice(
                $.pair_pattern,
                $.rest_pattern,
                alias(
                  $.binding_identifier,
                  $.shorthand_property_identifier_pattern
                ),
                $.object_assignment_pattern
              )
            )
          ),
          "}"
        )
      ),

    object_assignment_pattern: ($) =>
      seq(
        field(
          "left",
          choice(
            $.pair_pattern,
            alias($.binding_identifier, $.shorthand_property_identifier_pattern)
          )
        ),
        "=",
        field("right", $.expression)
      ),

    pair_pattern: ($) =>
      seq(field("key", $._property_name), ":", field("value", $.pattern)),

    rest_pattern: ($) =>
      prec.right(
        seq(
          "...",
          choice($.binding_identifier, $.array_pattern, $.object_pattern)
        )
      ),

    // We use this instead of _destructuring_pattern in places where new bindings
    // will be introduced b/c of the destructuring.
    _binding_destructuring_pattern: ($) =>
      choice($.object_pattern, $.array_pattern),

    _parameter_name: ($) =>
      seq(
        repeat(field("decorator", $.decorator)),
        optional($.accessibility_modifier),
        optional($.override_modifier),
        optional("readonly"),
        field("pattern", choice($.pattern, $.this))
      ),
  },
});

function commaSep1(rule) {
  return seq(rule, repeat(seq(",", rule)));
}

function commaSep(rule) {
  return optional(commaSep1(rule));
}
