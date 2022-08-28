module.exports = grammar({
    name: "crochet",
    extras: ($) => [/\s/, $.comment],
    conflicts: ($) => [],
    rules: {
        program: ($) => $.stmt,
        stmt: ($) => choice($.skip, $.asgn, $.seq, $.if, $.while),
        skip: ($) => "skip",
        asgn: ($) => seq(field("name", $.id), ":=", $._aexp),
        id: ($) => /[a-z]+/,
        seq: ($) => prec.right(1, seq($.stmt, ";", $.stmt, optional(";"))),
        if: ($) =>
            seq(
                "if",
                field("condition", $._bexp),
                "then",
                field("consequent", $.stmt),
                "else",
                field("alternative", $.stmt),
                "end"
            ),
        while: ($) =>
            seq(
                "while",
                field("condition", $._bexp),
                "do",
                field("body", $.stmt),
                "end"
            ),
        _aexp: ($) =>
            choice(
                $.num,
                $.id,
                $.plus,
                $.minus,
                $.times,
                seq("(", $._aexp, ")")
            ),
        plus: ($) => prec.left(1, seq($._aexp, "+", $._aexp)),
        minus: ($) => prec.left(1, seq($._aexp, "-", $._aexp)),
        times: ($) => prec.left(2, seq($._aexp, "*", $._aexp)),
        _bexp: ($) =>
            choice(
                "true",
                "false",
                $.eqb,
                $.leb,
                $.negb,
                $.andb,
                seq("(", $._bexp, ")")
            ),
        eqb: ($) => seq($._aexp, "=", $._aexp),
        leb: ($) => seq($._aexp, "=<", $._aexp),
        negb: ($) => prec.right(2, seq("~", $._bexp)),
        andb: ($) => prec.right(1, seq($._bexp, "&&", $._bexp)),
        num: ($) => /[0-9]+/,
        comment: ($) => token(seq("//", /.*/)),
    },
});
