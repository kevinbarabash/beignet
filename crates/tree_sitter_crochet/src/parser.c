#include <tree_sitter/parser.h>

#if defined(__GNUC__) || defined(__clang__)
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"
#endif

#define LANGUAGE_VERSION 13
#define STATE_COUNT 51
#define LARGE_STATE_COUNT 2
#define SYMBOL_COUNT 39
#define ALIAS_COUNT 0
#define TOKEN_COUNT 24
#define EXTERNAL_TOKEN_COUNT 0
#define FIELD_COUNT 5
#define MAX_ALIAS_SEQUENCE_LENGTH 7
#define PRODUCTION_ID_COUNT 4

enum {
  sym_skip = 1,
  anon_sym_COLON_EQ = 2,
  sym_id = 3,
  anon_sym_SEMI = 4,
  anon_sym_if = 5,
  anon_sym_then = 6,
  anon_sym_else = 7,
  anon_sym_end = 8,
  anon_sym_while = 9,
  anon_sym_do = 10,
  anon_sym_LPAREN = 11,
  anon_sym_RPAREN = 12,
  anon_sym_PLUS = 13,
  anon_sym_DASH = 14,
  anon_sym_STAR = 15,
  anon_sym_true = 16,
  anon_sym_false = 17,
  anon_sym_EQ = 18,
  anon_sym_EQ_LT = 19,
  anon_sym_TILDE = 20,
  anon_sym_AMP_AMP = 21,
  sym_num = 22,
  sym_comment = 23,
  sym_program = 24,
  sym_stmt = 25,
  sym_asgn = 26,
  sym_seq = 27,
  sym_if = 28,
  sym_while = 29,
  sym__aexp = 30,
  sym_plus = 31,
  sym_minus = 32,
  sym_times = 33,
  sym__bexp = 34,
  sym_eqb = 35,
  sym_leb = 36,
  sym_negb = 37,
  sym_andb = 38,
};

static const char * const ts_symbol_names[] = {
  [ts_builtin_sym_end] = "end",
  [sym_skip] = "skip",
  [anon_sym_COLON_EQ] = ":=",
  [sym_id] = "id",
  [anon_sym_SEMI] = ";",
  [anon_sym_if] = "if",
  [anon_sym_then] = "then",
  [anon_sym_else] = "else",
  [anon_sym_end] = "end",
  [anon_sym_while] = "while",
  [anon_sym_do] = "do",
  [anon_sym_LPAREN] = "(",
  [anon_sym_RPAREN] = ")",
  [anon_sym_PLUS] = "+",
  [anon_sym_DASH] = "-",
  [anon_sym_STAR] = "*",
  [anon_sym_true] = "true",
  [anon_sym_false] = "false",
  [anon_sym_EQ] = "=",
  [anon_sym_EQ_LT] = "=<",
  [anon_sym_TILDE] = "~",
  [anon_sym_AMP_AMP] = "&&",
  [sym_num] = "num",
  [sym_comment] = "comment",
  [sym_program] = "program",
  [sym_stmt] = "stmt",
  [sym_asgn] = "asgn",
  [sym_seq] = "seq",
  [sym_if] = "if",
  [sym_while] = "while",
  [sym__aexp] = "_aexp",
  [sym_plus] = "plus",
  [sym_minus] = "minus",
  [sym_times] = "times",
  [sym__bexp] = "_bexp",
  [sym_eqb] = "eqb",
  [sym_leb] = "leb",
  [sym_negb] = "negb",
  [sym_andb] = "andb",
};

static const TSSymbol ts_symbol_map[] = {
  [ts_builtin_sym_end] = ts_builtin_sym_end,
  [sym_skip] = sym_skip,
  [anon_sym_COLON_EQ] = anon_sym_COLON_EQ,
  [sym_id] = sym_id,
  [anon_sym_SEMI] = anon_sym_SEMI,
  [anon_sym_if] = anon_sym_if,
  [anon_sym_then] = anon_sym_then,
  [anon_sym_else] = anon_sym_else,
  [anon_sym_end] = anon_sym_end,
  [anon_sym_while] = anon_sym_while,
  [anon_sym_do] = anon_sym_do,
  [anon_sym_LPAREN] = anon_sym_LPAREN,
  [anon_sym_RPAREN] = anon_sym_RPAREN,
  [anon_sym_PLUS] = anon_sym_PLUS,
  [anon_sym_DASH] = anon_sym_DASH,
  [anon_sym_STAR] = anon_sym_STAR,
  [anon_sym_true] = anon_sym_true,
  [anon_sym_false] = anon_sym_false,
  [anon_sym_EQ] = anon_sym_EQ,
  [anon_sym_EQ_LT] = anon_sym_EQ_LT,
  [anon_sym_TILDE] = anon_sym_TILDE,
  [anon_sym_AMP_AMP] = anon_sym_AMP_AMP,
  [sym_num] = sym_num,
  [sym_comment] = sym_comment,
  [sym_program] = sym_program,
  [sym_stmt] = sym_stmt,
  [sym_asgn] = sym_asgn,
  [sym_seq] = sym_seq,
  [sym_if] = sym_if,
  [sym_while] = sym_while,
  [sym__aexp] = sym__aexp,
  [sym_plus] = sym_plus,
  [sym_minus] = sym_minus,
  [sym_times] = sym_times,
  [sym__bexp] = sym__bexp,
  [sym_eqb] = sym_eqb,
  [sym_leb] = sym_leb,
  [sym_negb] = sym_negb,
  [sym_andb] = sym_andb,
};

static const TSSymbolMetadata ts_symbol_metadata[] = {
  [ts_builtin_sym_end] = {
    .visible = false,
    .named = true,
  },
  [sym_skip] = {
    .visible = true,
    .named = true,
  },
  [anon_sym_COLON_EQ] = {
    .visible = true,
    .named = false,
  },
  [sym_id] = {
    .visible = true,
    .named = true,
  },
  [anon_sym_SEMI] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_if] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_then] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_else] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_end] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_while] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_do] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_LPAREN] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_RPAREN] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_PLUS] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_DASH] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_STAR] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_true] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_false] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_EQ] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_EQ_LT] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_TILDE] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_AMP_AMP] = {
    .visible = true,
    .named = false,
  },
  [sym_num] = {
    .visible = true,
    .named = true,
  },
  [sym_comment] = {
    .visible = true,
    .named = true,
  },
  [sym_program] = {
    .visible = true,
    .named = true,
  },
  [sym_stmt] = {
    .visible = true,
    .named = true,
  },
  [sym_asgn] = {
    .visible = true,
    .named = true,
  },
  [sym_seq] = {
    .visible = true,
    .named = true,
  },
  [sym_if] = {
    .visible = true,
    .named = true,
  },
  [sym_while] = {
    .visible = true,
    .named = true,
  },
  [sym__aexp] = {
    .visible = false,
    .named = true,
  },
  [sym_plus] = {
    .visible = true,
    .named = true,
  },
  [sym_minus] = {
    .visible = true,
    .named = true,
  },
  [sym_times] = {
    .visible = true,
    .named = true,
  },
  [sym__bexp] = {
    .visible = false,
    .named = true,
  },
  [sym_eqb] = {
    .visible = true,
    .named = true,
  },
  [sym_leb] = {
    .visible = true,
    .named = true,
  },
  [sym_negb] = {
    .visible = true,
    .named = true,
  },
  [sym_andb] = {
    .visible = true,
    .named = true,
  },
};

enum {
  field_alternative = 1,
  field_body = 2,
  field_condition = 3,
  field_consequent = 4,
  field_name = 5,
};

static const char * const ts_field_names[] = {
  [0] = NULL,
  [field_alternative] = "alternative",
  [field_body] = "body",
  [field_condition] = "condition",
  [field_consequent] = "consequent",
  [field_name] = "name",
};

static const TSFieldMapSlice ts_field_map_slices[PRODUCTION_ID_COUNT] = {
  [1] = {.index = 0, .length = 1},
  [2] = {.index = 1, .length = 2},
  [3] = {.index = 3, .length = 3},
};

static const TSFieldMapEntry ts_field_map_entries[] = {
  [0] =
    {field_name, 0},
  [1] =
    {field_body, 3},
    {field_condition, 1},
  [3] =
    {field_alternative, 5},
    {field_condition, 1},
    {field_consequent, 3},
};

static const TSSymbol ts_alias_sequences[PRODUCTION_ID_COUNT][MAX_ALIAS_SEQUENCE_LENGTH] = {
  [0] = {0},
};

static const uint16_t ts_non_terminal_alias_map[] = {
  0,
};

static bool ts_lex(TSLexer *lexer, TSStateId state) {
  START_LEXER();
  eof = lexer->eof(lexer);
  switch (state) {
    case 0:
      if (eof) ADVANCE(31);
      if (lookahead == '&') ADVANCE(1);
      if (lookahead == '(') ADVANCE(67);
      if (lookahead == ')') ADVANCE(68);
      if (lookahead == '*') ADVANCE(71);
      if (lookahead == '+') ADVANCE(69);
      if (lookahead == '-') ADVANCE(70);
      if (lookahead == '/') ADVANCE(6);
      if (lookahead == ':') ADVANCE(7);
      if (lookahead == ';') ADVANCE(56);
      if (lookahead == '=') ADVANCE(76);
      if (lookahead == 'd') ADVANCE(25);
      if (lookahead == 'e') ADVANCE(21);
      if (lookahead == 'f') ADVANCE(8);
      if (lookahead == 'i') ADVANCE(15);
      if (lookahead == 's') ADVANCE(20);
      if (lookahead == 't') ADVANCE(16);
      if (lookahead == 'w') ADVANCE(17);
      if (lookahead == '~') ADVANCE(78);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(0)
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(80);
      END_STATE();
    case 1:
      if (lookahead == '&') ADVANCE(79);
      END_STATE();
    case 2:
      if (lookahead == '(') ADVANCE(67);
      if (lookahead == '/') ADVANCE(6);
      if (lookahead == 'f') ADVANCE(35);
      if (lookahead == 't') ADVANCE(51);
      if (lookahead == '~') ADVANCE(78);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(2)
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(80);
      if (('a' <= lookahead && lookahead <= 'z')) ADVANCE(55);
      END_STATE();
    case 3:
      if (lookahead == '(') ADVANCE(67);
      if (lookahead == '/') ADVANCE(6);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(3)
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(80);
      if (('a' <= lookahead && lookahead <= 'z')) ADVANCE(55);
      END_STATE();
    case 4:
      if (lookahead == '/') ADVANCE(6);
      if (lookahead == ';') ADVANCE(56);
      if (lookahead == 'e') ADVANCE(49);
      if (lookahead == 'i') ADVANCE(41);
      if (lookahead == 's') ADVANCE(45);
      if (lookahead == 'w') ADVANCE(42);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(4)
      if (('a' <= lookahead && lookahead <= 'z')) ADVANCE(55);
      END_STATE();
    case 5:
      if (lookahead == '/') ADVANCE(6);
      if (lookahead == ';') ADVANCE(56);
      if (lookahead == 'e') ADVANCE(48);
      if (lookahead == 'i') ADVANCE(41);
      if (lookahead == 's') ADVANCE(45);
      if (lookahead == 'w') ADVANCE(42);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(5)
      if (('a' <= lookahead && lookahead <= 'z')) ADVANCE(55);
      END_STATE();
    case 6:
      if (lookahead == '/') ADVANCE(81);
      END_STATE();
    case 7:
      if (lookahead == '=') ADVANCE(34);
      END_STATE();
    case 8:
      if (lookahead == 'a') ADVANCE(23);
      END_STATE();
    case 9:
      if (lookahead == 'd') ADVANCE(62);
      END_STATE();
    case 10:
      if (lookahead == 'e') ADVANCE(24);
      END_STATE();
    case 11:
      if (lookahead == 'e') ADVANCE(60);
      END_STATE();
    case 12:
      if (lookahead == 'e') ADVANCE(72);
      END_STATE();
    case 13:
      if (lookahead == 'e') ADVANCE(74);
      END_STATE();
    case 14:
      if (lookahead == 'e') ADVANCE(64);
      END_STATE();
    case 15:
      if (lookahead == 'f') ADVANCE(57);
      END_STATE();
    case 16:
      if (lookahead == 'h') ADVANCE(10);
      if (lookahead == 'r') ADVANCE(29);
      END_STATE();
    case 17:
      if (lookahead == 'h') ADVANCE(19);
      END_STATE();
    case 18:
      if (lookahead == 'i') ADVANCE(26);
      END_STATE();
    case 19:
      if (lookahead == 'i') ADVANCE(22);
      END_STATE();
    case 20:
      if (lookahead == 'k') ADVANCE(18);
      END_STATE();
    case 21:
      if (lookahead == 'l') ADVANCE(27);
      if (lookahead == 'n') ADVANCE(9);
      END_STATE();
    case 22:
      if (lookahead == 'l') ADVANCE(14);
      END_STATE();
    case 23:
      if (lookahead == 'l') ADVANCE(28);
      END_STATE();
    case 24:
      if (lookahead == 'n') ADVANCE(59);
      END_STATE();
    case 25:
      if (lookahead == 'o') ADVANCE(66);
      END_STATE();
    case 26:
      if (lookahead == 'p') ADVANCE(32);
      END_STATE();
    case 27:
      if (lookahead == 's') ADVANCE(11);
      END_STATE();
    case 28:
      if (lookahead == 's') ADVANCE(13);
      END_STATE();
    case 29:
      if (lookahead == 'u') ADVANCE(12);
      END_STATE();
    case 30:
      if (eof) ADVANCE(31);
      if (lookahead == '/') ADVANCE(6);
      if (lookahead == ';') ADVANCE(56);
      if (lookahead == 'i') ADVANCE(41);
      if (lookahead == 's') ADVANCE(45);
      if (lookahead == 'w') ADVANCE(42);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(30)
      if (('a' <= lookahead && lookahead <= 'z')) ADVANCE(55);
      END_STATE();
    case 31:
      ACCEPT_TOKEN(ts_builtin_sym_end);
      END_STATE();
    case 32:
      ACCEPT_TOKEN(sym_skip);
      END_STATE();
    case 33:
      ACCEPT_TOKEN(sym_skip);
      if (('a' <= lookahead && lookahead <= 'z')) ADVANCE(55);
      END_STATE();
    case 34:
      ACCEPT_TOKEN(anon_sym_COLON_EQ);
      END_STATE();
    case 35:
      ACCEPT_TOKEN(sym_id);
      if (lookahead == 'a') ADVANCE(47);
      if (('b' <= lookahead && lookahead <= 'z')) ADVANCE(55);
      END_STATE();
    case 36:
      ACCEPT_TOKEN(sym_id);
      if (lookahead == 'd') ADVANCE(63);
      if (('a' <= lookahead && lookahead <= 'z')) ADVANCE(55);
      END_STATE();
    case 37:
      ACCEPT_TOKEN(sym_id);
      if (lookahead == 'e') ADVANCE(65);
      if (('a' <= lookahead && lookahead <= 'z')) ADVANCE(55);
      END_STATE();
    case 38:
      ACCEPT_TOKEN(sym_id);
      if (lookahead == 'e') ADVANCE(73);
      if (('a' <= lookahead && lookahead <= 'z')) ADVANCE(55);
      END_STATE();
    case 39:
      ACCEPT_TOKEN(sym_id);
      if (lookahead == 'e') ADVANCE(75);
      if (('a' <= lookahead && lookahead <= 'z')) ADVANCE(55);
      END_STATE();
    case 40:
      ACCEPT_TOKEN(sym_id);
      if (lookahead == 'e') ADVANCE(61);
      if (('a' <= lookahead && lookahead <= 'z')) ADVANCE(55);
      END_STATE();
    case 41:
      ACCEPT_TOKEN(sym_id);
      if (lookahead == 'f') ADVANCE(58);
      if (('a' <= lookahead && lookahead <= 'z')) ADVANCE(55);
      END_STATE();
    case 42:
      ACCEPT_TOKEN(sym_id);
      if (lookahead == 'h') ADVANCE(44);
      if (('a' <= lookahead && lookahead <= 'z')) ADVANCE(55);
      END_STATE();
    case 43:
      ACCEPT_TOKEN(sym_id);
      if (lookahead == 'i') ADVANCE(50);
      if (('a' <= lookahead && lookahead <= 'z')) ADVANCE(55);
      END_STATE();
    case 44:
      ACCEPT_TOKEN(sym_id);
      if (lookahead == 'i') ADVANCE(46);
      if (('a' <= lookahead && lookahead <= 'z')) ADVANCE(55);
      END_STATE();
    case 45:
      ACCEPT_TOKEN(sym_id);
      if (lookahead == 'k') ADVANCE(43);
      if (('a' <= lookahead && lookahead <= 'z')) ADVANCE(55);
      END_STATE();
    case 46:
      ACCEPT_TOKEN(sym_id);
      if (lookahead == 'l') ADVANCE(37);
      if (('a' <= lookahead && lookahead <= 'z')) ADVANCE(55);
      END_STATE();
    case 47:
      ACCEPT_TOKEN(sym_id);
      if (lookahead == 'l') ADVANCE(52);
      if (('a' <= lookahead && lookahead <= 'z')) ADVANCE(55);
      END_STATE();
    case 48:
      ACCEPT_TOKEN(sym_id);
      if (lookahead == 'l') ADVANCE(53);
      if (('a' <= lookahead && lookahead <= 'z')) ADVANCE(55);
      END_STATE();
    case 49:
      ACCEPT_TOKEN(sym_id);
      if (lookahead == 'n') ADVANCE(36);
      if (('a' <= lookahead && lookahead <= 'z')) ADVANCE(55);
      END_STATE();
    case 50:
      ACCEPT_TOKEN(sym_id);
      if (lookahead == 'p') ADVANCE(33);
      if (('a' <= lookahead && lookahead <= 'z')) ADVANCE(55);
      END_STATE();
    case 51:
      ACCEPT_TOKEN(sym_id);
      if (lookahead == 'r') ADVANCE(54);
      if (('a' <= lookahead && lookahead <= 'z')) ADVANCE(55);
      END_STATE();
    case 52:
      ACCEPT_TOKEN(sym_id);
      if (lookahead == 's') ADVANCE(39);
      if (('a' <= lookahead && lookahead <= 'z')) ADVANCE(55);
      END_STATE();
    case 53:
      ACCEPT_TOKEN(sym_id);
      if (lookahead == 's') ADVANCE(40);
      if (('a' <= lookahead && lookahead <= 'z')) ADVANCE(55);
      END_STATE();
    case 54:
      ACCEPT_TOKEN(sym_id);
      if (lookahead == 'u') ADVANCE(38);
      if (('a' <= lookahead && lookahead <= 'z')) ADVANCE(55);
      END_STATE();
    case 55:
      ACCEPT_TOKEN(sym_id);
      if (('a' <= lookahead && lookahead <= 'z')) ADVANCE(55);
      END_STATE();
    case 56:
      ACCEPT_TOKEN(anon_sym_SEMI);
      END_STATE();
    case 57:
      ACCEPT_TOKEN(anon_sym_if);
      END_STATE();
    case 58:
      ACCEPT_TOKEN(anon_sym_if);
      if (('a' <= lookahead && lookahead <= 'z')) ADVANCE(55);
      END_STATE();
    case 59:
      ACCEPT_TOKEN(anon_sym_then);
      END_STATE();
    case 60:
      ACCEPT_TOKEN(anon_sym_else);
      END_STATE();
    case 61:
      ACCEPT_TOKEN(anon_sym_else);
      if (('a' <= lookahead && lookahead <= 'z')) ADVANCE(55);
      END_STATE();
    case 62:
      ACCEPT_TOKEN(anon_sym_end);
      END_STATE();
    case 63:
      ACCEPT_TOKEN(anon_sym_end);
      if (('a' <= lookahead && lookahead <= 'z')) ADVANCE(55);
      END_STATE();
    case 64:
      ACCEPT_TOKEN(anon_sym_while);
      END_STATE();
    case 65:
      ACCEPT_TOKEN(anon_sym_while);
      if (('a' <= lookahead && lookahead <= 'z')) ADVANCE(55);
      END_STATE();
    case 66:
      ACCEPT_TOKEN(anon_sym_do);
      END_STATE();
    case 67:
      ACCEPT_TOKEN(anon_sym_LPAREN);
      END_STATE();
    case 68:
      ACCEPT_TOKEN(anon_sym_RPAREN);
      END_STATE();
    case 69:
      ACCEPT_TOKEN(anon_sym_PLUS);
      END_STATE();
    case 70:
      ACCEPT_TOKEN(anon_sym_DASH);
      END_STATE();
    case 71:
      ACCEPT_TOKEN(anon_sym_STAR);
      END_STATE();
    case 72:
      ACCEPT_TOKEN(anon_sym_true);
      END_STATE();
    case 73:
      ACCEPT_TOKEN(anon_sym_true);
      if (('a' <= lookahead && lookahead <= 'z')) ADVANCE(55);
      END_STATE();
    case 74:
      ACCEPT_TOKEN(anon_sym_false);
      END_STATE();
    case 75:
      ACCEPT_TOKEN(anon_sym_false);
      if (('a' <= lookahead && lookahead <= 'z')) ADVANCE(55);
      END_STATE();
    case 76:
      ACCEPT_TOKEN(anon_sym_EQ);
      if (lookahead == '<') ADVANCE(77);
      END_STATE();
    case 77:
      ACCEPT_TOKEN(anon_sym_EQ_LT);
      END_STATE();
    case 78:
      ACCEPT_TOKEN(anon_sym_TILDE);
      END_STATE();
    case 79:
      ACCEPT_TOKEN(anon_sym_AMP_AMP);
      END_STATE();
    case 80:
      ACCEPT_TOKEN(sym_num);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(80);
      END_STATE();
    case 81:
      ACCEPT_TOKEN(sym_comment);
      if (lookahead != 0 &&
          lookahead != '\n') ADVANCE(81);
      END_STATE();
    default:
      return false;
  }
}

static const TSLexMode ts_lex_modes[STATE_COUNT] = {
  [0] = {.lex_state = 0},
  [1] = {.lex_state = 30},
  [2] = {.lex_state = 2},
  [3] = {.lex_state = 2},
  [4] = {.lex_state = 2},
  [5] = {.lex_state = 2},
  [6] = {.lex_state = 2},
  [7] = {.lex_state = 0},
  [8] = {.lex_state = 0},
  [9] = {.lex_state = 0},
  [10] = {.lex_state = 0},
  [11] = {.lex_state = 4},
  [12] = {.lex_state = 5},
  [13] = {.lex_state = 30},
  [14] = {.lex_state = 30},
  [15] = {.lex_state = 30},
  [16] = {.lex_state = 30},
  [17] = {.lex_state = 30},
  [18] = {.lex_state = 30},
  [19] = {.lex_state = 30},
  [20] = {.lex_state = 0},
  [21] = {.lex_state = 3},
  [22] = {.lex_state = 3},
  [23] = {.lex_state = 3},
  [24] = {.lex_state = 3},
  [25] = {.lex_state = 3},
  [26] = {.lex_state = 3},
  [27] = {.lex_state = 0},
  [28] = {.lex_state = 3},
  [29] = {.lex_state = 0},
  [30] = {.lex_state = 0},
  [31] = {.lex_state = 0},
  [32] = {.lex_state = 0},
  [33] = {.lex_state = 0},
  [34] = {.lex_state = 0},
  [35] = {.lex_state = 0},
  [36] = {.lex_state = 0},
  [37] = {.lex_state = 0},
  [38] = {.lex_state = 0},
  [39] = {.lex_state = 0},
  [40] = {.lex_state = 0},
  [41] = {.lex_state = 0},
  [42] = {.lex_state = 0},
  [43] = {.lex_state = 0},
  [44] = {.lex_state = 0},
  [45] = {.lex_state = 0},
  [46] = {.lex_state = 0},
  [47] = {.lex_state = 0},
  [48] = {.lex_state = 0},
  [49] = {.lex_state = 0},
  [50] = {.lex_state = 0},
};

static const uint16_t ts_parse_table[LARGE_STATE_COUNT][SYMBOL_COUNT] = {
  [0] = {
    [ts_builtin_sym_end] = ACTIONS(1),
    [sym_skip] = ACTIONS(1),
    [anon_sym_COLON_EQ] = ACTIONS(1),
    [anon_sym_SEMI] = ACTIONS(1),
    [anon_sym_if] = ACTIONS(1),
    [anon_sym_then] = ACTIONS(1),
    [anon_sym_else] = ACTIONS(1),
    [anon_sym_end] = ACTIONS(1),
    [anon_sym_while] = ACTIONS(1),
    [anon_sym_do] = ACTIONS(1),
    [anon_sym_LPAREN] = ACTIONS(1),
    [anon_sym_RPAREN] = ACTIONS(1),
    [anon_sym_PLUS] = ACTIONS(1),
    [anon_sym_DASH] = ACTIONS(1),
    [anon_sym_STAR] = ACTIONS(1),
    [anon_sym_true] = ACTIONS(1),
    [anon_sym_false] = ACTIONS(1),
    [anon_sym_EQ] = ACTIONS(1),
    [anon_sym_EQ_LT] = ACTIONS(1),
    [anon_sym_TILDE] = ACTIONS(1),
    [anon_sym_AMP_AMP] = ACTIONS(1),
    [sym_num] = ACTIONS(1),
    [sym_comment] = ACTIONS(3),
  },
  [1] = {
    [sym_program] = STATE(49),
    [sym_stmt] = STATE(46),
    [sym_asgn] = STATE(36),
    [sym_seq] = STATE(36),
    [sym_if] = STATE(36),
    [sym_while] = STATE(36),
    [sym_skip] = ACTIONS(5),
    [sym_id] = ACTIONS(7),
    [anon_sym_if] = ACTIONS(9),
    [anon_sym_while] = ACTIONS(11),
    [sym_comment] = ACTIONS(3),
  },
};

static const uint16_t ts_small_parse_table[] = {
  [0] = 8,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(13), 1,
      sym_id,
    ACTIONS(15), 1,
      anon_sym_LPAREN,
    ACTIONS(19), 1,
      anon_sym_TILDE,
    ACTIONS(21), 1,
      sym_num,
    ACTIONS(17), 2,
      anon_sym_true,
      anon_sym_false,
    STATE(31), 4,
      sym__aexp,
      sym_plus,
      sym_minus,
      sym_times,
    STATE(43), 5,
      sym__bexp,
      sym_eqb,
      sym_leb,
      sym_negb,
      sym_andb,
  [33] = 8,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(13), 1,
      sym_id,
    ACTIONS(15), 1,
      anon_sym_LPAREN,
    ACTIONS(19), 1,
      anon_sym_TILDE,
    ACTIONS(21), 1,
      sym_num,
    ACTIONS(23), 2,
      anon_sym_true,
      anon_sym_false,
    STATE(31), 4,
      sym__aexp,
      sym_plus,
      sym_minus,
      sym_times,
    STATE(40), 5,
      sym__bexp,
      sym_eqb,
      sym_leb,
      sym_negb,
      sym_andb,
  [66] = 8,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(15), 1,
      anon_sym_LPAREN,
    ACTIONS(19), 1,
      anon_sym_TILDE,
    ACTIONS(25), 1,
      sym_id,
    ACTIONS(29), 1,
      sym_num,
    ACTIONS(27), 2,
      anon_sym_true,
      anon_sym_false,
    STATE(30), 4,
      sym__aexp,
      sym_plus,
      sym_minus,
      sym_times,
    STATE(39), 5,
      sym__bexp,
      sym_eqb,
      sym_leb,
      sym_negb,
      sym_andb,
  [99] = 8,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(13), 1,
      sym_id,
    ACTIONS(15), 1,
      anon_sym_LPAREN,
    ACTIONS(19), 1,
      anon_sym_TILDE,
    ACTIONS(21), 1,
      sym_num,
    ACTIONS(31), 2,
      anon_sym_true,
      anon_sym_false,
    STATE(31), 4,
      sym__aexp,
      sym_plus,
      sym_minus,
      sym_times,
    STATE(38), 5,
      sym__bexp,
      sym_eqb,
      sym_leb,
      sym_negb,
      sym_andb,
  [132] = 8,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(13), 1,
      sym_id,
    ACTIONS(15), 1,
      anon_sym_LPAREN,
    ACTIONS(19), 1,
      anon_sym_TILDE,
    ACTIONS(21), 1,
      sym_num,
    ACTIONS(33), 2,
      anon_sym_true,
      anon_sym_false,
    STATE(31), 4,
      sym__aexp,
      sym_plus,
      sym_minus,
      sym_times,
    STATE(35), 5,
      sym__bexp,
      sym_eqb,
      sym_leb,
      sym_negb,
      sym_andb,
  [165] = 3,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(37), 1,
      anon_sym_EQ,
    ACTIONS(35), 12,
      ts_builtin_sym_end,
      anon_sym_SEMI,
      anon_sym_then,
      anon_sym_else,
      anon_sym_end,
      anon_sym_do,
      anon_sym_RPAREN,
      anon_sym_PLUS,
      anon_sym_DASH,
      anon_sym_STAR,
      anon_sym_EQ_LT,
      anon_sym_AMP_AMP,
  [186] = 4,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(41), 1,
      anon_sym_STAR,
    ACTIONS(43), 1,
      anon_sym_EQ,
    ACTIONS(39), 11,
      ts_builtin_sym_end,
      anon_sym_SEMI,
      anon_sym_then,
      anon_sym_else,
      anon_sym_end,
      anon_sym_do,
      anon_sym_RPAREN,
      anon_sym_PLUS,
      anon_sym_DASH,
      anon_sym_EQ_LT,
      anon_sym_AMP_AMP,
  [209] = 4,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(41), 1,
      anon_sym_STAR,
    ACTIONS(47), 1,
      anon_sym_EQ,
    ACTIONS(45), 11,
      ts_builtin_sym_end,
      anon_sym_SEMI,
      anon_sym_then,
      anon_sym_else,
      anon_sym_end,
      anon_sym_do,
      anon_sym_RPAREN,
      anon_sym_PLUS,
      anon_sym_DASH,
      anon_sym_EQ_LT,
      anon_sym_AMP_AMP,
  [232] = 3,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(51), 1,
      anon_sym_EQ,
    ACTIONS(49), 12,
      ts_builtin_sym_end,
      anon_sym_SEMI,
      anon_sym_then,
      anon_sym_else,
      anon_sym_end,
      anon_sym_do,
      anon_sym_RPAREN,
      anon_sym_PLUS,
      anon_sym_DASH,
      anon_sym_STAR,
      anon_sym_EQ_LT,
      anon_sym_AMP_AMP,
  [253] = 9,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(5), 1,
      sym_skip,
    ACTIONS(7), 1,
      sym_id,
    ACTIONS(9), 1,
      anon_sym_if,
    ACTIONS(11), 1,
      anon_sym_while,
    ACTIONS(53), 1,
      anon_sym_SEMI,
    ACTIONS(55), 1,
      anon_sym_end,
    STATE(48), 1,
      sym_stmt,
    STATE(36), 4,
      sym_asgn,
      sym_seq,
      sym_if,
      sym_while,
  [284] = 9,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(5), 1,
      sym_skip,
    ACTIONS(7), 1,
      sym_id,
    ACTIONS(9), 1,
      anon_sym_if,
    ACTIONS(11), 1,
      anon_sym_while,
    ACTIONS(53), 1,
      anon_sym_SEMI,
    ACTIONS(55), 1,
      anon_sym_else,
    STATE(47), 1,
      sym_stmt,
    STATE(36), 4,
      sym_asgn,
      sym_seq,
      sym_if,
      sym_while,
  [315] = 8,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(5), 1,
      sym_skip,
    ACTIONS(7), 1,
      sym_id,
    ACTIONS(9), 1,
      anon_sym_if,
    ACTIONS(11), 1,
      anon_sym_while,
    STATE(44), 1,
      sym_stmt,
    ACTIONS(53), 2,
      ts_builtin_sym_end,
      anon_sym_SEMI,
    STATE(36), 4,
      sym_asgn,
      sym_seq,
      sym_if,
      sym_while,
  [344] = 7,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(5), 1,
      sym_skip,
    ACTIONS(7), 1,
      sym_id,
    ACTIONS(9), 1,
      anon_sym_if,
    ACTIONS(11), 1,
      anon_sym_while,
    STATE(41), 1,
      sym_stmt,
    STATE(36), 4,
      sym_asgn,
      sym_seq,
      sym_if,
      sym_while,
  [369] = 7,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(5), 1,
      sym_skip,
    ACTIONS(7), 1,
      sym_id,
    ACTIONS(9), 1,
      anon_sym_if,
    ACTIONS(11), 1,
      anon_sym_while,
    STATE(48), 1,
      sym_stmt,
    STATE(36), 4,
      sym_asgn,
      sym_seq,
      sym_if,
      sym_while,
  [394] = 7,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(5), 1,
      sym_skip,
    ACTIONS(7), 1,
      sym_id,
    ACTIONS(9), 1,
      anon_sym_if,
    ACTIONS(11), 1,
      anon_sym_while,
    STATE(47), 1,
      sym_stmt,
    STATE(36), 4,
      sym_asgn,
      sym_seq,
      sym_if,
      sym_while,
  [419] = 7,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(5), 1,
      sym_skip,
    ACTIONS(7), 1,
      sym_id,
    ACTIONS(9), 1,
      anon_sym_if,
    ACTIONS(11), 1,
      anon_sym_while,
    STATE(45), 1,
      sym_stmt,
    STATE(36), 4,
      sym_asgn,
      sym_seq,
      sym_if,
      sym_while,
  [444] = 7,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(5), 1,
      sym_skip,
    ACTIONS(7), 1,
      sym_id,
    ACTIONS(9), 1,
      anon_sym_if,
    ACTIONS(11), 1,
      anon_sym_while,
    STATE(44), 1,
      sym_stmt,
    STATE(36), 4,
      sym_asgn,
      sym_seq,
      sym_if,
      sym_while,
  [469] = 7,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(5), 1,
      sym_skip,
    ACTIONS(7), 1,
      sym_id,
    ACTIONS(9), 1,
      anon_sym_if,
    ACTIONS(11), 1,
      anon_sym_while,
    STATE(42), 1,
      sym_stmt,
    STATE(36), 4,
      sym_asgn,
      sym_seq,
      sym_if,
      sym_while,
  [494] = 5,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(41), 1,
      anon_sym_STAR,
    ACTIONS(59), 1,
      anon_sym_PLUS,
    ACTIONS(61), 1,
      anon_sym_DASH,
    ACTIONS(57), 4,
      anon_sym_then,
      anon_sym_do,
      anon_sym_RPAREN,
      anon_sym_AMP_AMP,
  [513] = 4,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(65), 1,
      anon_sym_LPAREN,
    ACTIONS(63), 2,
      sym_id,
      sym_num,
    STATE(8), 4,
      sym__aexp,
      sym_plus,
      sym_minus,
      sym_times,
  [530] = 4,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(65), 1,
      anon_sym_LPAREN,
    ACTIONS(67), 2,
      sym_id,
      sym_num,
    STATE(7), 4,
      sym__aexp,
      sym_plus,
      sym_minus,
      sym_times,
  [547] = 4,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(65), 1,
      anon_sym_LPAREN,
    ACTIONS(69), 2,
      sym_id,
      sym_num,
    STATE(29), 4,
      sym__aexp,
      sym_plus,
      sym_minus,
      sym_times,
  [564] = 4,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(65), 1,
      anon_sym_LPAREN,
    ACTIONS(71), 2,
      sym_id,
      sym_num,
    STATE(20), 4,
      sym__aexp,
      sym_plus,
      sym_minus,
      sym_times,
  [581] = 4,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(65), 1,
      anon_sym_LPAREN,
    ACTIONS(73), 2,
      sym_id,
      sym_num,
    STATE(27), 4,
      sym__aexp,
      sym_plus,
      sym_minus,
      sym_times,
  [598] = 4,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(65), 1,
      anon_sym_LPAREN,
    ACTIONS(75), 2,
      sym_id,
      sym_num,
    STATE(9), 4,
      sym__aexp,
      sym_plus,
      sym_minus,
      sym_times,
  [615] = 5,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(41), 1,
      anon_sym_STAR,
    ACTIONS(59), 1,
      anon_sym_PLUS,
    ACTIONS(61), 1,
      anon_sym_DASH,
    ACTIONS(77), 4,
      ts_builtin_sym_end,
      anon_sym_SEMI,
      anon_sym_else,
      anon_sym_end,
  [634] = 4,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(65), 1,
      anon_sym_LPAREN,
    ACTIONS(79), 2,
      sym_id,
      sym_num,
    STATE(33), 4,
      sym__aexp,
      sym_plus,
      sym_minus,
      sym_times,
  [651] = 5,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(41), 1,
      anon_sym_STAR,
    ACTIONS(59), 1,
      anon_sym_PLUS,
    ACTIONS(61), 1,
      anon_sym_DASH,
    ACTIONS(81), 4,
      anon_sym_then,
      anon_sym_do,
      anon_sym_RPAREN,
      anon_sym_AMP_AMP,
  [670] = 7,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(41), 1,
      anon_sym_STAR,
    ACTIONS(59), 1,
      anon_sym_PLUS,
    ACTIONS(61), 1,
      anon_sym_DASH,
    ACTIONS(83), 1,
      anon_sym_RPAREN,
    ACTIONS(85), 1,
      anon_sym_EQ,
    ACTIONS(87), 1,
      anon_sym_EQ_LT,
  [692] = 6,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(41), 1,
      anon_sym_STAR,
    ACTIONS(59), 1,
      anon_sym_PLUS,
    ACTIONS(61), 1,
      anon_sym_DASH,
    ACTIONS(85), 1,
      anon_sym_EQ,
    ACTIONS(87), 1,
      anon_sym_EQ_LT,
  [711] = 2,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(89), 4,
      ts_builtin_sym_end,
      anon_sym_SEMI,
      anon_sym_else,
      anon_sym_end,
  [721] = 5,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(41), 1,
      anon_sym_STAR,
    ACTIONS(59), 1,
      anon_sym_PLUS,
    ACTIONS(61), 1,
      anon_sym_DASH,
    ACTIONS(83), 1,
      anon_sym_RPAREN,
  [737] = 2,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(91), 4,
      anon_sym_then,
      anon_sym_do,
      anon_sym_RPAREN,
      anon_sym_AMP_AMP,
  [747] = 3,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(95), 1,
      anon_sym_AMP_AMP,
    ACTIONS(93), 3,
      anon_sym_then,
      anon_sym_do,
      anon_sym_RPAREN,
  [759] = 2,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(97), 4,
      ts_builtin_sym_end,
      anon_sym_SEMI,
      anon_sym_else,
      anon_sym_end,
  [769] = 2,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(99), 4,
      ts_builtin_sym_end,
      anon_sym_SEMI,
      anon_sym_else,
      anon_sym_end,
  [779] = 2,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(101), 4,
      anon_sym_then,
      anon_sym_do,
      anon_sym_RPAREN,
      anon_sym_AMP_AMP,
  [789] = 3,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(95), 1,
      anon_sym_AMP_AMP,
    ACTIONS(103), 1,
      anon_sym_RPAREN,
  [799] = 3,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(95), 1,
      anon_sym_AMP_AMP,
    ACTIONS(105), 1,
      anon_sym_do,
  [809] = 3,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(107), 1,
      anon_sym_SEMI,
    ACTIONS(109), 1,
      anon_sym_else,
  [819] = 3,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(111), 1,
      anon_sym_SEMI,
    ACTIONS(113), 1,
      anon_sym_end,
  [829] = 3,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(95), 1,
      anon_sym_AMP_AMP,
    ACTIONS(115), 1,
      anon_sym_then,
  [839] = 3,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(117), 1,
      ts_builtin_sym_end,
    ACTIONS(119), 1,
      anon_sym_SEMI,
  [849] = 3,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(111), 1,
      anon_sym_SEMI,
    ACTIONS(121), 1,
      anon_sym_end,
  [859] = 3,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(123), 1,
      ts_builtin_sym_end,
    ACTIONS(125), 1,
      anon_sym_SEMI,
  [869] = 3,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(117), 1,
      anon_sym_else,
    ACTIONS(127), 1,
      anon_sym_SEMI,
  [879] = 3,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(117), 1,
      anon_sym_end,
    ACTIONS(129), 1,
      anon_sym_SEMI,
  [889] = 2,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(131), 1,
      ts_builtin_sym_end,
  [896] = 2,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(133), 1,
      anon_sym_COLON_EQ,
};

static const uint32_t ts_small_parse_table_map[] = {
  [SMALL_STATE(2)] = 0,
  [SMALL_STATE(3)] = 33,
  [SMALL_STATE(4)] = 66,
  [SMALL_STATE(5)] = 99,
  [SMALL_STATE(6)] = 132,
  [SMALL_STATE(7)] = 165,
  [SMALL_STATE(8)] = 186,
  [SMALL_STATE(9)] = 209,
  [SMALL_STATE(10)] = 232,
  [SMALL_STATE(11)] = 253,
  [SMALL_STATE(12)] = 284,
  [SMALL_STATE(13)] = 315,
  [SMALL_STATE(14)] = 344,
  [SMALL_STATE(15)] = 369,
  [SMALL_STATE(16)] = 394,
  [SMALL_STATE(17)] = 419,
  [SMALL_STATE(18)] = 444,
  [SMALL_STATE(19)] = 469,
  [SMALL_STATE(20)] = 494,
  [SMALL_STATE(21)] = 513,
  [SMALL_STATE(22)] = 530,
  [SMALL_STATE(23)] = 547,
  [SMALL_STATE(24)] = 564,
  [SMALL_STATE(25)] = 581,
  [SMALL_STATE(26)] = 598,
  [SMALL_STATE(27)] = 615,
  [SMALL_STATE(28)] = 634,
  [SMALL_STATE(29)] = 651,
  [SMALL_STATE(30)] = 670,
  [SMALL_STATE(31)] = 692,
  [SMALL_STATE(32)] = 711,
  [SMALL_STATE(33)] = 721,
  [SMALL_STATE(34)] = 737,
  [SMALL_STATE(35)] = 747,
  [SMALL_STATE(36)] = 759,
  [SMALL_STATE(37)] = 769,
  [SMALL_STATE(38)] = 779,
  [SMALL_STATE(39)] = 789,
  [SMALL_STATE(40)] = 799,
  [SMALL_STATE(41)] = 809,
  [SMALL_STATE(42)] = 819,
  [SMALL_STATE(43)] = 829,
  [SMALL_STATE(44)] = 839,
  [SMALL_STATE(45)] = 849,
  [SMALL_STATE(46)] = 859,
  [SMALL_STATE(47)] = 869,
  [SMALL_STATE(48)] = 879,
  [SMALL_STATE(49)] = 889,
  [SMALL_STATE(50)] = 896,
};

static const TSParseActionEntry ts_parse_actions[] = {
  [0] = {.entry = {.count = 0, .reusable = false}},
  [1] = {.entry = {.count = 1, .reusable = false}}, RECOVER(),
  [3] = {.entry = {.count = 1, .reusable = true}}, SHIFT_EXTRA(),
  [5] = {.entry = {.count = 1, .reusable = false}}, SHIFT(36),
  [7] = {.entry = {.count = 1, .reusable = false}}, SHIFT(50),
  [9] = {.entry = {.count = 1, .reusable = false}}, SHIFT(2),
  [11] = {.entry = {.count = 1, .reusable = false}}, SHIFT(3),
  [13] = {.entry = {.count = 1, .reusable = false}}, SHIFT(31),
  [15] = {.entry = {.count = 1, .reusable = true}}, SHIFT(4),
  [17] = {.entry = {.count = 1, .reusable = false}}, SHIFT(43),
  [19] = {.entry = {.count = 1, .reusable = true}}, SHIFT(5),
  [21] = {.entry = {.count = 1, .reusable = true}}, SHIFT(31),
  [23] = {.entry = {.count = 1, .reusable = false}}, SHIFT(40),
  [25] = {.entry = {.count = 1, .reusable = false}}, SHIFT(30),
  [27] = {.entry = {.count = 1, .reusable = false}}, SHIFT(39),
  [29] = {.entry = {.count = 1, .reusable = true}}, SHIFT(30),
  [31] = {.entry = {.count = 1, .reusable = false}}, SHIFT(38),
  [33] = {.entry = {.count = 1, .reusable = false}}, SHIFT(35),
  [35] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_times, 3),
  [37] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_times, 3),
  [39] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_minus, 3),
  [41] = {.entry = {.count = 1, .reusable = true}}, SHIFT(22),
  [43] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_minus, 3),
  [45] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_plus, 3),
  [47] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_plus, 3),
  [49] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym__aexp, 3),
  [51] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym__aexp, 3),
  [53] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_seq, 4),
  [55] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_seq, 4),
  [57] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_leb, 3),
  [59] = {.entry = {.count = 1, .reusable = true}}, SHIFT(26),
  [61] = {.entry = {.count = 1, .reusable = true}}, SHIFT(21),
  [63] = {.entry = {.count = 1, .reusable = true}}, SHIFT(8),
  [65] = {.entry = {.count = 1, .reusable = true}}, SHIFT(28),
  [67] = {.entry = {.count = 1, .reusable = true}}, SHIFT(7),
  [69] = {.entry = {.count = 1, .reusable = true}}, SHIFT(29),
  [71] = {.entry = {.count = 1, .reusable = true}}, SHIFT(20),
  [73] = {.entry = {.count = 1, .reusable = true}}, SHIFT(27),
  [75] = {.entry = {.count = 1, .reusable = true}}, SHIFT(9),
  [77] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_asgn, 3, .production_id = 1),
  [79] = {.entry = {.count = 1, .reusable = true}}, SHIFT(33),
  [81] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_eqb, 3),
  [83] = {.entry = {.count = 1, .reusable = true}}, SHIFT(10),
  [85] = {.entry = {.count = 1, .reusable = false}}, SHIFT(23),
  [87] = {.entry = {.count = 1, .reusable = true}}, SHIFT(24),
  [89] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_while, 5, .production_id = 2),
  [91] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym__bexp, 3),
  [93] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_andb, 3),
  [95] = {.entry = {.count = 1, .reusable = true}}, SHIFT(6),
  [97] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_stmt, 1),
  [99] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_if, 7, .production_id = 3),
  [101] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_negb, 2),
  [103] = {.entry = {.count = 1, .reusable = true}}, SHIFT(34),
  [105] = {.entry = {.count = 1, .reusable = true}}, SHIFT(19),
  [107] = {.entry = {.count = 1, .reusable = true}}, SHIFT(16),
  [109] = {.entry = {.count = 1, .reusable = true}}, SHIFT(17),
  [111] = {.entry = {.count = 1, .reusable = true}}, SHIFT(15),
  [113] = {.entry = {.count = 1, .reusable = true}}, SHIFT(32),
  [115] = {.entry = {.count = 1, .reusable = true}}, SHIFT(14),
  [117] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_seq, 3),
  [119] = {.entry = {.count = 1, .reusable = true}}, SHIFT(13),
  [121] = {.entry = {.count = 1, .reusable = true}}, SHIFT(37),
  [123] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_program, 1),
  [125] = {.entry = {.count = 1, .reusable = true}}, SHIFT(18),
  [127] = {.entry = {.count = 1, .reusable = true}}, SHIFT(12),
  [129] = {.entry = {.count = 1, .reusable = true}}, SHIFT(11),
  [131] = {.entry = {.count = 1, .reusable = true}},  ACCEPT_INPUT(),
  [133] = {.entry = {.count = 1, .reusable = true}}, SHIFT(25),
};

#ifdef __cplusplus
extern "C" {
#endif
#ifdef _WIN32
#define extern __declspec(dllexport)
#endif

extern const TSLanguage *tree_sitter_crochet(void) {
  static const TSLanguage language = {
    .version = LANGUAGE_VERSION,
    .symbol_count = SYMBOL_COUNT,
    .alias_count = ALIAS_COUNT,
    .token_count = TOKEN_COUNT,
    .external_token_count = EXTERNAL_TOKEN_COUNT,
    .state_count = STATE_COUNT,
    .large_state_count = LARGE_STATE_COUNT,
    .production_id_count = PRODUCTION_ID_COUNT,
    .field_count = FIELD_COUNT,
    .max_alias_sequence_length = MAX_ALIAS_SEQUENCE_LENGTH,
    .parse_table = &ts_parse_table[0][0],
    .small_parse_table = ts_small_parse_table,
    .small_parse_table_map = ts_small_parse_table_map,
    .parse_actions = ts_parse_actions,
    .symbol_names = ts_symbol_names,
    .field_names = ts_field_names,
    .field_map_slices = ts_field_map_slices,
    .field_map_entries = ts_field_map_entries,
    .symbol_metadata = ts_symbol_metadata,
    .public_symbol_map = ts_symbol_map,
    .alias_map = ts_non_terminal_alias_map,
    .alias_sequences = &ts_alias_sequences[0][0],
    .lex_modes = ts_lex_modes,
    .lex_fn = ts_lex,
  };
  return &language;
}
#ifdef __cplusplus
}
#endif
