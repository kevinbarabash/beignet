use std::cmp::Ordering;

use lsp_types::*;

use crochet_ast::values::*;

use crate::visitor::Visitor;

pub fn get_semantic_tokens(prog: &mut Program) -> Vec<SemanticToken> {
    let mut visitor = SemanticTokenVisitor { raw_tokens: vec![] };
    visitor.visit(prog);

    visitor.raw_tokens.sort_by(|a, b| {
        if a.line < b.line {
            return Ordering::Less;
        }

        if a.line == b.line && a.start < b.start {
            return Ordering::Less;
        }

        if a.line == b.line && a.start == b.start {
            return Ordering::Equal;
        }

        Ordering::Greater
    });

    let mut semantic_tokens: Vec<SemanticToken> = vec![];

    let mut prev_line = 0;
    let mut prev_start = 0;

    for raw_token in visitor.raw_tokens {
        semantic_tokens.push(SemanticToken {
            delta_line: raw_token.line - prev_line,
            delta_start: if raw_token.line == prev_line {
                raw_token.start - prev_start
            } else {
                raw_token.start
            },
            length: raw_token.length,
            token_type: raw_token.token_type,
            token_modifiers_bitset: raw_token.token_modifiers_bitset,
        });

        prev_line = raw_token.line;
        prev_start = raw_token.start;
    }

    eprintln!("{semantic_tokens:#?}");

    semantic_tokens
}

#[derive(Debug)]
pub struct RawSemanticToken {
    pub line: u32,
    pub start: u32,
    pub length: u32,
    pub token_type: u32,
    pub token_modifiers_bitset: u32,
}

struct SemanticTokenVisitor {
    pub raw_tokens: Vec<RawSemanticToken>,
}

impl Visitor for SemanticTokenVisitor {
    fn visit_program(&mut self, _: &crochet_ast::values::Program) {
        eprintln!("visit_program");
    }

    fn visit_statement(&mut self, _stmt: &crochet_ast::values::Statement) {
        eprintln!("visit_statement");
    }

    fn visit_pattern(&mut self, _pat: &crochet_ast::values::Pattern) {
        eprintln!("visit_pattern");
    }

    fn visit_type_ann(&mut self, type_ann: &crochet_ast::values::TypeAnn) {
        // TYPE = 0
        // TYPE_PARAMATER = 2
        // KEYWORD = 8
        // STRING = 10
        // NUMBER = 11
        let token_type: Option<u32> = match &type_ann.kind {
            crochet_ast::values::TypeAnnKind::Lam(_) => None,
            crochet_ast::values::TypeAnnKind::Lit(lit) => match lit {
                Lit::Num(_) => Some(11),
                Lit::Bool(_) => Some(0),
                Lit::Str(_) => Some(10),
            },
            crochet_ast::values::TypeAnnKind::Keyword(_) => Some(0),
            crochet_ast::values::TypeAnnKind::Object(_) => None,
            crochet_ast::values::TypeAnnKind::TypeRef(_) => Some(0),
            crochet_ast::values::TypeAnnKind::Union(_) => None,
            crochet_ast::values::TypeAnnKind::Intersection(_) => None,
            crochet_ast::values::TypeAnnKind::Tuple(_) => None,
            crochet_ast::values::TypeAnnKind::Array(_) => None,
            crochet_ast::values::TypeAnnKind::KeyOf(_) => None,
            crochet_ast::values::TypeAnnKind::Query(_) => None,
            crochet_ast::values::TypeAnnKind::IndexedAccess(_) => None,
            crochet_ast::values::TypeAnnKind::Mapped(_) => None,
            crochet_ast::values::TypeAnnKind::Conditional(_) => None,
            // TODO: figure out how to get the SourceLocation for just the `mut`
            // modifier
            crochet_ast::values::TypeAnnKind::Mutable(_) => None,
        };

        let TypeAnn { loc, .. } = type_ann;
        if let Some(token_type) = token_type {
            let raw_token = RawSemanticToken {
                line: loc.start.line,
                start: loc.start.column,
                // assumes that tokens don't span multiple lines
                length: loc.end.column - loc.start.column,
                token_type,
                token_modifiers_bitset: 0,
            };
            eprintln!("{raw_token:#?}");
            self.raw_tokens.push(raw_token)
        }
    }

    fn visit_expr(&mut self, expr: &crochet_ast::values::Expr) {
        // PARAMETER = 3
        // VARIABLE = 4
        // PROPERTY = 5
        // FUNCTION = 6
        // METHOD = 7
        let token_type: Option<u32> = match &expr.kind {
            crochet_ast::values::ExprKind::App(_) => None,
            crochet_ast::values::ExprKind::New(_) => None,
            crochet_ast::values::ExprKind::Fix(_) => None,
            // TODO: Figure out how to differentiate identifiers used for different
            // purposes: e.g. parameters, varaibles, properties, etc.
            crochet_ast::values::ExprKind::Ident(_) => Some(4),
            crochet_ast::values::ExprKind::IfElse(_) => None,
            crochet_ast::values::ExprKind::JSXElement(_) => None,
            crochet_ast::values::ExprKind::Lambda(_) => None,
            crochet_ast::values::ExprKind::Let(_) => None,
            crochet_ast::values::ExprKind::Assign(_) => None,
            crochet_ast::values::ExprKind::LetExpr(_) => None,
            crochet_ast::values::ExprKind::Lit(lit) => match lit {
                Lit::Num(_) => Some(11),
                Lit::Bool(_) => Some(8),
                Lit::Str(_) => Some(10),
            },
            crochet_ast::values::ExprKind::Keyword(_) => None,
            crochet_ast::values::ExprKind::BinaryExpr(_) => None,
            crochet_ast::values::ExprKind::UnaryExpr(_) => None,
            crochet_ast::values::ExprKind::Obj(_) => None,
            crochet_ast::values::ExprKind::Await(_) => None,
            crochet_ast::values::ExprKind::Tuple(_) => None,
            crochet_ast::values::ExprKind::Member(_) => None,
            crochet_ast::values::ExprKind::Empty => None,
            crochet_ast::values::ExprKind::TemplateLiteral(_) => None,
            crochet_ast::values::ExprKind::TaggedTemplateLiteral(_) => None,
            crochet_ast::values::ExprKind::Match(_) => None,
        };

        let Expr { loc, .. } = expr;
        if let Some(token_type) = token_type {
            self.raw_tokens.push(RawSemanticToken {
                line: loc.start.line,
                start: loc.start.column,
                // assumes that tokens don't span multiple lines
                length: loc.end.column - loc.start.column,
                token_type,
                token_modifiers_bitset: 0,
            })
        }
    }
}
