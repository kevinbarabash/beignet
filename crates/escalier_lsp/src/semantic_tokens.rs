use derive_visitor::{DriveMut, VisitorMut};
use std::cmp::Ordering;

use lsp_types::*;

use escalier_ast::values::*;

pub fn get_semantic_tokens(prog: &mut Program) -> Vec<SemanticToken> {
    let mut visitor = SemanticTokenVisitor { raw_tokens: vec![] };
    prog.drive_mut(&mut visitor);

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

#[derive(VisitorMut)]
#[visitor(Expr(enter), Statement(enter), TypeAnn(enter))]
struct SemanticTokenVisitor {
    pub raw_tokens: Vec<RawSemanticToken>,
}

impl SemanticTokenVisitor {
    fn enter_expr(&mut self, expr: &Expr) {
        // PARAMETER = 3
        // VARIABLE = 4
        // PROPERTY = 5
        // FUNCTION = 6
        // METHOD = 7
        let token_type: Option<u32> = match &expr.kind {
            ExprKind::App(_) => None,
            ExprKind::New(_) => None,
            ExprKind::Fix(_) => None,
            // TODO: Figure out how to differentiate identifiers used for different
            // purposes: e.g. parameters, varaibles, properties, etc.
            ExprKind::Ident(_) => Some(4),
            ExprKind::IfElse(_) => None,
            ExprKind::JSXElement(_) => None,
            ExprKind::Lambda(_) => None,
            ExprKind::Assign(_) => None,
            ExprKind::LetExpr(_) => None,
            ExprKind::Lit(lit) => match lit {
                Lit::Num(_) => Some(11),
                Lit::Bool(_) => None,
                Lit::Str(_) => Some(10),
            },
            ExprKind::Keyword(_) => None,
            ExprKind::BinaryExpr(_) => None,
            ExprKind::UnaryExpr(_) => None,
            ExprKind::Obj(_) => None,
            ExprKind::Await(_) => None,
            ExprKind::Tuple(_) => None,
            ExprKind::Member(_) => None,
            ExprKind::Empty => None,
            ExprKind::TemplateLiteral(_) => None,
            ExprKind::TaggedTemplateLiteral(_) => None,
            ExprKind::Match(_) => None,
            ExprKind::Class(_) => None,
            ExprKind::Regex(_) => None, // TODO
            ExprKind::DoExpr(_) => None,
            ExprKind::LetDecl(_) => None,
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

    fn enter_statement(&mut self, stmt: &Statement) {
        if let Statement::TypeDecl {
            id,
            type_ann: _,
            type_params: _,
            ..
        } = stmt
        {
            let Ident { loc, .. } = id;
            self.raw_tokens.push(RawSemanticToken {
                line: loc.start.line,
                start: loc.start.column,
                // assumes that tokens don't span multiple lines
                length: loc.end.column - loc.start.column,
                token_type: 0, // TYPE
                token_modifiers_bitset: 0,
            })
        }
    }

    fn enter_type_ann(&mut self, type_ann: &TypeAnn) {
        // TYPE = 0
        // TYPE_PARAMATER = 2
        // KEYWORD = 8
        // STRING = 10
        // NUMBER = 11
        let token_type: Option<u32> = match &type_ann.kind {
            escalier_ast::values::TypeAnnKind::Lam(_) => None,
            escalier_ast::values::TypeAnnKind::Lit(lit) => match lit {
                Lit::Num(_) => Some(11),
                Lit::Bool(_) => None,
                Lit::Str(_) => Some(10),
            },
            escalier_ast::values::TypeAnnKind::Keyword(_) => Some(0),
            escalier_ast::values::TypeAnnKind::Object(_) => None,
            escalier_ast::values::TypeAnnKind::TypeRef(TypeRef {
                name: _,
                type_args: _,
            }) => {
                // TODO: have separate tokens for `name` and `type_args`
                // Right now `Baz<number>` in `let baz: Baz<number> = true;`
                // is solid green.
                Some(0)
            }
            escalier_ast::values::TypeAnnKind::Union(_) => None,
            escalier_ast::values::TypeAnnKind::Intersection(_) => None,
            escalier_ast::values::TypeAnnKind::Tuple(_) => None,
            escalier_ast::values::TypeAnnKind::Array(_) => None,
            escalier_ast::values::TypeAnnKind::KeyOf(_) => None,
            escalier_ast::values::TypeAnnKind::Query(_) => None,
            escalier_ast::values::TypeAnnKind::IndexedAccess(_) => None,
            escalier_ast::values::TypeAnnKind::Mapped(_) => None,
            escalier_ast::values::TypeAnnKind::Conditional(_) => None,
            escalier_ast::values::TypeAnnKind::Infer(_) => None,
            // TODO: figure out how to get the SourceLocation for just the `mut`
            // modifier
            escalier_ast::values::TypeAnnKind::Mutable(_) => None,
        };

        let TypeAnn { loc, .. } = type_ann;
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
