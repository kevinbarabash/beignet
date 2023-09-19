use std::cmp::Ordering;
use swc_common::source_map::SourceFile;

use lsp_types::*;

use escalier_ast::*;

use crate::util::*;

pub fn get_semantic_tokens(file: &SourceFile, program: &mut Script) -> Vec<SemanticToken> {
    let mut visitor = SemanticTokenVisitor {
        file,
        raw_tokens: vec![],
    };

    visitor.visit_program(program);

    visitor.raw_tokens.sort_by(|a, b| {
        if a.line < b.line {
            return Ordering::Less;
        }

        if a.line == b.line && a.character < b.character {
            return Ordering::Less;
        }

        if a.line == b.line && a.character == b.character {
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
                raw_token.character - prev_start
            } else {
                raw_token.character
            },
            length: raw_token.length,
            token_type: raw_token.token_type,
            token_modifiers_bitset: raw_token.token_modifiers_bitset,
        });

        prev_line = raw_token.line;
        prev_start = raw_token.character;
    }

    eprintln!("{semantic_tokens:#?}");

    semantic_tokens
}

#[derive(Debug)]
pub struct RawSemanticToken {
    pub line: u32,
    pub character: u32,
    pub length: u32,
    pub token_type: u32,
    pub token_modifiers_bitset: u32,
}

struct SemanticTokenVisitor<'a> {
    pub file: &'a SourceFile,
    pub raw_tokens: Vec<RawSemanticToken>,
}

impl<'a> Visitor for SemanticTokenVisitor<'a> {
    fn visit_expr(&mut self, expr: &Expr) {
        // PARAMETER = 3
        // VARIABLE = 4
        // PROPERTY = 5
        // FUNCTION = 6
        // METHOD = 7
        let token_type: Option<u32> = match &expr.kind {
            ExprKind::Call(_) => None,
            // ExprKind::New(_) => None,
            // TODO: Figure out how to differentiate identifiers used for different
            // purposes: e.g. parameters, varaibles, properties, etc.
            ExprKind::Ident(_) => Some(4),
            ExprKind::IfElse(_) => None,
            ExprKind::JSXElement(_) => None,
            ExprKind::JSXFragment(_) => None,
            ExprKind::Function(_) => None,
            ExprKind::Assign(_) => None,
            ExprKind::Num(_) => Some(11),
            ExprKind::Bool(_) => None,
            ExprKind::Str(_) => Some(10),
            ExprKind::Null(_) => None,
            ExprKind::Undefined(_) => None,
            ExprKind::Binary(_) => None,
            ExprKind::Unary(_) => None,
            ExprKind::Object(_) => None,
            ExprKind::Tuple(_) => None,
            ExprKind::Member(_) => None,
            ExprKind::TemplateLiteral(_) => None,
            ExprKind::TaggedTemplateLiteral(_) => None,
            ExprKind::Match(_) => None,
            ExprKind::Class(_) => None,
            ExprKind::Do(_) => None,
            ExprKind::Try(_) => None,
            ExprKind::Throw(_) => None,
            ExprKind::Yield(_) => None,
            ExprKind::Await(_) => None,
        };

        let Expr { span, .. } = expr;

        if let Some(token_type) = token_type {
            let start = get_location(self.file, span.start as u32).unwrap();
            let end = get_location(self.file, span.end as u32).unwrap();
            let loc = SourceLocation { start, end };

            self.raw_tokens.push(RawSemanticToken {
                line: loc.start.line,
                character: loc.start.character,
                // assumes that tokens don't span multiple lines
                length: loc.end.character - loc.start.character,
                token_type,
                token_modifiers_bitset: 0,
            })
        }

        walk_expr(self, expr);
    }

    fn visit_stmt(&mut self, stmt: &Stmt) {
        // TODO: update type_decl.name to be an Ident
        // if let StmtKind::TypeDecl(type_decl) = &stmt.kind {
        //     let Ident { span, .. } = type_decl.name;

        //     let start = get_location(self.file, span.start as u32).unwrap();
        //     let end = get_location(self.file, span.end as u32).unwrap();
        //     let loc = SourceLocation { start, end };

        //     self.raw_tokens.push(RawSemanticToken {
        //         line: loc.start.line,
        //         start: loc.start.column,
        //         // assumes that tokens don't span multiple lines
        //         length: loc.end.column - loc.start.column,
        //         token_type: 0, // TYPE
        //         token_modifiers_bitset: 0,
        //     })
        // }

        walk_stmt(self, stmt);
    }

    fn visit_type_ann(&mut self, type_ann: &TypeAnn) {
        // TYPE = 0
        // TYPE_PARAMATER = 2
        // KEYWORD = 8
        // STRING = 10
        // NUMBER = 11
        let token_type: Option<u32> = match &type_ann.kind {
            TypeAnnKind::Function(_) => None,
            TypeAnnKind::Object(_) => None,
            TypeAnnKind::TypeRef(_name, _type_args) => {
                // TODO: have separate tokens for `name` and `type_args`
                // Right now `Baz<number>` in `let baz: Baz<number> = true;`
                // is solid green.
                Some(0)
            }
            TypeAnnKind::Union(_) => None,
            TypeAnnKind::Intersection(_) => None,
            TypeAnnKind::Tuple(_) => None,
            TypeAnnKind::Array(_) => None,
            TypeAnnKind::KeyOf(_) => None,
            // TypeAnnKind::Query(_) => None,
            TypeAnnKind::IndexedAccess(_, _) => None,
            TypeAnnKind::Condition(_) => None,
            TypeAnnKind::Infer(_) => None,
            TypeAnnKind::BoolLit(_) => None,
            TypeAnnKind::Boolean => Some(0),
            TypeAnnKind::NumLit(_) => Some(11),
            TypeAnnKind::Number => Some(0),
            TypeAnnKind::StrLit(_) => Some(10),
            TypeAnnKind::String => Some(0),
            TypeAnnKind::Symbol => None,
            TypeAnnKind::Null => None,
            TypeAnnKind::Undefined => None,
            TypeAnnKind::Unknown => Some(0),
            TypeAnnKind::Never => Some(0),
            TypeAnnKind::Rest(_) => None,
            TypeAnnKind::TypeOf(_) => None,
            TypeAnnKind::Match(_) => None,
            TypeAnnKind::Wildcard => None,
            TypeAnnKind::Binary(_) => None,
        };

        let TypeAnn { span, .. } = type_ann;
        if let Some(token_type) = token_type {
            let start = get_location(self.file, span.start as u32).unwrap();
            let end = get_location(self.file, span.end as u32).unwrap();
            let loc = SourceLocation { start, end };

            self.raw_tokens.push(RawSemanticToken {
                line: loc.start.line,
                character: loc.start.character,
                // assumes that tokens don't span multiple lines
                length: loc.end.character - loc.start.character,
                token_type,
                token_modifiers_bitset: 0,
            })
        }

        walk_type_ann(self, type_ann);
    }
}
