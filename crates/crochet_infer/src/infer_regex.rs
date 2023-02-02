use regex_syntax::hir::{visit, Hir, Visitor};
use regex_syntax::Parser;

use crochet_ast::types::*;

#[derive(Default)]
struct RegexVisitor {
    pub optional_count: u32,
    pub elems: Vec<TObjElem>,
}

impl Visitor for RegexVisitor {
    type Output = Type;
    type Err = String;

    fn finish(self) -> Result<Self::Output, Self::Err> {
        let t = Type::from(TypeKind::Object(TObject {
            elems: self.elems,
            is_interface: false,
        }));
        Ok(t)
    }

    fn start(&mut self) {}
    fn visit_pre(&mut self, hir: &Hir) -> Result<(), Self::Err> {
        match hir.kind() {
            regex_syntax::hir::HirKind::Empty => (),
            regex_syntax::hir::HirKind::Literal(_) => (),
            regex_syntax::hir::HirKind::Class(_) => (),
            regex_syntax::hir::HirKind::Anchor(_) => (),
            regex_syntax::hir::HirKind::WordBoundary(_) => (),
            regex_syntax::hir::HirKind::Repetition(rep) => match rep.kind {
                regex_syntax::hir::RepetitionKind::ZeroOrOne => self.optional_count += 1,
                regex_syntax::hir::RepetitionKind::ZeroOrMore => self.optional_count += 1,
                regex_syntax::hir::RepetitionKind::OneOrMore => (),
                regex_syntax::hir::RepetitionKind::Range(_) => {
                    // TODO
                    todo!()
                }
            },
            regex_syntax::hir::HirKind::Group(_) => (),
            regex_syntax::hir::HirKind::Concat(_) => (),
            regex_syntax::hir::HirKind::Alternation(_) => self.optional_count += 1,
        };
        Ok(())
    }
    fn visit_post(&mut self, hir: &Hir) -> Result<(), Self::Err> {
        match hir.kind() {
            regex_syntax::hir::HirKind::Empty => (),
            regex_syntax::hir::HirKind::Literal(_) => (),
            regex_syntax::hir::HirKind::Class(_) => (),
            regex_syntax::hir::HirKind::Anchor(_) => (),
            regex_syntax::hir::HirKind::WordBoundary(_) => (),
            regex_syntax::hir::HirKind::Repetition(rep) => match rep.kind {
                regex_syntax::hir::RepetitionKind::ZeroOrOne => self.optional_count -= 1,
                regex_syntax::hir::RepetitionKind::ZeroOrMore => self.optional_count -= 1,
                regex_syntax::hir::RepetitionKind::OneOrMore => (),
                regex_syntax::hir::RepetitionKind::Range(_) => {
                    // TODO
                    todo!()
                }
            },
            regex_syntax::hir::HirKind::Group(group) => {
                match &group.kind {
                    regex_syntax::hir::GroupKind::CaptureIndex(index) => {
                        self.elems.push(TObjElem::Prop(TProp {
                            name: TPropKey::NumberKey(index.to_string()),
                            optional: self.optional_count > 0,
                            mutable: false,
                            t: Type::from(TypeKind::Keyword(TKeyword::String)),
                        }));
                    }
                    regex_syntax::hir::GroupKind::CaptureName { name, index } => {
                        // named capture groups also appear in the same array
                        // as regular capture groups
                        self.elems.push(TObjElem::Prop(TProp {
                            name: TPropKey::NumberKey(index.to_string()),
                            optional: self.optional_count > 0,
                            mutable: false,
                            t: Type::from(TypeKind::Keyword(TKeyword::String)),
                        }));
                        self.elems.push(TObjElem::Prop(TProp {
                            name: TPropKey::NumberKey(name.to_string()),
                            optional: self.optional_count > 0,
                            mutable: false,
                            t: Type::from(TypeKind::Keyword(TKeyword::String)),
                        }));
                    }
                    regex_syntax::hir::GroupKind::NonCapturing => (),
                }
            }
            regex_syntax::hir::HirKind::Concat(_) => (),
            regex_syntax::hir::HirKind::Alternation(_) => self.optional_count -= 1,
        };
        Ok(())
    }
    fn visit_alternation_in(&mut self) -> Result<(), Self::Err> {
        Ok(())
    }
}

fn parse_regex(pattern: &str) -> Type {
    let hir = Parser::new().parse(pattern).unwrap();
    let mut visitor = RegexVisitor::default();
    visitor.elems.push(TObjElem::Prop(TProp {
        name: TPropKey::StringKey("0".to_string()),
        optional: false,
        mutable: false,
        t: Type::from(TypeKind::Keyword(TKeyword::String)),
    }));
    visit(&hir, visitor).unwrap()
}

pub fn infer_regex(pattern: &Type) -> Type {
    if let TypeKind::Lit(TLit::Str(pattern)) = &pattern.kind {
        parse_regex(pattern)
    } else {
        todo!()
    }
}
