use regex_syntax::hir::{visit, Hir, Visitor};
use regex_syntax::Parser;

use crochet_ast::types::*;

#[derive(Default)]
struct RegexVisitor {
    pub optional_count: u32,
    pub elems: Vec<Type>,
    pub groups: Vec<TObjElem>,
}

impl Visitor for RegexVisitor {
    type Output = Type;
    type Err = String;

    fn finish(self) -> Result<Self::Output, Self::Err> {
        let elems = self.elems.clone();
        let tuple = Type::from(TypeKind::Tuple(elems));

        let t = if self.groups.is_empty() {
            tuple
        } else {
            let groups = Type::from(TypeKind::Object(TObject {
                elems: self.groups,
                is_interface: false,
            }));
            let obj = Type::from(TypeKind::Object(TObject {
                elems: vec![TObjElem::Prop(TProp {
                    name: TPropKey::StringKey("groups".to_string()),
                    mutable: false,
                    optional: false,
                    t: groups,
                })],
                is_interface: false,
            }));

            Type::from(TypeKind::Intersection(vec![tuple, obj]))
        };
        Ok(t)
    }

    fn start(&mut self) {
        self.elems
            .push(Type::from(TypeKind::Keyword(TKeyword::String)));
    }
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
                    regex_syntax::hir::GroupKind::CaptureIndex(_) => {
                        let mut t = Type::from(TypeKind::Keyword(TKeyword::String));
                        if self.optional_count > 0 {
                            let undefined = Type::from(TypeKind::Keyword(TKeyword::Undefined));
                            t = Type::from(TypeKind::Union(vec![t, undefined]));
                        }
                        self.elems.push(t);
                    }
                    regex_syntax::hir::GroupKind::CaptureName { name, index: _ } => {
                        // named capture groups also appear in the same array
                        // as regular capture groups
                        let mut t = Type::from(TypeKind::Keyword(TKeyword::String));
                        if self.optional_count > 0 {
                            let undefined = Type::from(TypeKind::Keyword(TKeyword::Undefined));
                            t = Type::from(TypeKind::Union(vec![t, undefined]));
                        }
                        self.elems.push(t);
                        self.groups.push(TObjElem::Prop(TProp {
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

pub fn parse_regex(pattern: &str) -> Type {
    let hir = Parser::new()
        .parse(&pattern.replace("(?<", "(?P<"))
        .unwrap();
    let visitor = RegexVisitor::default();
    visit(&hir, visitor).unwrap()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn no_capture_groups() {
        let t = parse_regex("foo|bar");
        assert_eq!(t.to_string(), "[string]");
    }

    #[test]
    fn capture_groups() {
        let t = parse_regex("(foo)(bar)");
        assert_eq!(t.to_string(), "[string, string, string]");
    }

    #[test]
    fn optional_capture_groups() {
        let t = parse_regex("(foo)|(bar)");
        assert_eq!(
            t.to_string(),
            "[string, string | undefined, string | undefined]"
        );
    }

    #[test]
    fn some_optional_capture_groups() {
        let t = parse_regex("(foo)+(bar)?");
        assert_eq!(t.to_string(), "[string, string, string | undefined]");
    }

    #[test]
    fn named_capture_groups() {
        let t = parse_regex("(?<x>\\d+),(?<y>\\d+)");
        assert_eq!(
            t.to_string(),
            "[string, string, string] & {groups: {x: string, y: string}}"
        );
    }

    #[test]
    fn optional_name_capture_groups() {
        let t = parse_regex("(?<x>\\d+)|(?<y>\\d+)");
        assert_eq!(
            t.to_string(),
            "[string, string | undefined, string | undefined] & {groups: {x?: string, y?: string}}",
        );
    }

    #[test]
    fn some_optional_named_capture_groups_and_non_capturing_group() {
        let t = parse_regex("(?<x>\\d+),(?<y>\\d+)(?:,(?<z>\\d+))?");
        assert_eq!(
            t.to_string(),
            "[string, string, string, string | undefined] & {groups: {x: string, y: string, z?: string}}",
        );
    }
}
