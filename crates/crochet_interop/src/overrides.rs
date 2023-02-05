use swc_ecma_ast::TsInterfaceDecl;

use crochet_ast::types::*;
use crochet_infer::{Visitor, VisitorMut};

pub fn maybe_override_string_methods(decl: &TsInterfaceDecl, elem: &TObjElem) -> Option<TObjElem> {
    if decl.id.sym.to_string() == "String" {
        if let TObjElem::Method(method) = &elem {
            let mut visitor = HasTypeRefVisitor::new("RegExp");
            visitor.visit_obj_elem(elem);

            // Adds `TPattern` and `TFlags` as type parameters to
            // any method signature that includes the `RegExp` as
            // a type.
            if visitor.found {
                let mut method = method.clone();

                match &mut method.type_params {
                    Some(type_params) => {
                        type_params.push(TypeParam {
                            name: "TPattern".to_string(),
                            constraint: None,
                            default: None,
                        });
                        type_params.push(TypeParam {
                            name: "TFlags".to_string(),
                            constraint: None,
                            default: None,
                        });
                    }
                    None => {
                        method.type_params = Some(vec![
                            TypeParam {
                                name: "TPattern".to_string(),
                                constraint: None,
                                default: None,
                            },
                            TypeParam {
                                name: "TFlags".to_string(),
                                constraint: None,
                                default: None,
                            },
                        ])
                    }
                }

                // Replaces any `RegExp`, `RegExpExecArray`, and
                // `RegExpMatchArray` types with versions that are
                // passed `TPattern` and `TFlags` as type arguments.
                let mut elem = TObjElem::Method(method);
                let mut regex_visitor = RegExpVisitor {};
                regex_visitor.visit_obj_elem(&mut elem);

                return Some(elem);
            }
        }
    }

    None
}

struct HasTypeRefVisitor {
    name: String,
    found: bool,
}

impl HasTypeRefVisitor {
    fn new(name: &str) -> Self {
        HasTypeRefVisitor {
            name: name.to_owned(),
            found: false,
        }
    }
}

impl Visitor for HasTypeRefVisitor {
    fn visit_type(&mut self, t: &Type) {
        if self.found {
            return;
        }

        match &t.kind {
            TypeKind::Ref(alias) if alias.name == self.name => {
                self.found = true;
            }
            _ => (),
        }
    }
}

struct RegExpVisitor {}

impl VisitorMut for RegExpVisitor {
    fn visit_type(&mut self, t: &mut Type) {
        match &t.kind {
            TypeKind::Ref(alias)
                if (alias.name == "RegExp"
                    || alias.name == "RegExpMatchArray"
                    || alias.name == "RegExpExecArray") =>
            {
                t.kind = TypeKind::Ref(TRef {
                    name: alias.name.to_owned(),
                    type_args: Some(vec![
                        Type::from(TypeKind::Ref(TRef {
                            name: "TPattern".to_string(),
                            type_args: None,
                        })),
                        Type::from(TypeKind::Ref(TRef {
                            name: "TFlags".to_string(),
                            type_args: None,
                        })),
                    ]),
                })
            }
            _ => (),
        }
    }
}
