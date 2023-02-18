use derive_visitor::{Drive, DriveMut, Visitor, VisitorMut};
use swc_ecma_ast::TsInterfaceDecl;

use escalier_ast::types::*;

pub fn maybe_override_string_methods(decl: &TsInterfaceDecl, elem: &TObjElem) -> Option<TObjElem> {
    if decl.id.sym.to_string() == "String" {
        if let TObjElem::Method(method) = &elem {
            let mut visitor = HasTypeRefVisitor::new("RegExp");
            elem.drive(&mut visitor);

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
                elem.drive_mut(&mut regex_visitor);

                return Some(elem);
            }
        }
    }

    None
}

#[derive(Visitor)]
#[visitor(Type(enter))]
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
    fn enter_type(&mut self, t: &Type) {
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

#[derive(VisitorMut)]
#[visitor(Type(enter))]
struct RegExpVisitor {}

impl RegExpVisitor {
    fn enter_type(&mut self, t: &mut Type) {
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
