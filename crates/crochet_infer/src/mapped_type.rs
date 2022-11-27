use crochet_ast::types::{
    BindingIdent, TFnParam, TIndex, TMappedType, TObjElem, TObject, TPat, TProp, Type, TypeKind,
};
use error_stack::{Report, Result};

use crate::context::Context;
use crate::type_error::TypeError;
use crate::visitor::Visitor;

pub fn compute_mapped_type(mapped: &TMappedType, ctx: &Context) -> Result<Type, TypeError> {
    // TODO:
    // - compute the keys to map over
    // - compute the value of each key (may require computing indexed access)
    // - create a new object type from those (key, value) pairs
    let constraint = mapped.type_param.constraint.as_ref().unwrap();

    // keys is assumed to be union type of indexers
    if let TypeKind::Union(keys) = &constraint.kind {
        let elems = keys
            .iter()
            .map(|key| {
                let mut value = mapped.t.clone();

                // TODO: check why kind of type `value` is, if it's a indexed
                // type, we can get its object type.

                // if key is a:
                // - number, string, or symbol then create an indexer
                // - literal of those types then create a normal property
                replace_tvar(&mut value, &mapped.type_param.id, key);

                match &key.kind {
                    TypeKind::Lit(lit) => match lit {
                        crochet_ast::types::TLit::Num(num) => Ok(TObjElem::Prop(TProp {
                            name: num.to_owned(),
                            // How do we maintain the optionality of each property
                            // when we aren't setting it explicitly
                            optional: false, // TODO
                            mutable: false,  // TODO
                            t: value.as_ref().to_owned(),
                        })),
                        crochet_ast::types::TLit::Bool(_) => Err(Report::new(TypeError::Unhandled)),
                        crochet_ast::types::TLit::Str(str) => Ok(TObjElem::Prop(TProp {
                            name: str.to_owned(),
                            // How do we maintain the optionality of each property
                            // when we aren't setting it explicitly
                            optional: false, // TODO
                            mutable: false,  // TODO
                            t: value.as_ref().to_owned(),
                        })),
                    },
                    TypeKind::Keyword(_) => Ok(TObjElem::Index(TIndex {
                        key: TFnParam {
                            pat: TPat::Ident(BindingIdent {
                                name: String::from("key"),
                                mutable: false, // TODO
                            }),
                            t: key.to_owned(),
                            optional: false, // TODO
                        },
                        // How do we maintain the optionality of each property
                        // when we aren't setting it explicitly
                        mutable: false, // TODO
                        t: value.as_ref().to_owned(),
                    })),
                    _ => Err(Report::new(TypeError::Unhandled)),
                }
            })
            .collect::<Result<Vec<_>, TypeError>>()?;

        let t = Type {
            kind: TypeKind::Object(TObject { elems }),
            mutable: false,
            provenance: None, // TODO: fill this in
        };

        Ok(t)
    } else {
        Err(Report::new(TypeError::Unhandled))
    }
}

struct ReplaceVisitor {
    search_id: i32,
    rep: Type,
}

impl ReplaceVisitor {
    fn new(search_id: &i32, rep: &Type) -> Self {
        ReplaceVisitor {
            search_id: *search_id,
            rep: rep.to_owned(),
        }
    }
}

impl Visitor for ReplaceVisitor {
    fn visit_type(&mut self, t: &mut Type) {
        if let TypeKind::Var(tvar) = &t.kind {
            if tvar.id == self.search_id {
                t.kind = self.rep.kind.to_owned();
                t.mutable = self.rep.mutable;
            }
        }
    }
}

fn replace_tvar(t: &mut Type, search_id: &i32, rep: &Type) {
    let mut rep_visitor = ReplaceVisitor::new(search_id, rep);
    rep_visitor.visit_children(t);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::infer;
    use crochet_parser::*;

    fn infer_prog(input: &str) -> Context {
        let mut prog = parse(input).unwrap();
        let mut ctx: Context = Context::default();
        infer::infer_prog(&mut prog, &mut ctx).unwrap()
    }
}
