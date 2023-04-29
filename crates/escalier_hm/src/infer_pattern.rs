use generational_arena::{Arena, Index};
use std::collections::HashMap;

use crate::ast::*;
use crate::context::{get_type, Context};
use crate::errors::*;
use crate::types::{self, *};

#[derive(Clone, Debug)]
pub struct Binding {
    pub mutable: bool,
    pub t: Index,
}

type Assump = HashMap<String, Binding>;

pub fn infer_pattern(
    arena: &mut Arena<Type>,
    pattern: &mut Pattern,
    ctx: &Context,
) -> Result<(Assump, Index), Errors> {
    fn infer_pattern_rec(
        arena: &mut Arena<Type>,
        pattern: &mut Pattern,
        assump: &mut Assump,
        ctx: &Context,
    ) -> Result<Index, Errors> {
        let t = match &mut pattern.kind {
            PatternKind::Ident(BindingIdent { name, mutable, .. }) => {
                let t = new_var_type(arena);
                if assump
                    .insert(
                        name.to_owned(),
                        Binding {
                            mutable: *mutable,
                            t,
                        },
                    )
                    .is_some()
                {
                    return Err(Errors::InferenceError(
                        "Duplicate identifier in pattern".to_string(),
                    ));
                }
                t
            }
            PatternKind::Rest(_) => todo!(),
            PatternKind::Object(ObjectPat { props, .. }) => {
                let mut rest_opt_ty: Option<Index> = None;
                let mut elems: Vec<types::TObjElem> = vec![];

                for prop in props.iter_mut() {
                    match prop {
                        // re-assignment, e.g. {x: new_x, y: new_y} = point
                        ObjectPatProp::KeyValue(KeyValuePatProp { key, value, .. }) => {
                            // We ignore `init` for now, we can come back later to handle
                            // default values.
                            // TODO: handle default values

                            // TODO: bubble the error up from infer_patter_rec() if there is one.
                            let value_type = infer_pattern_rec(arena, value.as_mut(), assump, ctx)?;

                            elems.push(types::TObjElem::Prop(types::TProp {
                                name: TPropKey::StringKey(key.name.to_owned()),
                                optional: false,
                                mutable: false,
                                t: value_type,
                            }))
                        }
                        ObjectPatProp::Shorthand(ShorthandPatProp { ident, .. }) => {
                            // We ignore `init` for now, we can come back later to handle
                            // default values.
                            // TODO: handle default values

                            let t = new_var_type(arena);
                            if assump
                                .insert(ident.name.to_owned(), Binding { mutable: false, t })
                                .is_some()
                            {
                                todo!("return an error");
                            }

                            elems.push(types::TObjElem::Prop(types::TProp {
                                name: TPropKey::StringKey(ident.name.to_owned()),
                                optional: false,
                                mutable: false,
                                t,
                            }))
                        }
                        ObjectPatProp::Rest(rest) => {
                            if rest_opt_ty.is_some() {
                                // TODO: return an Err() instead of panicking.
                                panic!("Maximum one rest pattern allowed in object patterns")
                            }
                            // TypeScript doesn't support spreading/rest in types so instead we
                            // do the following conversion:
                            // {x, y, ...rest} -> {x: A, y: B} & C
                            // TODO: bubble the error up from infer_patter_rec() if there is one.
                            rest_opt_ty =
                                Some(infer_pattern_rec(arena, &mut rest.arg, assump, ctx)?);
                        }
                    }
                }

                let obj_type = new_object_type(arena, &elems);
                // let obj_type = self.from_type_kind(TypeKind::Object(TObject {
                //     elems,
                //     is_interface: false,
                // }));

                match rest_opt_ty {
                    // TODO: Replace this with a proper Rest/Spread type
                    // See https://github.com/microsoft/TypeScript/issues/10727
                    Some(rest_ty) => new_intersection_type(arena, &[obj_type, rest_ty]),
                    None => obj_type,
                }
            }
            PatternKind::Tuple(_) => todo!(),
            PatternKind::Lit(_) => todo!(),
            PatternKind::Is(IsPat { ident, is_id }) => {
                let t = match is_id.name.as_str() {
                    "number" => new_constructor(arena, "number", &[]),
                    "string" => new_constructor(arena, "string", &[]),
                    "boolean" => new_constructor(arena, "boolean", &[]),
                    name => get_type(arena, name, ctx)?,
                };

                assump.insert(ident.name.to_owned(), Binding { t, mutable: false });

                t
            }
            PatternKind::Wildcard => todo!(),
        };

        Ok(t)
    }

    let mut assump = Assump::default();
    let pat_type = infer_pattern_rec(arena, pattern, &mut assump, ctx)?;

    Ok((assump, pat_type))
}
