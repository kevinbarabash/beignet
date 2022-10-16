use crochet_ast::*;
use crochet_types::{self as types, TFnParam, TKeyword, TPat, Type};

use crate::assump::Assump;
use crate::context::Context;
use crate::infer_pattern::infer_pattern;
use crate::substitutable::Subst;

// NOTE: The caller is responsible for inserting any new variables introduced
// into the appropriate context.
pub fn infer_fn_param(
    param: &mut EFnParam,
    ctx: &mut Context,
    type_param_map: &Assump,
) -> Result<(Subst, Assump, TFnParam), String> {
    // Keeps track of all of the variables the need to be introduced by this pattern.
    // let mut new_vars: Assump = Assump::default();

    let (ps, mut pa, pt) = infer_pattern(&mut param.pat, &mut param.type_ann, ctx, type_param_map)?;

    // TypeScript annotates rest params using an array type so we do the
    // same thing by converting top-level rest types to array types.
    let pt = if let Type::Rest(arg) = pt {
        Type::Array(arg)
    } else {
        pt
    };

    if param.optional {
        match pa.iter().find(|(_, value)| pt == **value) {
            Some((name, t)) => {
                let t = Type::Union(vec![t.to_owned(), Type::Keyword(TKeyword::Undefined)]);
                pa.insert(name.to_owned(), t);
            }
            None => (),
        };
    }

    let param = TFnParam {
        pat: pattern_to_tpat(&param.pat),
        t: pt,
        optional: param.optional,
    };

    Ok((ps, pa, param))
}

pub fn pattern_to_tpat(pattern: &Pattern) -> TPat {
    match &pattern.kind {
        PatternKind::Ident(e_bi) => TPat::Ident(types::BindingIdent {
            name: e_bi.id.name.to_owned(),
            mutable: false,
        }),
        PatternKind::Rest(e_rest) => TPat::Rest(types::RestPat {
            arg: Box::from(pattern_to_tpat(e_rest.arg.as_ref())),
        }),
        PatternKind::Object(e_obj) => {
            // TODO: replace TProp with the type equivalent of EFnParamObjectPatProp
            let props: Vec<types::TObjectPatProp> = e_obj
                .props
                .iter()
                .map(|e_prop| {
                    match e_prop {
                        ObjectPatProp::KeyValue(kv) => {
                            types::TObjectPatProp::KeyValue(types::TObjectKeyValuePatProp {
                                key: kv.key.name.to_owned(),
                                value: pattern_to_tpat(&kv.value),
                            })
                        }
                        ObjectPatProp::Assign(assign) => {
                            types::TObjectPatProp::Assign(types::TObjectAssignPatProp {
                                key: assign.key.name.to_owned(),
                                // TODO: figure when/how to set this to a non-None value
                                value: None,
                            })
                        }
                        ObjectPatProp::Rest(rest) => types::TObjectPatProp::Rest(types::RestPat {
                            arg: Box::from(pattern_to_tpat(rest.arg.as_ref())),
                        }),
                    }
                })
                .collect();
            TPat::Object(types::TObjectPat { props })
        }
        PatternKind::Array(e_array) => {
            TPat::Array(types::ArrayPat {
                // TODO: fill in gaps in array patterns with types from the corresponding
                // type annotation if one exists.
                elems: e_array
                    .elems
                    .iter()
                    .map(|elem| elem.as_ref().map(pattern_to_tpat))
                    .collect(),
            })
        }
        PatternKind::Lit(_) => panic!("Literal patterns not allowed in function params"),
        PatternKind::Is(_) => panic!("'is' patterns not allowed in function params"),
        PatternKind::Wildcard(_) => panic!("Wildcard patterns not allowed in function params"),
    }
}
