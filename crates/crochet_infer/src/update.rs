use crochet_ast::*;
use crochet_types::Type;

use super::substitutable::{Subst, Substitutable};

pub fn update_program(prog: &mut Program, s: &Subst) {
    for stmt in prog.body.iter_mut() {
        match stmt {
            Statement::VarDecl {
                span: _,
                pattern,
                type_ann,
                init,
                declare: _,
            } => {
                update_pattern(pattern, s);
                if let Some(type_ann) = type_ann {
                    update_type_ann(type_ann, s);
                }
                if let Some(init) = init.as_mut() {
                    update_expr(init, s);
                }
            }
            Statement::TypeDecl {
                span: _,
                declare: _,
                id: _,
                type_ann,
                type_params: _,
            } => {
                update_type_ann(type_ann, s);
            }
            Statement::Expr { span: _, expr } => {
                update_expr(expr, s);
            }
        }
    }
}

fn update_pattern(pattern: &mut Pattern, s: &Subst) {
    // Since we process the node first, if `expr` is a leaf node we
    // ignore it in the match statement below.
    if let Some(t) = &pattern.inferred_type {
        if let Type::Var(_) = t {
            pattern.inferred_type = Some(t.apply(s));
        }
    }

    match &mut pattern.kind {
        PatternKind::Ident(_) => (), // leaf node
        PatternKind::Rest(RestPat { arg }) => {
            update_pattern(arg, s);
        }
        PatternKind::Object(ObjectPat { props, optional: _ }) => {
            props.iter_mut().for_each(|prop| match prop {
                ObjectPatProp::KeyValue(KeyValuePatProp {
                    key: _, // TODO: figure out how to attach inferred_types to idents
                    value,
                }) => {
                    update_pattern(value, s);
                }
                ObjectPatProp::Rest(RestPat { arg }) => {
                    update_pattern(arg, s);
                }
                ObjectPatProp::Assign(AssignPatProp {
                    span: _,
                    key: _, // TODO: figure out how to attach inferred_types to idents
                    value,
                }) => {
                    if let Some(value) = value {
                        update_expr(value, s)
                    }
                }
            })
            // TODO: process `props`
        }
        PatternKind::Array(ArrayPat { elems, optional: _ }) => {
            elems.iter_mut().for_each(|elem| match elem {
                Some(pat) => update_pattern(pat, s),
                None => (),
            })
        }
        PatternKind::Lit(_) => (), // leaf node
        // TODO: figure out how to attach inferred_types to idents
        PatternKind::Is(IsPat { id: _, is_id: _ }) => (),
        PatternKind::Wildcard(_) => (), // leaf node (also has no binding)
    }
}

fn update_expr(expr: &mut Expr, s: &Subst) {
    // Since we process the node first, if `expr` is a leaf node we
    // ignore it in the match statement below.
    if let Some(t) = &expr.inferred_type {
        if let Type::Var(_) = t {
            expr.inferred_type = Some(t.apply(s));
        }
    }

    match &mut expr.kind {
        ExprKind::App(App { lam, args }) => {
            update_expr(lam, s);
            args.iter_mut().for_each(|arg_or_spread| {
                // TODO: rework args to be less awkward
                update_expr(&mut arg_or_spread.expr, s);
            });
        }
        ExprKind::Fix(Fix { expr }) => {
            update_expr(expr, s);
        }
        ExprKind::Ident(_) => (), // leaf node
        ExprKind::IfElse(IfElse {
            cond,
            consequent,
            alternate,
        }) => {
            update_expr(cond, s);
            update_expr(consequent, s);
            if let Some(alternate) = alternate {
                update_expr(alternate, s);
            }
        }
        ExprKind::JSXElement(JSXElement {
            span: _,
            name: _,
            attrs,
            children,
        }) => {
            attrs.iter_mut().for_each(|attr| {
                match &mut attr.value {
                    JSXAttrValue::Lit(_) => (), // leaf node
                    JSXAttrValue::JSXExprContainer(JSXExprContainer { expr, span: _ }) => {
                        update_expr(expr, s);
                    }
                }
            });
            children.iter_mut().for_each(|child| {
                update_jsx_element_child(child, s);
            })
        }
        ExprKind::Lambda(Lambda {
            params,
            body,
            is_async: _,
            return_type: _,
            type_params: _,
        }) => {
            params.iter_mut().for_each(|param| {
                update_fn_param_pat(&mut param.pat, s);
            });
            update_expr(body, s);
        }
        ExprKind::Let(Let {
            pattern,
            type_ann,
            init,
            body,
        }) => {
            if let Some(pattern) = pattern {
                update_pattern(pattern, s);
            }
            if let Some(type_ann) = type_ann {
                update_type_ann(type_ann, s);
            }
            update_expr(init, s);
            update_expr(body, s);
        }
        ExprKind::LetExpr(LetExpr { pat, expr }) => {
            update_pattern(pat, s);
            update_expr(expr, s);
        }
        ExprKind::Lit(_) => (), // leaf node
        ExprKind::BinaryExpr(BinaryExpr { left, right, op: _ }) => {
            update_expr(left, s);
            update_expr(right, s);
        }
        ExprKind::UnaryExpr(UnaryExpr { arg, op: _ }) => {
            update_expr(arg, s);
        }
        ExprKind::Obj(Obj { props }) => {
            // TODO: handle props
            props
                .iter_mut()
                .for_each(|prop_or_spread| match prop_or_spread {
                    PropOrSpread::Spread(spread) => {
                        update_expr(&mut spread.expr, s);
                    }
                    // TODO: figure out how to attach an inferred_type to props
                    PropOrSpread::Prop(prop) => match prop.as_mut() {
                        Prop::Shorthand(_) => (),
                        Prop::KeyValue(_) => (),
                    },
                });
        }
        ExprKind::Await(Await { expr }) => update_expr(expr, s),
        ExprKind::Tuple(Tuple { elems }) => {
            elems
                .iter_mut()
                .for_each(|elem| update_expr(&mut elem.expr, s));
        }
        ExprKind::Member(Member { obj, prop }) => {
            update_expr(obj, s);
            update_member_prop(prop, s);
        }
        ExprKind::Empty => (), // leaf node
        ExprKind::TemplateLiteral(TemplateLiteral { exprs, quasis: _ }) => {
            // NOTE: we don't bother with quasis because they're just another
            // flavor of string literal.
            exprs.iter_mut().for_each(|expr| {
                update_expr(expr, s);
            });
        }
        ExprKind::TaggedTemplateLiteral(_) => (),
        ExprKind::Match(Match { expr, arms }) => {
            update_expr(expr, s);
            arms.iter_mut().for_each(|arm| {
                let Arm {
                    span: _,
                    pattern,
                    guard,
                    body,
                } = arm;
                update_pattern(pattern, s);
                if let Some(guard) = guard {
                    update_expr(guard, s);
                }
                update_expr(body, s);
            })
        }
    }
}

// TODO: Unify EFnParamPat with Pattern
// If we're worried about generated blocks of AST being invalid, we can have a validate
// function to detect the issue.  Trying to enforce all restrictions at the type level
// can result in near duplicate code which is a worse problem to deal with.
fn update_fn_param_pat(pat: &mut EFnParamPat, s: &Subst) {
    match pat {
        EFnParamPat::Ident(_) => (), // leaf node
        EFnParamPat::Rest(EFnParamRestPat { span: _, arg }) => update_fn_param_pat(arg, s),
        EFnParamPat::Object(EFnParamObjectPat { span: _, props }) => {
            props.iter_mut().for_each(|prop| match prop {
                // TODO: figure out how to attach inferred_types to idents
                EFnParamObjectPatProp::KeyValue(EFnParamKeyValuePatProp { key: _, value }) => {
                    update_fn_param_pat(value, s);
                }
                EFnParamObjectPatProp::Assign(EFnParamAssignPatProp { key: _, value }) => {
                    if let Some(value) = value {
                        update_expr(value, s);
                    }
                }
                EFnParamObjectPatProp::Rest(EFnParamRestPat { span: _, arg }) => {
                    update_fn_param_pat(arg, s);
                }
            });
        }
        EFnParamPat::Array(EFnParamArrayPat { span: _, elems }) => {
            elems.iter_mut().for_each(|elem| {
                if let Some(elem) = elem {
                    update_fn_param_pat(elem, s);
                }
            })
        }
    }
}

fn update_type_ann(type_ann: &mut TypeAnn, s: &Subst) {
    // Since we process the node first, if `expr` is a leaf node we
    // ignore it in the match statement below.
    if let Some(t) = &type_ann.inferred_type {
        if let Type::Var(_) = t {
            type_ann.inferred_type = Some(t.apply(s));
        }
    }

    match &mut type_ann.kind {
        TypeAnnKind::Lam(LamType {
            span: _,
            params,
            ret,
            type_params,
        }) => {
            params
                .iter_mut()
                .for_each(|param| update_fn_param_pat(&mut param.pat, s));
            update_type_ann(ret, s);
            if let Some(type_params) = type_params {
                type_params.iter_mut().for_each(|type_param| {
                    if let Some(constraint) = &mut type_param.constraint {
                        update_type_ann(constraint, s);
                    }
                    if let Some(default) = &mut type_param.default {
                        update_type_ann(default, s);
                    }
                });
            }
        }
        TypeAnnKind::Lit(_) => (), // leaf node
        // We should be able to fill some of these correctly as part of infer_type_ann()
        TypeAnnKind::Keyword(_) => (), // leaf node
        TypeAnnKind::Object(ObjectType { span: _, elems }) => {
            elems.iter_mut().for_each(|elem| match elem {
                TObjElem::Index(TIndex { type_ann, .. }) => {
                    update_type_ann(type_ann, s);
                }
                TObjElem::Prop(TProp { type_ann, .. }) => {
                    update_type_ann(type_ann, s);
                }
            })
        }
        TypeAnnKind::TypeRef(_) => (),
        TypeAnnKind::Union(UnionType { span: _, types }) => {
            types.iter_mut().for_each(|t| update_type_ann(t, s));
        }
        TypeAnnKind::Intersection(IntersectionType { span: _, types }) => {
            types.iter_mut().for_each(|t| update_type_ann(t, s));
        }
        TypeAnnKind::Tuple(TupleType { span: _, types }) => {
            types.iter_mut().for_each(|t| update_type_ann(t, s));
        }
        TypeAnnKind::Array(ArrayType { span: _, elem_type }) => {
            update_type_ann(elem_type, s);
        }
        TypeAnnKind::KeyOf(KeyOfType { span: _, type_ann }) => {
            update_type_ann(type_ann, s);
        }
        TypeAnnKind::Query(QueryType { span: _, expr }) => {
            update_expr(expr, s);
        }
        TypeAnnKind::IndexedAccess(IndexedAccessType {
            span: _,
            obj_type,
            index_type,
        }) => {
            update_type_ann(obj_type, s);
            update_type_ann(index_type, s);
        }
    }
}

fn update_member_prop(prop: &mut MemberProp, s: &Subst) {
    match prop {
        // TODO: figure out how to attach inferred_types to idents
        MemberProp::Ident(Ident { span: _, name: _ }) => (),
        MemberProp::Computed(ComputedPropName { span: _, expr }) => update_expr(expr, s),
    }
}

fn update_jsx_element_child(jsx_elem: &mut JSXElementChild, s: &Subst) {
    match jsx_elem {
        JSXElementChild::JSXText(_) => (), // leaf node (type is always string)
        JSXElementChild::JSXExprContainer(JSXExprContainer { span: _, expr }) => {
            update_expr(expr, s);
        }
        JSXElementChild::JSXElement(jsx_elem) => {
            let jsx_elem = jsx_elem.as_mut();
            jsx_elem
                .attrs
                .iter_mut()
                .for_each(|attr| match &mut attr.value {
                    JSXAttrValue::Lit(_) => (), // leaf node (type is just the value itself)
                    JSXAttrValue::JSXExprContainer(JSXExprContainer { span: _, expr }) => {
                        update_expr(expr, s);
                    }
                });
            jsx_elem.children.iter_mut().for_each(|child| {
                update_jsx_element_child(child, s);
            });
        }
    }
}
