use crate::ast::syntax::{BindingIdent, Expr, Fix, Lambda};
use crate::ts::ast::{Param, TsObjProp, TsQualifiedType, TsType, Func, Alias};
use crate::types::{Scheme, TLam, Type, TypeKind};

/// Converts a Scheme to a TsQualifiedTyepe for eventual export to .d.ts.
pub fn convert_scheme(scheme: &Scheme, expr: Option<&Expr>) -> TsQualifiedType {
    TsQualifiedType {
        ty: convert_type(&scheme.ty, expr),
        type_params: scheme.qualifiers.clone(),
    }
}

/// Converts an internal Type to a TsType for eventual export to .d.ts.
///
/// `expr` should be the original expression that `ty` was inferred
/// from if it exists.
pub fn convert_type(ty: &Type, expr: Option<&Expr>) -> TsType {
    match &ty.kind {
        TypeKind::Var => {
            let chars: Vec<_> = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
                .chars()
                .collect();
            let id = chars.get(ty.id.to_owned() as usize).unwrap();
            TsType::Var(format!("{id}"))
        }
        TypeKind::Prim(prim) => TsType::Prim(prim.to_owned()),
        TypeKind::Lit(lit) => TsType::Lit(lit.to_owned()),
        // This is used to copy the names of args from the expression
        // over to the lambda's type.
        TypeKind::Lam(TLam { args, ret }) => {
            match expr {
                // TODO: handle is_async
                Some(Expr::Lambda(Lambda {
                    args: expr_args, ..
                })) => {
                    if args.len() != expr_args.len() {
                        panic!("number of args don't match")
                    } else {
                        let params: Vec<_> = args
                            .iter()
                            .zip(expr_args)
                            .map(|(arg, binding)| {
                                let name = match binding {
                                    BindingIdent::Ident(ident) => &ident.name,
                                    BindingIdent::Rest { name, .. } => name,
                                };
                                Param {
                                    name: name.to_owned(),
                                    ty: convert_type(arg, None),
                                }
                            })
                            .collect();

                        TsType::Func(Func {
                            params,
                            ret: Box::new(convert_type(&ret, None)),
                        })
                    }
                }
                // Fix nodes are assumed to wrap a lambda where the body of
                // the lambda is recursive function.
                Some(Expr::Fix(Fix { expr, .. })) => match expr.as_ref() {
                    Expr::Lambda(Lambda { body, .. }) => convert_type(ty, Some(&body)),
                    _ => panic!("mismatch"),
                },
                None => {
                    let params: Vec<_> = args
                        .iter()
                        .enumerate()
                        .map(|(i, arg)| Param {
                            name: format!("arg{}", i),
                            ty: convert_type(arg, None),
                        })
                        .collect();
                    TsType::Func(Func {
                        params,
                        ret: Box::new(convert_type(&ret, None)),
                    })
                }
                _ => panic!("mismatch"),
            }
        }
        TypeKind::Union(types) => {
            TsType::Union(types.iter().map(|ty| convert_type(ty, None)).collect())
        }
        TypeKind::Obj(props) => {
            let props: Vec<_> = props
                .iter()
                .map(|prop| TsObjProp {
                    name: prop.name.clone(),
                    ty: convert_type(&prop.ty, None),
                })
                .collect();
            TsType::Obj(props)
        }
        TypeKind::Alias { name, type_params } => {
            let type_params: Vec<_> = type_params
                .iter()
                .map(|ty| convert_type(ty, None))
                .collect();
            TsType::Alias(Alias {
                name: name.to_owned(),
                type_params,
            })
        }
    }
}
