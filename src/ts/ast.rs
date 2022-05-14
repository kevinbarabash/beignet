use itertools::{join, Itertools};
use std::fmt;

use super::super::literal::Literal;
use super::super::types::Primitive;

pub struct TsQualifiedType {
    pub ty: TsType,
    pub type_params: Vec<i32>,
}

#[derive(Debug)]
pub enum TsType {
    Prim(Primitive),
    Var(String),
    Lit(Literal),
    Func {
        params: Vec<Param>,
        ret: Box<TsType>,
    },
    Union(Vec<TsType>),
    Obj(Vec<TsObjProp>),
    Alias {
        name: String,
        type_params: Vec<TsType>,
    },
}

impl fmt::Display for TsQualifiedType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let chars: Vec<_> = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
            .chars()
            .collect();

        if self.type_params.len() > 0 {
            let type_params = self
                .type_params
                .iter()
                .map(|id| {
                    let id = chars.get(id.to_owned() as usize).unwrap();
                    format!("{id}")
                })
                .join(", ");
            write!(f, "<{type_params}>{}", self.ty)
        } else {
            write!(f, "{}", self.ty)
        }
    }
}

impl fmt::Display for TsType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TsType::Var(name) => write!(f, "{}", name),
            TsType::Prim(prim) => write!(f, "{}", prim),
            TsType::Lit(lit) => write!(f, "{}", lit),
            TsType::Func { params, ret } => {
                let params = params
                    .iter()
                    // TODO: use write! to format the params more directly instead of
                    // using the intermediary format!
                    .map(|Param { name, ty }| format!("{name}: {ty}").to_owned())
                    .join(", ");
                write!(f, "({params}) => {ret}")
            }
            TsType::Union(types) => write!(f, "{}", join(types, " | ")),
            TsType::Obj(props) => {
                let props = props
                    .iter()
                    .map(|TsObjProp { name, ty }| format!("{name}: {ty}"))
                    .join(", ");

                // TODO: output multi-line object types
                write!(f, "{{{props}}}")
            }
            TsType::Alias {name, type_params} => {
                let type_params = type_params.iter().join(", ");
                write!(f, "{name}<{type_params}>")
            }
        }
    }
}

#[derive(Debug)]
pub struct Param {
    pub name: String,
    pub ty: TsType,
}

#[derive(Debug)]
pub struct TsObjProp {
    pub name: String,
    pub ty: TsType,
}
