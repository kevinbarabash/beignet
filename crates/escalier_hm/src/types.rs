// Types and type constructors
use generational_arena::Index;
use std::convert::From;
use std::fmt;

// TODO: create type versions of these so that we don't have to bother
// with source locations when doing type-level stuff.
use escalier_ast::{BindingIdent, Literal as Lit};

use crate::checker::Checker;
use crate::provenance::Provenance;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeVar {
    pub id: usize,
    pub instance: Option<Index>,
    pub constraint: Option<Index>,
}

// TODO: rename this TypeRef
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeRef {
    pub name: String,
    // TODO: rename this to type_args
    pub types: Vec<Index>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Keyword {
    Never,
    Object,
    Unknown,
}

impl fmt::Display for Keyword {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let result = match self {
            Self::Never => "never",
            Self::Object => "object",
            Self::Unknown => "unknown",
        };
        write!(f, "{result}")
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Primitive {
    Number,
    Boolean,
    String,
    Symbol,
}

impl Primitive {
    pub fn get_scheme_name(&self) -> &'static str {
        match self {
            Self::Number => "Number",
            Self::Boolean => "Boolean",
            Self::String => "String",
            Self::Symbol => "Symbol",
        }
    }
}

impl fmt::Display for Primitive {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let result = match self {
            Self::Number => "number",
            Self::Boolean => "boolean",
            Self::String => "string",
            Self::Symbol => "symbol",
        };
        write!(f, "{result}")
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Function {
    pub params: Vec<FuncParam>,
    pub ret: Index,
    pub type_params: Option<Vec<TypeParam>>,
    pub throws: Option<Index>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct FuncParam {
    pub pattern: TPat,
    pub t: Index,
    pub optional: bool,
}

impl FuncParam {
    pub fn is_self(&self) -> bool {
        match &self.pattern {
            TPat::Ident(BindingIdent { name, .. }) => name == "self",
            _ => false,
        }
    }

    pub fn is_mut_self(&self) -> bool {
        match &self.pattern {
            TPat::Ident(BindingIdent { name, mutable, .. }) => name == "self" && *mutable,
            _ => false,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum TPat {
    Ident(BindingIdent),
    Rest(RestPat),
    Tuple(TuplePat),
    Object(TObjectPat),
    Lit(TLitPat),
    Is(TIsPat),
    Wildcard,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct RestPat {
    pub arg: Box<TPat>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TuplePat {
    pub elems: Vec<Option<TPat>>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TObjectPat {
    pub props: Vec<TObjectPatProp>,
}

// TODO: update this to match AST changes to ObjectPatProp
#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum TObjectPatProp {
    KeyValue(TObjectKeyValuePatProp),
    Assign(TObjectAssignPatProp),
    Rest(RestPat),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TObjectKeyValuePatProp {
    pub key: String,
    pub value: TPat,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TObjectAssignPatProp {
    pub key: String,
    pub value: Option<Index>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TLitPat {
    pub lit: Lit,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TIsPat {
    pub ident: String,
    pub is_id: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TypeParam {
    pub name: String,
    pub constraint: Option<Index>,
    pub default: Option<Index>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Call {
    pub args: Vec<Index>,
    pub ret: Index,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TMethod {
    pub name: TPropKey,
    pub params: Vec<FuncParam>,
    pub ret: Index,
    pub type_params: Option<Vec<TypeParam>>,
    pub throws: Option<Index>,
    pub mutates: bool,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TGetter {
    pub name: TPropKey,
    pub ret: Index,
    pub throws: Option<Index>,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TSetter {
    pub name: TPropKey,
    pub param: FuncParam,
    pub throws: Option<Index>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TIndexKey {
    pub name: String,
    pub t: Index,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TPropKey {
    StringKey(String),
    NumberKey(String),
}

impl fmt::Display for TPropKey {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TPropKey::StringKey(key) => write!(f, "{key}"),
            TPropKey::NumberKey(key) => write!(f, "{key}"),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TProp {
    pub name: TPropKey,
    pub optional: bool,
    pub readonly: bool,
    pub t: Index,
}

impl TProp {
    pub fn get_type(&self, checker: &mut Checker) -> Index {
        let t = self.t;

        match self.optional {
            true => {
                let undefined = checker.new_lit_type(&Lit::Undefined);
                checker.new_union_type(&[t, undefined])
            }
            false => t,
        }
    }
}

// #[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
// pub struct TCallable {
//     pub params: Vec<FuncParam>,
//     pub ret: Index,
//     pub type_params: Option<Vec<TypeParam>>,
//     pub throws: Option<Index>,
// }

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum MappedModifier {
    Add,
    Remove,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MappedType {
    pub key: Index,
    pub value: Index,
    pub target: String,
    pub source: Index,
    pub optional: Option<MappedModifier>,

    // First half of a Conditional
    pub check: Option<Index>,
    pub extends: Option<Index>,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TObjElem {
    Call(Function),
    // NOTE: type_params on constructors should be a subset of type_params on
    // the object scheme in which they live
    Constructor(Function),
    Method(TMethod),
    Getter(TGetter),
    Setter(TSetter),
    Mapped(MappedType),
    Prop(TProp),
    // RestSpread - we can use this instead of converting {a, ...x} to {a} & tvar
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Object {
    pub elems: Vec<TObjElem>,
}

// NOTE: this is only used for the rest element in array patterns since we
// treat `{a, ...x}` as `{a} & x` where `x` is a type variable.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Rest {
    pub arg: Index,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum UtilityKind {
    KeyOf,
    Index,
    Cond,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Utility {
    pub kind: UtilityKind,
    pub types: Vec<Index>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Mutable {
    pub t: Index,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Union {
    pub types: Vec<Index>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Intersection {
    pub types: Vec<Index>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Tuple {
    pub types: Vec<Index>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Array {
    pub t: Index,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct KeyOf {
    pub t: Index,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct IndexedAccess {
    pub obj: Index,
    pub index: Index,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Conditional {
    pub check: Index,
    pub extends: Index,
    pub true_type: Index,
    pub false_type: Index,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Infer {
    pub name: String,
    // TODO
    // pub constraint: Option<Index>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Wildcard {}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TBinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    // TODO: fill this out with more operators
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BinaryT {
    pub op: TBinaryOp,
    pub left: Index,
    pub right: Index,
}

#[derive(Debug, Clone, Hash)]
pub enum TypeKind {
    TypeVar(TypeVar),
    TypeRef(TypeRef),
    Union(Union),
    Intersection(Intersection),
    Array(Array),
    Tuple(Tuple),
    Keyword(Keyword),
    Primitive(Primitive),
    Literal(Lit),
    Function(Function),
    Object(Object),
    Rest(Rest), // Why is this its own type?
    KeyOf(KeyOf),
    IndexedAccess(IndexedAccess),
    Conditional(Conditional),
    Infer(Infer),
    Wildcard,
    Binary(BinaryT),
}

#[derive(Debug, Clone)]
pub struct Type {
    pub kind: TypeKind,
    pub provenance: Option<Provenance>,
}

impl From<TypeKind> for Type {
    fn from(kind: TypeKind) -> Self {
        Self {
            kind,
            provenance: None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Scheme {
    pub t: Index,
    pub type_params: Option<Vec<TypeParam>>,
}

/// A type variable standing for an arbitrary type.
///
/// All type variables have a unique id, but names are
/// only assigned lazily, when required.

impl Checker {
    pub fn print_scheme(&self, scheme: &Scheme) -> String {
        let mut result = String::default();
        if let Some(type_params) = &scheme.type_params {
            let type_params = type_params
                .iter()
                .map(|tp| match &tp.constraint {
                    Some(constraint) => {
                        format!("{}:{}", tp.name.clone(), self.print_type(constraint))
                    }
                    None => tp.name.clone(),
                })
                .collect::<Vec<_>>();
            result.push_str(&format!("<{}>", type_params.join(", ")))
        }
        result.push_str(&self.print_type(&scheme.t));
        result
    }

    // TODO: support pretty printing of types
    pub fn print_type(&self, index: &Index) -> String {
        match &self.arena[*index].kind {
            TypeKind::TypeVar(TypeVar {
                instance: Some(inst),
                ..
            }) => self.print_type(inst),
            TypeKind::TypeVar(TypeVar { id, constraint, .. }) => match constraint {
                Some(constraint) => format!("t{id}:{}", self.print_type(constraint)),
                None => format!("t{id}"),
            },
            TypeKind::Union(Union { types }) => self.print_types(types).join(" | "),
            TypeKind::Intersection(Intersection { types }) => self.print_types(types).join(" & "),
            TypeKind::Tuple(Tuple { types }) => {
                format!("[{}]", self.print_types(types).join(", "))
            }
            TypeKind::Array(Array { t }) => format!("{}[]", self.print_type(t)),
            TypeKind::TypeRef(TypeRef { name, types }) => {
                if types.is_empty() {
                    name.to_string()
                } else {
                    format!("{}<{}>", name, self.print_types(types).join(", "))
                }
            }
            TypeKind::Keyword(keyword) => keyword.to_string(),
            TypeKind::Primitive(primitive) => primitive.to_string(),
            TypeKind::Literal(lit) => lit.to_string(),
            TypeKind::Object(object) => {
                let mut fields = vec![];
                for prop in &object.elems {
                    match prop {
                        TObjElem::Getter(TGetter {
                            ret,
                            name,
                            throws: _,
                        }) => {
                            let ret_type = self.print_type(ret);
                            fields.push(format!("get {name}(self) -> {ret_type}"));
                        }
                        TObjElem::Setter(TSetter {
                            param,
                            name,
                            throws: _, // TODO
                        }) => {
                            let param = self.print_type(&param.t);
                            fields.push(format!("set {name}(mut self, {param})"))
                        }
                        TObjElem::Constructor(Function {
                            params,
                            ret,
                            type_params,
                            throws: _, // TODO
                        }) => {
                            let mut result = "new fn".to_string();
                            match type_params {
                                Some(type_params) if !type_params.is_empty() => {
                                    let type_params = type_params
                                        .iter()
                                        .map(|tp| match &tp.constraint {
                                            Some(constraint) => format!(
                                                "{}:{}",
                                                tp.name.clone(),
                                                self.print_type(constraint)
                                            ),
                                            None => tp.name.clone(),
                                        })
                                        .collect::<Vec<_>>();
                                    result.push_str(&format!("<{}>", type_params.join(", ")))
                                }
                                _ => (),
                            };
                            result.push_str(&format!(
                                "({}) -> {}",
                                self.print_params(params).join(", "),
                                self.print_type(ret)
                            ));
                            fields.push(result);
                        }
                        TObjElem::Call(Function {
                            params,
                            ret,
                            type_params,
                            throws: _, // TODO
                        }) => {
                            let mut result = "fn".to_string();
                            match type_params {
                                Some(type_params) if !type_params.is_empty() => {
                                    let type_params = type_params
                                        .iter()
                                        .map(|tp| match &tp.constraint {
                                            Some(constraint) => format!(
                                                "{}:{}",
                                                tp.name.clone(),
                                                self.print_type(constraint)
                                            ),
                                            None => tp.name.clone(),
                                        })
                                        .collect::<Vec<_>>();
                                    result.push_str(&format!("<{}>", type_params.join(", ")))
                                }
                                _ => (),
                            };
                            result.push_str(&format!(
                                "({}) -> {}",
                                self.print_params(params).join(", "),
                                self.print_type(ret)
                            ));
                            fields.push(result);
                        }
                        TObjElem::Mapped(MappedType {
                            key,
                            value,
                            target,
                            source,
                            optional: _, // TODO
                            // TODO: handle `if`-clause
                            check: _,
                            extends: _,
                        }) => {
                            let key = self.print_type(key);
                            let value = self.print_type(value);
                            let source = self.print_type(source);

                            let result = format!("[{key}]: {value} for {target} in {source}",);
                            fields.push(result);
                        }
                        TObjElem::Method(TMethod {
                            name,
                            type_params,
                            params,
                            ret,
                            throws,
                            mutates,
                        }) => {
                            let name = match name {
                                TPropKey::StringKey(s) => s,
                                TPropKey::NumberKey(n) => n,
                            };
                            let type_params = match type_params {
                                Some(type_params) if !type_params.is_empty() => {
                                    let type_params = type_params
                                        .iter()
                                        .map(|tp| match &tp.constraint {
                                            Some(constraint) => {
                                                format!(
                                                    "{}:{}",
                                                    tp.name.clone(),
                                                    self.print_type(constraint)
                                                )
                                            }
                                            None => tp.name.clone(),
                                        })
                                        .collect::<Vec<_>>();
                                    format!("<{}>", type_params.join(", "))
                                }
                                _ => "".to_string(),
                            };

                            let throws = match throws {
                                Some(throws) => format!(" throws {}", self.print_type(throws)),
                                None => "".to_string(),
                            };

                            let mut params = self.print_params(params);
                            match mutates {
                                true => params.insert(0, "mut self".to_string()),
                                false => params.insert(0, "self".to_string()),
                            }
                            let params = params.join(", ");

                            let ret = self.print_type(ret);
                            let field = format!("{name}{type_params}({params}) -> {ret}{throws}",);
                            fields.push(field);
                        }
                        TObjElem::Prop(TProp {
                            name,
                            optional,
                            readonly,
                            t,
                        }) => {
                            let name = match name {
                                TPropKey::StringKey(s) => s,
                                TPropKey::NumberKey(n) => n,
                            };
                            let t = self.print_type(t);
                            let mut str = "".to_string();
                            if *readonly {
                                str += "readonly ";
                            }

                            str += name;
                            if *optional {
                                str += "?";
                            }
                            str += &format!(": {t}");

                            fields.push(str);
                        }
                    }
                }
                format!("{{{}}}", fields.join(", "))
            }
            TypeKind::Rest(rest) => {
                format!("...{}", self.print_type(&rest.arg))
            }
            TypeKind::Function(func) => {
                let type_params = match &func.type_params {
                    Some(type_params) if !type_params.is_empty() => {
                        let type_params = type_params
                            .iter()
                            .map(|tp| match &tp.constraint {
                                Some(constraint) => {
                                    format!("{}:{}", tp.name.clone(), self.print_type(constraint))
                                }
                                None => tp.name.clone(),
                            })
                            .collect::<Vec<_>>();
                        format!("<{}>", type_params.join(", "))
                    }
                    _ => "".to_string(),
                };
                let throws = match func.throws {
                    Some(throws) => format!(" throws {}", self.print_type(&throws)),
                    None => "".to_string(),
                };
                format!(
                    "{type_params}({}) -> {}{throws}",
                    self.print_params(&func.params).join(", "),
                    self.print_type(&func.ret),
                )
            }
            TypeKind::KeyOf(KeyOf { t }) => format!("keyof {}", self.print_type(t)),
            TypeKind::IndexedAccess(IndexedAccess { obj, index }) => {
                format!("{}[{}]", self.print_type(obj), self.print_type(index))
            }
            TypeKind::Conditional(Conditional {
                check,
                extends,
                true_type,
                false_type,
            }) => {
                format!(
                    "{} extends {} ? {} : {}",
                    self.print_type(check),
                    self.print_type(extends),
                    self.print_type(true_type),
                    self.print_type(false_type),
                )
            }
            TypeKind::Infer(Infer { name }) => format!("infer {}", name),
            TypeKind::Wildcard => "_".to_string(),
            TypeKind::Binary(BinaryT { op, left, right }) => {
                let op = match op {
                    TBinaryOp::Add => "+",
                    TBinaryOp::Sub => "-",
                    TBinaryOp::Mul => "*",
                    TBinaryOp::Div => "/",
                    TBinaryOp::Mod => "%",
                };
                format!(
                    "{} {} {}",
                    self.print_type(left),
                    op,
                    self.print_type(right),
                )
            }
        }
    }

    fn print_types(&self, indexes: &[Index]) -> Vec<String> {
        let mut result = vec![];
        for index in indexes {
            result.push(self.print_type(index));
        }
        result
    }

    fn print_params(&self, params: &[FuncParam]) -> Vec<String> {
        let mut strings = vec![];
        for param in params {
            strings.push(self.print_param(param))
        }
        strings
    }

    fn print_param(&self, param: &FuncParam) -> String {
        let name = Self::tpat_to_string(&param.pattern);
        match param.optional {
            true => format!("{name}?: {}", self.print_type(&param.t)),
            false => format!("{name}: {}", self.print_type(&param.t)),
        }
    }

    fn tpat_to_string(pattern: &TPat) -> String {
        match pattern {
            TPat::Ident(BindingIdent { name, mutable, .. }) => match mutable {
                true => format!("mut {}", name),
                false => name.to_owned(),
            },
            TPat::Rest(RestPat { arg }) => format!("...{}", Self::tpat_to_string(arg.as_ref())),
            TPat::Tuple(TuplePat { elems }) => format!(
                "[{}]",
                elems
                    .iter()
                    .map(|elem| match elem {
                        Some(elem) => Self::tpat_to_string(elem),
                        None => " ".to_string(),
                    })
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            TPat::Object(TObjectPat { props }) => {
                let props: Vec<String> = props
                    .iter()
                    .map(|prop| match prop {
                        TObjectPatProp::KeyValue(TObjectKeyValuePatProp { key, value }) => {
                            match value {
                                TPat::Ident(_) => key.to_string(),
                                _ => format!("{}: {}", key, Self::tpat_to_string(value)),
                            }
                        }
                        // TODO: handle assignments in object patterns
                        TObjectPatProp::Assign(TObjectAssignPatProp { key, value: _ }) => {
                            key.to_string()
                        }
                        TObjectPatProp::Rest(RestPat { arg }) => {
                            format!("...{}", Self::tpat_to_string(arg.as_ref()))
                        }
                    })
                    .collect();
                format!("{{{}}}", props.join(", "))
            }
            TPat::Lit(TLitPat { lit }) => lit.to_string(),
            TPat::Is(TIsPat { ident, is_id }) => {
                format!("{ident} is {is_id}")
            }
            TPat::Wildcard => "_".to_string(),
        }
    }

    pub fn equals(&self, a: &Index, b: &Index) -> bool {
        match (&self.arena[*a].kind, &self.arena[*b].kind) {
            (TypeKind::TypeVar(v1), TypeKind::TypeVar(v2)) => match (v1.instance, v2.instance) {
                (Some(a_inst), Some(b_inst)) => self.equals(&a_inst, &b_inst),
                (Some(a_inst), None) => self.equals(&a_inst, b),
                (None, Some(b_inst)) => self.equals(a, &b_inst),
                (None, None) => v1.id == v2.id,
            },
            (TypeKind::TypeRef(c1), TypeKind::TypeRef(c2)) => {
                c1.name == c2.name && self.types_equal(&c1.types, &c2.types)
            }
            (TypeKind::Union(union1), TypeKind::Union(union2)) => {
                self.types_equal(&union1.types, &union2.types)
            }
            (TypeKind::Intersection(int1), TypeKind::Intersection(int2)) => {
                self.types_equal(&int1.types, &int2.types)
            }
            (TypeKind::Tuple(tuple1), TypeKind::Tuple(tuple2)) => {
                self.types_equal(&tuple1.types, &tuple2.types)
            }
            (TypeKind::Keyword(kw1), TypeKind::Keyword(kw2)) => kw1 == kw2,
            (TypeKind::Primitive(prim1), TypeKind::Primitive(prim2)) => prim1 == prim2,
            (TypeKind::Literal(l1), TypeKind::Literal(l2)) => l1 == l2,
            (TypeKind::Function(f1), TypeKind::Function(f2)) => {
                eprintln!("checking function types");
                f1.params.len() == f2.params.len()
                    && f1
                        .params
                        .iter()
                        .zip(f2.params.iter())
                        .all(|(a, b)| self.equals(&a.t, &b.t))
                    && self.equals(&f1.ret, &f2.ret)
            }
            (TypeKind::Object(o1), TypeKind::Object(o2)) => {
                o1.elems.len() == o2.elems.len()
                    && o1
                        .elems
                        .iter()
                        .all(|p1| o2.elems.iter().any(|p2| self.obj_elem_equals(p1, p2)))
            }
            (TypeKind::Rest(r1), TypeKind::Rest(r2)) => self.equals(&r1.arg, &r2.arg),
            // TODO:
            // - unification of object and intersection
            _ => false,
        }
    }

    fn types_equal(&self, types1: &[Index], types2: &[Index]) -> bool {
        types1.len() == types2.len()
            && types1
                .iter()
                .zip(types2.iter())
                .all(|(a, b)| self.equals(a, b))
    }

    fn obj_elem_equals(&self, elem1: &TObjElem, elem2: &TObjElem) -> bool {
        match (elem1, elem2) {
            (TObjElem::Mapped(map1), TObjElem::Mapped(map2)) => {
                // TODO: compare key types as well
                self.equals(&map1.value, &map2.value)
            }
            (TObjElem::Prop(p1), TObjElem::Prop(p2)) => self.equals(&p1.t, &p2.t),
            _ => false,
        }
    }

    /// A binary type constructor which builds function types
    pub fn new_func_type(
        &mut self,
        params: &[FuncParam],
        ret: Index,
        type_params: &Option<Vec<TypeParam>>,
        throws: Option<Index>,
    ) -> Index {
        self.arena.insert(Type::from(TypeKind::Function(Function {
            params: params.to_vec(),
            ret: ret.to_owned(),
            type_params: type_params.to_owned(),
            throws,
        })))
    }

    // TODO: flatten union types
    pub fn new_union_type(&mut self, types: &[Index]) -> Index {
        match types.len() {
            0 => self.new_keyword(Keyword::Never),
            1 => types[0],
            _ => self.arena.insert(Type::from(TypeKind::Union(Union {
                types: types
                    .to_owned()
                    .iter()
                    .filter(|t| !matches!(self.arena[**t].kind, TypeKind::Keyword(Keyword::Never)))
                    .cloned()
                    .collect(),
            }))),
        }
    }

    pub fn new_intersection_type(&mut self, types: &[Index]) -> Index {
        self.arena
            .insert(Type::from(TypeKind::Intersection(Intersection {
                types: types.to_owned(),
            })))
    }

    pub fn new_tuple_type(&mut self, types: &[Index]) -> Index {
        self.arena.insert(Type::from(TypeKind::Tuple(Tuple {
            types: types.to_owned(),
        })))
    }

    pub fn new_object_type(&mut self, elems: &[TObjElem]) -> Index {
        self.arena.insert(Type::from(TypeKind::Object(Object {
            elems: elems.to_vec(),
        })))
    }

    pub fn new_type_var(&mut self, constraint: Option<Index>) -> Index {
        self.arena.insert(Type::from(TypeKind::TypeVar(TypeVar {
            id: self.arena.len(), // use for debugging purposes only
            instance: None,
            constraint,
        })))
    }

    pub fn new_type_ref(&mut self, name: &str, types: &[Index]) -> Index {
        self.arena.insert(Type::from(TypeKind::TypeRef(TypeRef {
            name: name.to_string(),
            types: types.to_vec(),
        })))
    }

    pub fn new_array_type(&mut self, t: Index) -> Index {
        self.arena.insert(Type::from(TypeKind::Array(Array { t })))
    }

    pub fn new_keyword(&mut self, keyword: Keyword) -> Index {
        self.arena.insert(Type::from(TypeKind::Keyword(keyword)))
    }

    pub fn new_primitive(&mut self, primitive: Primitive) -> Index {
        self.arena
            .insert(Type::from(TypeKind::Primitive(primitive)))
    }

    pub fn new_rest_type(&mut self, t: Index) -> Index {
        self.arena
            .insert(Type::from(TypeKind::Rest(Rest { arg: t })))
    }

    pub fn new_lit_type(&mut self, lit: &Lit) -> Index {
        self.arena
            .insert(Type::from(TypeKind::Literal(lit.clone())))
    }

    pub fn new_keyof_type(&mut self, t: Index) -> Index {
        self.arena.insert(Type::from(TypeKind::KeyOf(KeyOf { t })))
    }

    pub fn new_indexed_access_type(&mut self, obj: Index, index: Index) -> Index {
        self.arena
            .insert(Type::from(TypeKind::IndexedAccess(IndexedAccess {
                obj,
                index,
            })))
    }

    pub fn new_conditional_type(
        &mut self,
        check: Index,
        extends: Index,
        true_type: Index,
        false_type: Index,
    ) -> Index {
        self.arena
            .insert(Type::from(TypeKind::Conditional(Conditional {
                check,
                extends,
                true_type,
                false_type,
            })))
    }

    pub fn new_infer_type(&mut self, name: &str) -> Index {
        self.arena.insert(Type::from(TypeKind::Infer(Infer {
            name: name.to_string(),
        })))
    }

    pub fn new_wildcard_type(&mut self) -> Index {
        self.arena.insert(Type::from(TypeKind::Wildcard))
    }

    pub fn from_type_kind(&mut self, kind: TypeKind) -> Index {
        self.arena.insert(Type::from(kind))
    }
}
