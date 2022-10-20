use crate::types::Type;
use crate::values::expr::Expr;
use crate::values::ident::Ident;
use crate::values::keyword::Keyword;
use crate::values::lit::Lit;
use crate::values::pattern::Pattern;
use crate::values::span::Span;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeAnnFnParam {
    pub pat: Pattern,
    pub type_ann: TypeAnn,
    pub optional: bool,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LamType {
    pub span: Span,
    pub params: Vec<TypeAnnFnParam>,
    pub ret: Box<TypeAnn>,
    pub type_params: Option<Vec<TypeParam>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct KeywordType {
    pub span: Span,
    pub keyword: Keyword,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TypeRef {
    pub span: Span,
    pub name: String,
    // TODO: drop the Option
    pub type_args: Option<Vec<TypeAnn>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ObjectType {
    pub span: Span,
    // TODO: update this to support indexers and callables as well.
    pub elems: Vec<TObjElem>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TObjElem {
    // Call(TLam),
    // Constructor(TLam),
    Index(TIndex),
    Prop(TProp),
    // Getter
    // Setter
    // RestSpread - we can use this instead of converting {a, ...x} to {a} & tvar
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TProp {
    pub span: Span,
    pub name: String,
    pub optional: bool,
    pub mutable: bool,
    pub type_ann: Box<TypeAnn>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TIndex {
    pub span: Span,
    // TODO: update this to only allow `<ident>: string` or `<ident>: number`
    pub key: Box<TypeAnnFnParam>,
    pub mutable: bool,
    pub type_ann: Box<TypeAnn>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct UnionType {
    pub span: Span,
    pub types: Vec<TypeAnn>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct IntersectionType {
    pub span: Span,
    pub types: Vec<TypeAnn>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TupleType {
    pub span: Span,
    pub types: Vec<TypeAnn>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ArrayType {
    pub span: Span,
    pub elem_type: Box<TypeAnn>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct KeyOfType {
    pub span: Span,
    pub type_ann: Box<TypeAnn>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct QueryType {
    pub span: Span,
    // TypeScript only supports typeof on (qualified) identifiers.
    // We could modify the parser if we wanted to support taking
    // the type of arbitrary expressions.
    pub expr: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IndexedAccessType {
    pub span: Span,
    pub obj_type: Box<TypeAnn>,
    pub index_type: Box<TypeAnn>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MutableType {
    pub span: Span,
    pub type_ann: Box<TypeAnn>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeAnnKind {
    Lam(LamType),
    Lit(Lit),
    Keyword(KeywordType),
    Object(ObjectType),
    TypeRef(TypeRef),
    Union(UnionType),
    Intersection(IntersectionType),
    Tuple(TupleType),
    Array(ArrayType), // T[]
    KeyOf(KeyOfType), // keyof
    Query(QueryType), // typeof
    IndexedAccess(IndexedAccessType),
    Mutable(MutableType),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TypeAnn {
    pub kind: TypeAnnKind,
    pub span: Span,
    pub inferred_type: Option<Type>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeParam {
    pub span: Span,
    pub name: Ident,
    pub constraint: Option<Box<TypeAnn>>,
    pub default: Option<Box<TypeAnn>>,
}
