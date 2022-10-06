use crate::expr::EFnParamPat;
use crate::expr::Expr;
use crate::ident::Ident;
use crate::keyword::Keyword;
use crate::lit::Lit;
use crate::prim::Primitive;
use crate::span::Span;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeAnnFnParam {
    pub pat: EFnParamPat,
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
pub struct PrimType {
    pub span: Span,
    pub prim: Primitive,
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
    pub key: TypeAnnFnParam,
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

// TODO: split into TypeAnnKind and TypeAnn so that we can attach
// inferred_type to TypeAnn.  This may seem a little silly on the
// surface, but we want all types to be described as Crochet Types
// within the system.  This will help with more complex LSP queries
// down the road such as jump to definition of a particular type.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeAnn {
    Lam(LamType),
    Lit(Lit),
    Keyword(KeywordType),
    Prim(PrimType),
    Object(ObjectType),
    TypeRef(TypeRef),
    Union(UnionType),
    Intersection(IntersectionType),
    Tuple(TupleType),
    Array(ArrayType), // T[]
    KeyOf(KeyOfType), // keyof
    Query(QueryType), // typeof
    IndexedAccess(IndexedAccessType),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeParam {
    pub span: Span,
    pub name: Ident,
    pub constraint: Option<Box<TypeAnn>>,
    pub default: Option<Box<TypeAnn>>,
}
