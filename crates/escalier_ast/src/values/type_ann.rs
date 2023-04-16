use derive_visitor::{Drive, DriveMut};

use crate::values::common::{SourceLocation, Span};
use crate::values::expr::Expr;
use crate::values::ident::Ident;
use crate::values::keyword::Keyword;
use crate::values::lit::Lit;
use crate::values::pattern::Pattern;

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub struct TypeAnnFnParam<T: 'static>
where
    T: Drive + DriveMut,
{
    pub pat: Pattern<T>,
    pub type_ann: TypeAnn<T>,
    #[drive(skip)]
    pub optional: bool,
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub struct LamType<T: 'static>
where
    T: Drive + DriveMut,
{
    pub params: Vec<TypeAnnFnParam<T>>,
    pub ret: Box<TypeAnn<T>>,
    pub type_params: Option<Vec<TypeParam<T>>>,
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub struct KeywordType {
    #[drive(skip)]
    pub keyword: Keyword,
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub struct TypeRef<T: 'static>
where
    T: Drive + DriveMut,
{
    #[drive(skip)]
    pub name: String,
    // TODO: drop the Option
    pub type_args: Option<Vec<TypeAnn<T>>>,
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub struct ObjectType<T: 'static>
where
    T: Drive + DriveMut,
{
    // TODO: update this to support indexers and callables as well.
    pub elems: Vec<TObjElem<T>>,
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub enum TObjElem<T: 'static>
where
    T: Drive + DriveMut,
{
    // Call(TLam),
    // Constructor(TLam),
    Index(TIndex<T>),
    Prop(TProp<T>),
    // Getter
    // Setter
    // RestSpread - we can use this instead of converting {a, ...x} to {a} & tvar
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub struct TProp<T: 'static>
where
    T: Drive + DriveMut,
{
    #[drive(skip)]
    pub loc: SourceLocation,
    #[drive(skip)]
    pub span: Span,
    #[drive(skip)]
    pub name: String,
    #[drive(skip)]
    pub optional: bool,
    #[drive(skip)]
    pub mutable: bool,
    pub type_ann: Box<TypeAnn<T>>,
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub struct TIndexKey<T: 'static>
where
    T: Drive + DriveMut,
{
    #[drive(skip)]
    pub name: String,
    pub type_ann: Box<TypeAnn<T>>,
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub struct TIndex<T: 'static>
where
    T: Drive + DriveMut,
{
    #[drive(skip)]
    pub loc: SourceLocation,
    #[drive(skip)]
    pub span: Span,
    // TODO: update this to only allow `<ident>: string` or `<ident>: number`
    pub key: Box<TypeAnnFnParam<T>>,
    #[drive(skip)]
    pub mutable: bool,
    pub type_ann: Box<TypeAnn<T>>,
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub struct UnionType<T: 'static>
where
    T: Drive + DriveMut,
{
    pub types: Vec<TypeAnn<T>>,
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub struct IntersectionType<T: 'static>
where
    T: Drive + DriveMut,
{
    pub types: Vec<TypeAnn<T>>,
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub struct TupleType<T: 'static>
where
    T: Drive + DriveMut,
{
    pub types: Vec<TypeAnn<T>>,
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub struct ArrayType<T: 'static>
where
    T: Drive + DriveMut,
{
    pub elem_type: Box<TypeAnn<T>>,
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub struct KeyOfType<T: 'static>
where
    T: Drive + DriveMut,
{
    pub type_ann: Box<TypeAnn<T>>,
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub struct QueryType<T: 'static>
where
    T: Drive + DriveMut,
{
    // TypeScript only supports typeof on (qualified) identifiers.
    // We could modify the parser if we wanted to support taking
    // the type of arbitrary expressions.
    pub expr: Box<Expr<T>>,
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub struct IndexedAccessType<T: 'static>
where
    T: Drive + DriveMut,
{
    pub obj_type: Box<TypeAnn<T>>,
    pub index_type: Box<TypeAnn<T>>,
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub struct MappedType<T: 'static>
where
    T: Drive + DriveMut,
{
    pub type_param: TypeParam<T>, // default is always None for MappedType
    pub optional: Option<TMappedTypeChange>,
    pub mutable: Option<TMappedTypeChange>,
    pub type_ann: Box<TypeAnn<T>>,
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub struct TMappedTypeChange {
    #[drive(skip)]
    pub span: Span,
    pub change: TMappedTypeChangeProp,
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub enum TMappedTypeChangeProp {
    Plus,
    Minus,
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub struct ConditionalType<T: 'static>
where
    T: Drive + DriveMut,
{
    pub check_type: Box<TypeAnn<T>>,
    pub extends_type: Box<TypeAnn<T>>,
    pub true_type: Box<TypeAnn<T>>,
    pub false_type: Box<TypeAnn<T>>,
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub struct MutableType<T: 'static>
where
    T: Drive + DriveMut,
{
    pub type_ann: Box<TypeAnn<T>>,
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub struct InferType {
    #[drive(skip)]
    pub name: String,
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub enum TypeAnnKind<T: 'static>
where
    T: Drive + DriveMut,
{
    Lam(LamType<T>),
    Lit(Lit),
    Keyword(KeywordType),
    Object(ObjectType<T>),
    TypeRef(TypeRef<T>),
    Union(UnionType<T>),
    Intersection(IntersectionType<T>),
    Tuple(TupleType<T>),
    Array(ArrayType<T>), // T[]
    KeyOf(KeyOfType<T>), // keyof
    Query(QueryType<T>), // typeof
    IndexedAccess(IndexedAccessType<T>),
    Mapped(MappedType<T>),
    Conditional(ConditionalType<T>),
    Mutable(MutableType<T>),
    Infer(InferType),
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub struct TypeAnn<T: 'static>
where
    T: Drive + DriveMut,
{
    pub kind: TypeAnnKind<T>,
    #[drive(skip)]
    pub loc: SourceLocation,
    #[drive(skip)]
    pub span: Span,
    pub inferred_type: Option<T>,
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub struct TypeParam<T: 'static>
where
    T: Drive + DriveMut,
{
    #[drive(skip)]
    pub span: Span,
    #[drive(skip)]
    pub name: Ident,
    pub constraint: Option<Box<TypeAnn<T>>>,
    pub default: Option<Box<TypeAnn<T>>>,
}
