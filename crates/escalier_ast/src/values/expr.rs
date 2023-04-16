use derive_visitor::{Drive, DriveMut};

use crate::values::block::Block;
use crate::values::class::Class;
use crate::values::common::{SourceLocation, Span};
use crate::values::ident::*;
use crate::values::jsx::JSXElement;
use crate::values::keyword::Keyword;
use crate::values::lit::Lit;
use crate::values::pattern::{Pattern, PatternKind};
use crate::values::stmt::Statement;
use crate::values::type_ann::{TypeAnn, TypeParam};

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub struct Program<T: 'static>
where
    T: Drive + DriveMut,
{
    pub body: Vec<Statement<T>>,
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub struct App<T: 'static>
where
    T: Drive + DriveMut,
{
    pub lam: Box<Expr<T>>,
    pub args: Vec<ExprOrSpread<T>>,
    pub type_args: Option<Vec<TypeAnn<T>>>,
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub struct Fix<T: 'static>
where
    T: Drive + DriveMut,
{
    pub expr: Box<Expr<T>>,
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub struct IfElse<T: 'static>
where
    T: Drive + DriveMut,
{
    pub cond: Box<Expr<T>>,
    pub consequent: Block<T>,
    pub alternate: Option<Block<T>>,
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub struct LetExpr<T: 'static>
where
    T: Drive + DriveMut,
{
    pub pat: Pattern<T>,
    pub expr: Box<Expr<T>>,
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub enum BlockOrExpr<T: 'static>
where
    T: Drive + DriveMut,
{
    Block(Block<T>),
    Expr(Box<Expr<T>>),
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub struct Lambda<T: 'static>
where
    T: Drive + DriveMut,
{
    pub params: Vec<EFnParam<T>>,
    pub body: BlockOrExpr<T>,
    #[drive(skip)]
    pub is_async: bool,
    pub return_type: Option<TypeAnn<T>>,
    pub type_params: Option<Vec<TypeParam<T>>>,
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub struct EFnParam<T: 'static>
where
    T: Drive + DriveMut,
{
    pub pat: Pattern<T>,
    pub type_ann: Option<TypeAnn<T>>,
    #[drive(skip)]
    pub optional: bool,
}

impl<T: 'static> EFnParam<T>
where
    T: Drive + DriveMut,
{
    pub fn get_name(&self, index: &usize) -> String {
        match &self.pat.kind {
            PatternKind::Ident(BindingIdent { name, .. }) => name.to_owned(),
            _ => format!("arg{index}"),
        }
    }
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub struct Assign<T: 'static>
where
    T: Drive + DriveMut,
{
    pub left: Box<Expr<T>>,
    pub right: Box<Expr<T>>,
    pub op: AssignOp,
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub enum AssignOp {
    Eq,
    PlusEq,
    MinusEq,
    TimesEq,
    DivEq,
    ModEq,
    // TODO: support bit and logic assign ops
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub struct BinaryExpr<T: 'static>
where
    T: Drive + DriveMut,
{
    pub op: BinOp,
    pub left: Box<Expr<T>>,
    pub right: Box<Expr<T>>,
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    EqEq,
    NotEq,
    Gt,
    GtEq,
    Lt,
    LtEq,
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub struct UnaryExpr<T: 'static>
where
    T: Drive + DriveMut,
{
    pub op: UnaryOp,
    pub arg: Box<Expr<T>>,
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub enum UnaryOp {
    Minus,
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub struct Obj<T: 'static>
where
    T: Drive + DriveMut,
{
    pub props: Vec<PropOrSpread<T>>,
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub enum PropOrSpread<T: 'static>
where
    T: Drive + DriveMut,
{
    Spread(SpreadElement<T>),
    Prop(Box<Prop<T>>),
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub struct SpreadElement<T: 'static>
where
    T: Drive + DriveMut,
{
    pub expr: Box<Expr<T>>,
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub enum Prop<T: 'static>
where
    T: Drive + DriveMut,
{
    #[drive(skip)]
    Shorthand(Ident),
    KeyValue(KeyValueProp<T>),
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub struct KeyValueProp<T: 'static>
where
    T: Drive + DriveMut,
{
    #[drive(skip)]
    pub key: Ident,
    pub value: Box<Expr<T>>,
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub struct Await<T: 'static>
where
    T: Drive + DriveMut,
{
    pub expr: Box<Expr<T>>,
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub struct Tuple<T: 'static>
where
    T: Drive + DriveMut,
{
    pub elems: Vec<ExprOrSpread<T>>,
}

// TODO: make this a enum instead of a struct
#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub struct ExprOrSpread<T: 'static>
where
    T: Drive + DriveMut,
{
    #[drive(skip)]
    pub spread: Option<Span>,
    pub expr: Box<Expr<T>>,
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub struct Member<T: 'static>
where
    T: Drive + DriveMut,
{
    pub obj: Box<Expr<T>>,
    #[drive(skip)]
    pub prop: MemberProp<T>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum MemberProp<T: 'static>
where
    T: Drive + DriveMut,
{
    Ident(Ident),
    Computed(ComputedPropName<T>),
}

impl<T: 'static> MemberProp<T>
where
    T: Drive + DriveMut,
{
    pub fn name(&self) -> String {
        match self {
            MemberProp::Ident(Ident { name, .. }) => name.to_owned(),
            MemberProp::Computed(_) => todo!(),
        }
    }
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub struct ComputedPropName<T: 'static>
where
    T: Drive + DriveMut,
{
    #[drive(skip)]
    pub loc: SourceLocation,
    #[drive(skip)]
    pub span: Span, // includes enclosing []
    pub expr: Box<Expr<T>>,
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub struct TemplateElem {
    #[drive(skip)]
    pub loc: SourceLocation,
    #[drive(skip)]
    pub span: Span,
    pub raw: Lit,
    pub cooked: Lit,
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub struct TemplateLiteral<T: 'static>
where
    T: Drive + DriveMut,
{
    pub exprs: Vec<Expr<T>>,
    pub quasis: Vec<TemplateElem>,
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub struct TaggedTemplateLiteral<T: 'static>
where
    T: Drive + DriveMut,
{
    #[drive(skip)]
    pub tag: Ident,
    // TODO: figure out how to track the span of the `template` part
    pub template: TemplateLiteral<T>,
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub struct Match<T: 'static>
where
    T: Drive + DriveMut,
{
    pub expr: Box<Expr<T>>,
    pub arms: Vec<Arm<T>>,
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub struct Arm<T: 'static>
where
    T: Drive + DriveMut,
{
    #[drive(skip)]
    pub loc: SourceLocation,
    #[drive(skip)]
    pub span: Span,
    pub pattern: Pattern<T>,
    pub guard: Option<Expr<T>>,
    pub body: Block<T>,
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub struct New<T: 'static>
where
    T: Drive + DriveMut,
{
    pub expr: Box<Expr<T>>, // should resolve to an object with a constructor signature
    pub args: Vec<ExprOrSpread<T>>,
    pub type_args: Option<Vec<TypeAnn<T>>>,
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub struct Regex {
    #[drive(skip)]
    pub pattern: String,
    #[drive(skip)]
    pub flags: Option<String>,
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub struct DoExpr<T: 'static>
where
    T: Drive + DriveMut,
{
    pub body: Block<T>,
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub enum ExprKind<T: 'static>
where
    T: Drive + DriveMut,
{
    App(App<T>),
    New(New<T>), // like App but for calling constructors to create a new instance
    Fix(Fix<T>),
    #[drive(skip)]
    Ident(Ident),
    IfElse(IfElse<T>),
    JSXElement(JSXElement<T>),
    Lambda(Lambda<T>),
    Assign(Assign<T>),
    LetExpr(LetExpr<T>), // should only be used in `if let` expressions
    Lit(Lit),
    #[drive(skip)]
    Keyword(Keyword),
    BinaryExpr(BinaryExpr<T>),
    UnaryExpr(UnaryExpr<T>),
    Obj(Obj<T>),
    Await(Await<T>),
    Tuple(Tuple<T>),
    Member(Member<T>),
    Empty,
    TemplateLiteral(TemplateLiteral<T>),
    TaggedTemplateLiteral(TaggedTemplateLiteral<T>),
    Match(Match<T>),
    Class(Class<T>),
    Regex(Regex),
    DoExpr(DoExpr<T>),
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub struct Expr<T: 'static>
where
    T: Drive + DriveMut,
{
    #[drive(skip)]
    pub loc: SourceLocation,
    #[drive(skip)]
    pub span: Span,
    pub kind: ExprKind<T>,
    pub inferred_type: Option<T>,
}
