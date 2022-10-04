use crochet_types::Type;

use crate::ident::Ident;
use crate::jsx::JSXElement;
use crate::lit::Lit;
use crate::pattern::Pattern;
use crate::span::Span;
use crate::type_ann::{TypeAnn, TypeParam};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Program {
    pub body: Vec<Statement>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Statement {
    VarDecl {
        span: Span,
        pattern: Pattern,
        type_ann: Option<TypeAnn>,
        init: Option<Expr>,
        declare: bool,
    },
    TypeDecl {
        span: Span,
        declare: bool,
        id: Ident,
        type_ann: TypeAnn,
        type_params: Option<Vec<TypeParam>>,
    },
    Expr {
        span: Span,
        expr: Expr,
    }, // NOTE: does not include Expr::Let
}

// #[derive(Debug, Clone, PartialEq, Eq)]
// struct Block {
//     pub span: Span,
//     pub stmts: Vec<Expr>,
// }

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct App {
    pub lam: Box<Expr>,
    pub args: Vec<ExprOrSpread>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Fix {
    pub expr: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IfElse {
    pub cond: Box<Expr>,
    pub consequent: Box<Expr>,
    pub alternate: Option<Box<Expr>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LetExpr {
    pub pat: Pattern,
    pub expr: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Lambda {
    pub params: Vec<EFnParam>,
    pub body: Box<Expr>,
    pub is_async: bool,
    pub return_type: Option<TypeAnn>,
    pub type_params: Option<Vec<TypeParam>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EFnParam {
    pub pat: EFnParamPat,
    pub type_ann: Option<TypeAnn>,
    pub optional: bool,
    pub mutable: bool,
}

impl EFnParam {
    pub fn get_name(&self, index: &usize) -> String {
        match &self.pat {
            EFnParamPat::Ident(bi) => bi.id.name.to_owned(),
            _ => format!("arg{index}"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EFnParamPat {
    Ident(EFnParamBindingIdent),
    Rest(EFnParamRestPat),
    Object(EFnParamObjectPat),
    Array(EFnParamArrayPat),
}

impl EFnParamPat {
    pub fn get_name(&self, index: &usize) -> String {
        match self {
            EFnParamPat::Ident(bi) => bi.id.name.to_owned(),
            _ => format!("arg{index}"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EFnParamBindingIdent {
    pub span: Span,
    pub id: Ident,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EFnParamRestPat {
    pub span: Span,
    pub arg: Box<EFnParamPat>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EFnParamArrayPat {
    pub span: Span,
    pub elems: Vec<Option<EFnParamPat>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EFnParamObjectPat {
    pub span: Span,
    pub props: Vec<EFnParamObjectPatProp>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EFnParamObjectPatProp {
    KeyValue(EFnParamKeyValuePatProp),
    Assign(EFnParamAssignPatProp),
    Rest(EFnParamRestPat),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EFnParamKeyValuePatProp {
    // TODO: span
    pub key: Ident,
    pub value: Box<EFnParamPat>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EFnParamAssignPatProp {
    // TODO: span
    pub key: Ident,
    pub value: Option<Box<Expr>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Let {
    pub pattern: Option<Pattern>,
    pub type_ann: Option<TypeAnn>,
    pub init: Box<Expr>,
    pub body: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BinaryExpr {
    pub op: BinOp,
    pub left: Box<Expr>,
    pub right: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnaryExpr {
    pub op: UnaryOp,
    pub arg: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Obj {
    pub props: Vec<PropOrSpread>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PropOrSpread {
    Spread(SpreadElement),
    Prop(Box<Prop>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SpreadElement {
    pub expr: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Prop {
    Shorthand(Ident),
    KeyValue(KeyValueProp),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct KeyValueProp {
    pub name: String,
    pub value: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Await {
    pub expr: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Tuple {
    pub elems: Vec<ExprOrSpread>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExprOrSpread {
    pub spread: Option<Span>,
    pub expr: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Member {
    pub obj: Box<Expr>,
    pub prop: MemberProp,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MemberProp {
    Ident(Ident),
    Computed(ComputedPropName),
}

impl MemberProp {
    pub fn name(&self) -> String {
        match self {
            MemberProp::Ident(Ident { name, .. }) => name.to_owned(),
            MemberProp::Computed(_) => todo!(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ComputedPropName {
    pub span: Span, // includes enclosing []
    pub expr: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TemplateElem {
    pub span: Span,
    pub raw: Lit,
    pub cooked: Lit,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TemplateLiteral {
    pub exprs: Vec<Expr>,
    pub quasis: Vec<TemplateElem>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TaggedTemplateLiteral {
    pub tag: Ident,
    // TODO: figure out how to track the span of the `template` part
    pub template: TemplateLiteral,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Match {
    pub expr: Box<Expr>,
    pub arms: Vec<Arm>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Arm {
    pub span: Span,
    pub pattern: Pattern,
    pub guard: Option<Expr>,
    pub body: Expr,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExprKind {
    App(App),
    Fix(Fix),
    Ident(Ident),
    IfElse(IfElse),
    JSXElement(JSXElement),
    Lambda(Lambda),
    Let(Let),
    LetExpr(LetExpr), // should only be used in `if let` expressions
    Lit(Lit),
    BinaryExpr(BinaryExpr),
    UnaryExpr(UnaryExpr),
    Obj(Obj),
    Await(Await),
    Tuple(Tuple),
    Member(Member),
    Empty,
    TemplateLiteral(TemplateLiteral),
    TaggedTemplateLiteral(TaggedTemplateLiteral),
    Match(Match),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Expr {
    pub span: Span,
    pub kind: ExprKind,
    pub inferred_type: Option<Type>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnaryOp {
    Minus,
}
