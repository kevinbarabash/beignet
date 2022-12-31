use crate::types::Type;
use crate::values::common::{SourceLocation, Span};
use crate::values::ident::*;
use crate::values::jsx::JSXElement;
use crate::values::keyword::Keyword;
use crate::values::lit::Lit;
use crate::values::pattern::{Pattern, PatternKind};
use crate::values::type_ann::{TypeAnn, TypeParam};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Program {
    pub body: Vec<Statement>,
}

// TODO: Update Statement to mimic structure of Expr/ExprKind
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Statement {
    VarDecl {
        loc: SourceLocation,
        span: Span,
        pattern: Pattern,
        type_ann: Option<TypeAnn>,
        init: Option<Box<Expr>>,
        declare: bool,
    },
    TypeDecl {
        loc: SourceLocation,
        span: Span,
        declare: bool,
        id: Ident,
        type_ann: TypeAnn,
        type_params: Option<Vec<TypeParam>>,
    },
    Expr {
        loc: SourceLocation,
        span: Span,
        expr: Box<Expr>,
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
    pub pat: Pattern,
    pub type_ann: Option<TypeAnn>,
    pub optional: bool,
    pub mutable: bool,
}

impl EFnParam {
    pub fn get_name(&self, index: &usize) -> String {
        match &self.pat.kind {
            PatternKind::Ident(BindingIdent { name, .. }) => name.to_owned(),
            _ => format!("arg{index}"),
        }
    }
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
pub struct Assign {
    pub left: Box<Expr>,
    pub right: Box<Expr>,
    pub op: AssignOp,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AssignOp {
    Eq,
    PlusEq,
    MinusEq,
    TimesEq,
    DivEq,
    ModEq,
    // TODO: support bit and logic assign ops
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BinaryExpr {
    pub op: BinOp,
    pub left: Box<Expr>,
    pub right: Box<Expr>,
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
pub struct UnaryExpr {
    pub op: UnaryOp,
    pub arg: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnaryOp {
    Minus,
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
    pub key: Ident,
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
    pub loc: SourceLocation,
    pub span: Span, // includes enclosing []
    pub expr: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TemplateElem {
    pub loc: SourceLocation,
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
    pub loc: SourceLocation,
    pub span: Span,
    pub pattern: Pattern,
    pub guard: Option<Expr>,
    pub body: Expr,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct New {
    pub expr: Box<Expr>, // should resolve to an object with a constructor signature
    pub args: Vec<ExprOrSpread>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExprKind {
    App(App),
    New(New), // like App but for calling constructors to create a new instance
    Fix(Fix),
    Ident(Ident),
    IfElse(IfElse),
    JSXElement(JSXElement),
    Lambda(Lambda),
    Let(Let),
    Assign(Assign),
    LetExpr(LetExpr), // should only be used in `if let` expressions
    Lit(Lit),
    Keyword(Keyword),
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
    pub loc: SourceLocation,
    pub span: Span,
    pub kind: ExprKind,
    pub inferred_type: Option<Type>,
}
