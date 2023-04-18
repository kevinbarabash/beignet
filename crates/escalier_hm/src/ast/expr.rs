use generational_arena::Index;

use crate::ast::block::Block;
use crate::ast::class::Class;
use crate::ast::common::{SourceLocation, Span};
use crate::ast::ident::*;
use crate::ast::jsx::JSXElement;
use crate::ast::keyword::Keyword;
use crate::ast::lit::Lit;
use crate::ast::pattern::{Pattern, PatternKind};
use crate::ast::stmt::Statement;
use crate::ast::type_ann::{TypeAnn, TypeParam};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Program {
    pub statements: Vec<Statement>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct App {
    pub lam: Box<Expr>,
    pub args: Vec<ExprOrSpread>,
    pub type_args: Option<Vec<TypeAnn>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct IfElse {
    pub cond: Box<Expr>,
    pub consequent: Block,
    pub alternate: Option<Block>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LetExpr {
    pub pat: Pattern,
    pub expr: Box<Expr>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum BlockOrExpr {
    Block(Block),
    Expr(Box<Expr>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Lambda {
    pub params: Vec<EFnParam>,
    pub body: BlockOrExpr,
    pub is_async: bool,
    pub return_type: Option<TypeAnn>,
    pub type_params: Option<Vec<TypeParam>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct EFnParam {
    pub pat: Pattern,
    pub type_ann: Option<TypeAnn>,
    pub optional: bool,
}

impl EFnParam {
    pub fn get_name(&self, index: &usize) -> String {
        match &self.pat.kind {
            PatternKind::Ident(BindingIdent { name, .. }) => name.to_owned(),
            _ => format!("arg{index}"),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Assign {
    pub left: Box<Expr>,
    pub right: Box<Expr>,
    pub op: AssignOp,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum AssignOp {
    Eq,
    PlusEq,
    MinusEq,
    TimesEq,
    DivEq,
    ModEq,
    // TODO: support bit and logic assign ops
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BinaryExpr {
    pub op: BinOp,
    pub left: Box<Expr>,
    pub right: Box<Expr>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
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

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct UnaryExpr {
    pub op: UnaryOp,
    pub arg: Box<Expr>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum UnaryOp {
    Minus,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Obj {
    pub props: Vec<PropOrSpread>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum PropOrSpread {
    Spread(SpreadElement),
    Prop(Box<Prop>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SpreadElement {
    pub expr: Box<Expr>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Prop {
    Shorthand(Ident),
    KeyValue(KeyValueProp),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct KeyValueProp {
    pub key: Ident,
    pub value: Box<Expr>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Await {
    pub expr: Box<Expr>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Tuple {
    pub elems: Vec<ExprOrSpread>,
}

// TODO: make this a enum instead of a struct
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ExprOrSpread {
    pub spread: Option<Span>,
    pub expr: Box<Expr>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Member {
    pub obj: Box<Expr>,
    pub prop: MemberProp,
}

#[derive(Clone, Debug, PartialEq, Eq)]
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

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ComputedPropName {
    pub loc: SourceLocation,
    pub span: Span, // includes enclosing []
    pub expr: Box<Expr>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TemplateElem {
    pub loc: SourceLocation,
    pub span: Span,
    pub raw: Lit,
    pub cooked: Lit,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TemplateLiteral {
    pub exprs: Vec<Expr>,
    pub quasis: Vec<TemplateElem>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TaggedTemplateLiteral {
    pub tag: Ident,
    // TODO: figure out how to track the span of the `template` part
    pub template: TemplateLiteral,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Match {
    pub expr: Box<Expr>,
    pub arms: Vec<Arm>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Arm {
    pub loc: SourceLocation,
    pub span: Span,
    pub pattern: Pattern,
    pub guard: Option<Expr>,
    pub body: Block,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct New {
    pub expr: Box<Expr>, // should resolve to an object with a constructor signature
    pub args: Vec<ExprOrSpread>,
    pub type_args: Option<Vec<TypeAnn>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Regex {
    pub pattern: String,
    pub flags: Option<String>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DoExpr {
    pub body: Block,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ExprKind {
    App(App),
    New(New), // like App but for calling constructors to create a new instance
    Ident(Ident),
    IfElse(IfElse),
    JSXElement(JSXElement),
    Lambda(Lambda),
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
    Class(Class),
    Regex(Regex),
    DoExpr(DoExpr),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Expr {
    pub loc: SourceLocation,
    pub span: Span,
    pub kind: ExprKind,
    pub inferred_type: Option<Index>,
}
