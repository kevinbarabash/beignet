use derive_visitor::{Drive, DriveMut};

use crate::types::Type;
use crate::values::class::Class;
use crate::values::common::{SourceLocation, Span};
use crate::values::ident::*;
use crate::values::jsx::JSXElement;
use crate::values::keyword::Keyword;
use crate::values::lit::Lit;
use crate::values::pattern::{Pattern, PatternKind};
use crate::values::type_ann::{TypeAnn, TypeParam};

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub struct Program {
    pub body: Vec<Statement>,
}

// TODO: Update Statement to mimic structure of Expr/ExprKind
// TODO: Update Statement to have an .inferred_type field like Expr
#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub enum Statement {
    ClassDecl {
        #[drive(skip)]
        loc: SourceLocation,
        #[drive(skip)]
        span: Span,
        #[drive(skip)]
        ident: Ident, // Why do have `ident` here an in `Class`?
        class: Box<Class>,
    },
    VarDecl {
        #[drive(skip)]
        loc: SourceLocation,
        #[drive(skip)]
        span: Span,
        pattern: Pattern,
        type_ann: Option<TypeAnn>,
        init: Option<Box<Expr>>,
        #[drive(skip)]
        declare: bool,
    },
    TypeDecl {
        #[drive(skip)]
        loc: SourceLocation,
        #[drive(skip)]
        span: Span,
        #[drive(skip)]
        declare: bool,
        #[drive(skip)]
        id: Ident,
        type_ann: TypeAnn,
        type_params: Option<Vec<TypeParam>>,
    },
    ExprStmt(Expr),
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub struct Block {
    #[drive(skip)]
    pub span: Span,
    pub stmts: Vec<Expr>,
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub struct App {
    pub lam: Box<Expr>,
    pub args: Vec<ExprOrSpread>,
    pub type_args: Option<Vec<TypeAnn>>,
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub struct Fix {
    pub expr: Box<Expr>,
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub struct IfElse {
    pub cond: Box<Expr>,
    pub consequent: Block,
    pub alternate: Option<Block>,
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub struct LetExpr {
    pub pat: Pattern,
    pub expr: Box<Expr>,
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub struct Lambda {
    pub params: Vec<EFnParam>,
    pub body: Block,
    #[drive(skip)]
    pub is_async: bool,
    pub return_type: Option<TypeAnn>,
    pub type_params: Option<Vec<TypeParam>>,
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub struct EFnParam {
    pub pat: Pattern,
    pub type_ann: Option<TypeAnn>,
    #[drive(skip)]
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

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub struct Assign {
    pub left: Box<Expr>,
    pub right: Box<Expr>,
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
pub struct BinaryExpr {
    pub op: BinOp,
    pub left: Box<Expr>,
    pub right: Box<Expr>,
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
pub struct UnaryExpr {
    pub op: UnaryOp,
    pub arg: Box<Expr>,
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub enum UnaryOp {
    Minus,
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub struct Obj {
    pub props: Vec<PropOrSpread>,
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub enum PropOrSpread {
    Spread(SpreadElement),
    Prop(Box<Prop>),
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub struct SpreadElement {
    pub expr: Box<Expr>,
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub enum Prop {
    #[drive(skip)]
    Shorthand(Ident),
    KeyValue(KeyValueProp),
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub struct KeyValueProp {
    #[drive(skip)]
    pub key: Ident,
    pub value: Box<Expr>,
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub struct Await {
    pub expr: Box<Expr>,
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub struct Tuple {
    pub elems: Vec<ExprOrSpread>,
}

// TODO: make this a enum instead of a struct
#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub struct ExprOrSpread {
    #[drive(skip)]
    pub spread: Option<Span>,
    pub expr: Box<Expr>,
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub struct Member {
    pub obj: Box<Expr>,
    #[drive(skip)]
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

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub struct ComputedPropName {
    #[drive(skip)]
    pub loc: SourceLocation,
    #[drive(skip)]
    pub span: Span, // includes enclosing []
    pub expr: Box<Expr>,
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
pub struct TemplateLiteral {
    pub exprs: Vec<Expr>,
    pub quasis: Vec<TemplateElem>,
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub struct TaggedTemplateLiteral {
    #[drive(skip)]
    pub tag: Ident,
    // TODO: figure out how to track the span of the `template` part
    pub template: TemplateLiteral,
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub struct Match {
    pub expr: Box<Expr>,
    pub arms: Vec<Arm>,
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub struct Arm {
    #[drive(skip)]
    pub loc: SourceLocation,
    #[drive(skip)]
    pub span: Span,
    pub pattern: Pattern,
    pub guard: Option<Expr>,
    pub body: Block,
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub struct New {
    pub expr: Box<Expr>, // should resolve to an object with a constructor signature
    pub args: Vec<ExprOrSpread>,
    pub type_args: Option<Vec<TypeAnn>>,
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub struct Regex {
    #[drive(skip)]
    pub pattern: String,
    #[drive(skip)]
    pub flags: Option<String>,
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub struct DoExpr {
    pub body: Block,
}

// Later on we can make this a real decl.  For now, it's here to ease the move
// to deferring lambda-ization.
#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub struct LetDecl {
    pub pattern: Pattern,
    #[drive(skip)]
    pub type_ann: Option<TypeAnn>,
    pub init: Box<Expr>,
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub enum ExprKind {
    App(App),
    New(New), // like App but for calling constructors to create a new instance
    Fix(Fix),
    #[drive(skip)]
    Ident(Ident),
    IfElse(IfElse),
    JSXElement(JSXElement),
    Lambda(Lambda),
    Assign(Assign),
    LetExpr(LetExpr), // should only be used in `if let` expressions
    Lit(Lit),
    #[drive(skip)]
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
    LetDecl(LetDecl),
}

#[derive(Clone, Debug, Drive, DriveMut, PartialEq, Eq)]
pub struct Expr {
    #[drive(skip)]
    pub loc: SourceLocation,
    #[drive(skip)]
    pub span: Span,
    pub kind: ExprKind,
    pub inferred_type: Option<Type>,
}
