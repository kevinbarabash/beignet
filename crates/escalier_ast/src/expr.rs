use generational_arena::Index;

use crate::block::Block;
use crate::class::Class;
use crate::func_param::FuncParam;
use crate::identifier::Ident;
use crate::jsx::{JSXElement, JSXFragment};
use crate::pattern::Pattern;
use crate::span::*;
use crate::type_ann::TypeAnn;
use crate::type_param::TypeParam;

// TODO: track source location
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ObjectKey {
    Ident(Ident),
    String(String),
    Number(String),
    Computed(Box<Expr>),
}

// TODO: track source location
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Prop {
    Shorthand(Ident),
    Property { key: ObjectKey, value: Expr },
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum PropOrSpread {
    Prop(Prop),
    Spread(Expr),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ExprOrSpread {
    Expr(Expr),
    Spread(Expr),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Num {
    pub value: String,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Str {
    pub span: Span,
    pub value: String,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Bool {
    pub value: bool,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Null {}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Undefined {}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct TemplateLiteral {
    pub tag: Option<Box<Expr>>,
    pub parts: Vec<Str>,
    pub exprs: Vec<Expr>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Object {
    pub properties: Vec<PropOrSpread>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Tuple {
    pub elements: Vec<ExprOrSpread>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Assign {
    pub left: Box<Expr>,
    pub op: AssignOp,
    pub right: Box<Expr>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Binary {
    pub left: Box<Expr>,
    pub op: BinaryOp,
    pub right: Box<Expr>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Unary {
    pub op: UnaryOp,
    pub right: Box<Expr>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Function {
    pub type_params: Option<Vec<TypeParam>>,
    pub params: Vec<FuncParam>,
    pub body: BlockOrExpr,
    pub type_ann: Option<TypeAnn>, // return type
    pub throws: Option<TypeAnn>,
    pub is_async: bool,
    pub is_gen: bool,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Call {
    pub callee: Box<Expr>,
    pub type_args: Option<Vec<TypeAnn>>,
    pub args: Vec<Expr>,
    pub opt_chain: bool,
    pub throws: Option<Index>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Member {
    pub object: Box<Expr>,
    pub property: MemberProp,
    pub opt_chain: bool,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum MemberProp {
    Ident(Ident),
    Computed(ComputedPropName),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ComputedPropName {
    pub span: Span, // includes enclosing []
    pub expr: Box<Expr>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct OptionalChain {
    pub base: Box<Expr>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct IfElse {
    pub cond: Box<Expr>,
    pub consequent: Block,
    pub alternate: Option<BlockOrExpr>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Match {
    pub expr: Box<Expr>,
    pub arms: Vec<MatchArm>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Try {
    pub body: Block,
    // At least `catch` or `finally` must be present
    pub catch: Option<CatchClause>,
    pub finally: Option<Block>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Do {
    pub body: Block,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Await {
    pub arg: Box<Expr>,
    // Awaiting a rejected promise turns it into a throw.
    pub throws: Option<Index>, // the type of the thrown value
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Yield {
    pub arg: Box<Expr>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Throw {
    pub arg: Box<Expr>,
    pub throws: Option<Index>, // the type of the thrown value
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ExprKind {
    Ident(Ident),
    Num(Num),
    Str(Str),
    Bool(Bool),
    Null(Null),
    Undefined(Undefined),
    TemplateLiteral(TemplateLiteral),
    // TODO: Add regex support
    // Regex(Regex),
    Object(Object),
    Tuple(Tuple),
    Assign(Assign),
    Binary(Binary),
    Unary(Unary),
    Function(Function),
    Class(Class),
    Call(Call),
    Member(Member),
    IfElse(IfElse),
    Match(Match),
    Try(Try),
    Do(Do),
    Await(Await),
    Yield(Yield),
    Throw(Throw),
    JSXElement(JSXElement),
    JSXFragment(JSXFragment),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
    pub inferred_type: Option<Index>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct CatchClause {
    pub param: Option<Pattern>,
    pub body: Block,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct MatchArm {
    // pub loc: SourceLocation,
    pub span: Span,
    pub pattern: Pattern,
    pub guard: Option<Box<Expr>>,
    pub body: BlockOrExpr,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum BlockOrExpr {
    Block(Block),
    Expr(Box<Expr>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum AssignOp {
    Assign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    ModAssign,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum BinaryOp {
    Plus,
    Minus,
    Times,
    Divide,
    Modulo,
    Equals,
    NotEquals,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
    Or,
    And,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum UnaryOp {
    Plus,
    Minus,
    Not,
}

impl Expr {
    pub fn get_span(&self) -> Span {
        self.span.to_owned()
    }

    pub fn is_lvalue(&self) -> bool {
        match &self.kind {
            ExprKind::Ident(_) => true,
            ExprKind::Member(Member { object, .. }) => object.is_lvalue(),
            _ => false,
        }
    }
}
