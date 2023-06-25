use crate::func_param::FuncParam;
use crate::identifier::Ident;
use crate::jsx::{JSXElement, JSXFragment};
use crate::pattern::Pattern;
use crate::span::*;
use crate::stmt::Stmt;
use crate::type_ann::TypeAnn;

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
    Shorthand { key: String }, // TODO: use Identifier
    Property { key: ObjectKey, value: Expr },
    // TODO:
    // - method
    // - getter
    // - setter
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
    pub span: Span,
    pub value: String,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Str {
    pub span: Span,
    pub value: String,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Bool {
    pub span: Span,
    pub value: bool,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Null {
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Undefined {
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct TemplateLiteral {
    pub span: Span,
    pub parts: Vec<Str>,
    pub exprs: Vec<Expr>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Object {
    pub span: Span,
    pub properties: Vec<PropOrSpread>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Tuple {
    pub span: Span,
    pub elements: Vec<ExprOrSpread>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Assign {
    pub span: Span,
    pub left: Box<Expr>,
    pub op: AssignOp,
    pub right: Box<Expr>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Binary {
    pub span: Span,
    pub left: Box<Expr>,
    pub op: BinaryOp,
    pub right: Box<Expr>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Unary {
    pub span: Span,
    pub op: UnaryOp,
    pub right: Box<Expr>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct TypeParam {
    pub span: Span,
    pub t: TypeAnn,
    pub bound: Option<TypeAnn>,
    pub default: Option<TypeAnn>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Function {
    pub span: Span,
    pub type_params: Option<Vec<TypeParam>>,
    pub params: Vec<FuncParam>,
    pub body: BlockOrExpr,
    pub type_ann: Option<TypeAnn>, // return type
    pub is_async: bool,
    pub is_gen: bool,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Call {
    pub span: Span,
    pub callee: Box<Expr>,
    pub type_args: Option<Vec<TypeAnn>>,
    pub args: Vec<Expr>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Member {
    pub span: Span,
    pub object: Box<Expr>,
    pub property: Box<Expr>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Index {
    pub span: Span,
    pub left: Box<Expr>,
    pub right: Box<Expr>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct OptionalChain {
    pub span: Span,
    pub base: Box<Expr>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct IfElse {
    pub span: Span,
    pub cond: Box<Expr>,
    pub consequent: Block,
    pub alternate: Option<Block>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Match {
    pub span: Span,
    pub expr: Box<Expr>,
    pub arms: Vec<MatchArm>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Try {
    pub span: Span,
    pub body: Block,
    // At least `catch` or `finally` must be present
    pub catch: Option<CatchClause>,
    pub finally: Option<Block>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Do {
    pub span: Span,
    pub body: Block,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Await {
    pub span: Span,
    pub arg: Box<Expr>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Yield {
    pub span: Span,
    pub arg: Box<Expr>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expr {
    Ident(Ident),
    Num(Num),
    Str(Str),
    Bool(Bool),
    Null(Null),
    Undefined(Undefined),
    TemplateLiteral(TemplateLiteral),
    Object(Object),
    Tuple(Tuple),
    Assign(Assign),
    Binary(Binary),
    Unary(Unary),
    Function(Function),
    Call(Call),
    Member(Member),
    Index(Index),
    OptionalChain(OptionalChain),
    IfElse(IfElse),
    Match(Match),
    Try(Try),
    Do(Do),
    Await(Await),
    Yield(Yield),
    JSXElement(JSXElement),
    JSXFragment(JSXFragment),
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
pub struct Block {
    pub span: Span,
    pub stmts: Vec<Stmt>,
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
}

impl Expr {
    pub fn get_span(&self) -> Span {
        match self {
            Expr::Ident(Ident { span, .. }) => span.to_owned(),
            Expr::Num(Num { span, .. }) => span.to_owned(),
            Expr::Str(Str { span, .. }) => span.to_owned(),
            Expr::Bool(Bool { span, .. }) => span.to_owned(),
            Expr::Null(Null { span, .. }) => span.to_owned(),
            Expr::Undefined(Undefined { span, .. }) => span.to_owned(),
            Expr::TemplateLiteral(TemplateLiteral { span, .. }) => span.to_owned(),
            Expr::Object(Object { span, .. }) => span.to_owned(),
            Expr::Tuple(Tuple { span, .. }) => span.to_owned(),
            Expr::Assign(Assign { span, .. }) => span.to_owned(),
            Expr::Binary(Binary { span, .. }) => span.to_owned(),
            Expr::Unary(Unary { span, .. }) => span.to_owned(),
            Expr::Function(Function { span, .. }) => span.to_owned(),
            Expr::Call(Call { span, .. }) => span.to_owned(),
            Expr::Member(Member { span, .. }) => span.to_owned(),
            Expr::Index(Index { span, .. }) => span.to_owned(),
            Expr::OptionalChain(OptionalChain { span, .. }) => span.to_owned(),
            Expr::IfElse(IfElse { span, .. }) => span.to_owned(),
            Expr::Match(Match { span, .. }) => span.to_owned(),
            Expr::Try(Try { span, .. }) => span.to_owned(),
            Expr::Do(Do { span, .. }) => span.to_owned(),
            Expr::Await(Await { span, .. }) => span.to_owned(),
            Expr::Yield(Yield { span, .. }) => span.to_owned(),
            Expr::JSXElement(JSXElement { span, .. }) => span.to_owned(),
            Expr::JSXFragment(JSXFragment { span, .. }) => span.to_owned(),
        }
    }

    pub fn is_lvalue(&self) -> bool {
        match self {
            Expr::Ident(_) => true,
            Expr::Member(Member { object, .. }) => object.is_lvalue(),
            Expr::Index(Index { left, .. }) => left.is_lvalue(),
            _ => false,
        }
    }
}
