use crate::literal::Literal;
use crate::pattern::Pattern;
use crate::source_location::SourceLocation;
use crate::stmt::Stmt;

// TODO: track source location
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ObjectKey {
    Identifier(String), // TODO: use Identifier
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
pub enum ExprKind {
    Identifier(String),
    Literal(Literal),
    TemplateLiteral {
        parts: Vec<Literal>,
        exprs: Vec<Expr>,
    },
    Object {
        properties: Vec<PropOrSpread>,
    },
    Tuple {
        elements: Vec<ExprOrSpread>,
    },
    Binary {
        left: Box<Expr>,
        op: BinaryOp,
        right: Box<Expr>,
    },
    Unary {
        op: UnaryOp,
        right: Box<Expr>,
    },
    Index {
        left: Box<Expr>,
        right: Box<Expr>,
    },
    Function {
        // TODO: add support for explicit type annotations
        params: Vec<Pattern>,
        body: BlockOrExpr,
    },
    Call {
        args: Vec<Expr>,
        callee: Box<Expr>,
    },
    Member {
        object: Box<Expr>,
        property: Box<Expr>,
    },
    IfElse {
        cond: Box<Expr>,
        consequent: Block,
        alternate: Option<Block>,
    },
    Match {
        expr: Box<Expr>,
        arms: Vec<MatchArm>,
    },
    Try {
        body: Block,
        catch: CatchClause,
        finally: Option<Block>,
    },
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct CatchClause {
    pub param: Option<Pattern>,
    pub body: Block,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct MatchArm {
    pub loc: SourceLocation,
    // pub span: Span,
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
    pub loc: SourceLocation,
    pub stmts: Vec<Stmt>,
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

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Expr {
    pub kind: ExprKind,
    pub loc: SourceLocation,
}
