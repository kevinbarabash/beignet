use std::fmt;

use crate::literal::Literal;
use crate::types::ArenaType;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct BindingIdent {
    pub name: String,
    pub mutable: bool,
    // pub span: Span,
    // pub loc: SourceLocation,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RestPat {
    pub arg: Box<Pattern>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ObjectPat {
    pub props: Vec<ObjectPatProp>,
    pub optional: bool,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ObjectPatProp {
    KeyValue(KeyValuePatProp),
    Shorthand(ShorthandPatProp),
    Rest(RestPat), // TODO: create a new RestPatProp that includes a span
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct KeyValuePatProp {
    // pub loc: SourceLocation,
    // pub span: Span,
    pub key: Identifier,
    pub value: Box<Pattern>,
    pub init: Option<Box<Expression>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ShorthandPatProp {
    // pub loc: SourceLocation,
    // pub span: Span,
    pub ident: BindingIdent,
    pub init: Option<Box<Expression>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ArrayPat {
    // The elements are optional to support sparse arrays.
    pub elems: Vec<Option<ArrayPatElem>>,
    pub optional: bool,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ArrayPatElem {
    // TODO: add .span property
    pub pattern: Pattern,
    pub init: Option<Box<Expression>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LitPat {
    pub lit: Literal,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct IsPat {
    pub ident: BindingIdent,
    pub is_id: Identifier,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum PatternKind {
    Ident(BindingIdent),
    Rest(RestPat),
    Object(ObjectPat),
    Array(ArrayPat),
    Lit(LitPat),
    Is(IsPat),
    Wildcard,
    // This can't be used at the top level similar to rest
    // Assign(AssignPat),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Pattern {
    // pub loc: SourceLocation,
    // pub span: Span,
    pub kind: PatternKind,
    pub inferred_type: Option<ArenaType>,
}

impl fmt::Display for Pattern {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.kind {
            PatternKind::Ident(BindingIdent { name, mutable: _ }) => write!(f, "{name}"),
            PatternKind::Rest(RestPat { arg }) => write!(f, "...{arg}"),
            PatternKind::Object(ObjectPat { props, optional }) => todo!(),
            PatternKind::Array(ArrayPat { elems, optional }) => todo!(),
            PatternKind::Lit(LitPat { lit }) => write!(f, "{lit}"),
            PatternKind::Is(IsPat { ident, is_id }) => todo!(),
            PatternKind::Wildcard => write!(f, "_"),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Block {
    pub stmts: Vec<Statement>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum BlockOrExpr {
    Block(Block),
    Expr(Box<Expression>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FuncParam {
    pub pattern: Pattern,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Lambda {
    pub params: Vec<FuncParam>,
    pub body: BlockOrExpr,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Identifier {
    pub name: String,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Tuple {
    pub elems: Vec<Expression>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Object {
    pub props: Vec<(String, Expression)>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Apply {
    pub func: Box<Expression>,
    pub args: Vec<Expression>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Let {
    pub var: String,
    pub defn: Box<Expression>,
    pub body: Box<Expression>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Letrec {
    pub decls: Vec<(String, Box<Expression>)>, // (var, defn)
    pub body: Box<Expression>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct IfElse {
    pub cond: Box<Expression>,
    pub consequent: Box<Expression>,
    pub alternate: Box<Expression>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Member {
    pub obj: Box<Expression>,
    pub prop: Box<Expression>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ExprKind {
    Lambda(Lambda),
    Identifier(Identifier),
    Literal(Literal),
    Tuple(Tuple),
    Object(Object),
    Apply(Apply),
    Letrec(Letrec),
    IfElse(IfElse),
    Member(Member),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Expression {
    pub kind: ExprKind,
    pub inferred_type: Option<ArenaType>,
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.kind {
            ExprKind::Lambda(Lambda { params, body }) => {
                let params = params
                    .iter()
                    .map(|param| param.pattern.to_string())
                    .collect::<Vec<_>>();

                match body {
                    BlockOrExpr::Block(Block { stmts }) => {
                        let stmts = stmts
                            .iter()
                            .map(|stmt| stmt.to_string())
                            .collect::<Vec<_>>();
                        write!(f, "fn ({}) => {{{}}}", params.join(", "), stmts.join("\n"))
                    }
                    BlockOrExpr::Expr(expr) => {
                        write!(f, "fn ({}) => {expr})", params.join(", "))
                    }
                }
            }
            ExprKind::Identifier(Identifier { name }) => {
                write!(f, "{}", name)
            }
            ExprKind::Literal(literal) => {
                write!(f, "{literal}")
            }
            ExprKind::Apply(Apply { func, args }) => {
                let args = args.iter().map(|arg| arg.to_string()).collect::<Vec<_>>();
                write!(f, "{func}({})", args.join(", "))
            }
            ExprKind::Letrec(Letrec { decls, body }) => {
                write!(f, "let rec ")?;
                let decls = decls
                    .iter()
                    .map(|(var, defn)| format!("{} = {}", var, defn))
                    .collect::<Vec<_>>();
                write!(f, "{}", decls.join(" and "))?;
                write!(f, " in {body})")
            }
            ExprKind::IfElse(IfElse {
                cond,
                consequent,
                alternate,
            }) => {
                write!(f, "if ({cond}) then {{{consequent}}} else {{{alternate}}}")
            }
            ExprKind::Member(Member { obj, prop }) => {
                write!(f, "{obj}.{prop}")
            }
            ExprKind::Tuple(Tuple { elems }) => {
                let elems = elems
                    .iter()
                    .map(|elem| elem.to_string())
                    .collect::<Vec<_>>();
                write!(f, "[{}]", elems.join(", "))
            }
            ExprKind::Object(Object { props }) => {
                let props = props
                    .iter()
                    .map(|(key, value)| format!("{}: {}", key, value))
                    .collect::<Vec<_>>();
                write!(f, "{{{}}}", props.join(", "))
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Declaration {
    pub pattern: Pattern,
    pub defn: Box<Expression>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Return {
    pub expr: Box<Expression>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Statement {
    Declaration(Declaration),
    Expression(Expression),
    Return(Return),
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Statement::Declaration(Declaration { pattern, defn }) => {
                write!(f, "let {pattern} = {defn}")
            }
            Statement::Expression(expr) => write!(f, "{expr}"),
            Statement::Return(Return { expr }) => write!(f, "return {expr}"),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Program {
    pub statements: Vec<Statement>,
}
