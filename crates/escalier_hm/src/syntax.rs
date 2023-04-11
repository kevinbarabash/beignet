use std::fmt;

use crate::literal::Literal;

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
pub struct Lambda {
    pub params: Vec<String>,
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
    pub inferred_type: Option<usize>,
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.kind {
            ExprKind::Lambda(Lambda { params, body }) => {
                let params = params
                    .iter()
                    .map(|param| param.to_string())
                    .collect::<Vec<_>>();

                match body {
                    BlockOrExpr::Block(Block { stmts }) => {
                        let stmts = stmts
                            .iter()
                            .map(|stmt| stmt.to_string())
                            .collect::<Vec<_>>();
                        write!(f, "(fn ({}) => {})", params.join(", "), stmts.join("\n"))
                    }
                    BlockOrExpr::Expr(expr) => {
                        write!(f, "(fn ({}) => {expr})", params.join(", "))
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
                write!(f, "(letrec ")?;
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
                write!(f, "(if {cond} then {consequent} else {alternate})",)
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
    pub var: String,
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
            Statement::Declaration(Declaration { var, defn }) => {
                write!(f, "let {var} = {defn}")
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
