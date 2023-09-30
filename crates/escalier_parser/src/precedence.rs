use lazy_static::lazy_static;
use std::collections::hash_map::HashMap;

pub type Precedence = u32;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Associativity {
    Left,
    Right,
    Neither, // indicates a unary operator
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Operator {
    // 18
    // Grouping, - parsed as an atom

    // 17
    MemberAccess,
    OptionalChaining,
    ComputedMemberAccess,
    NewWithArgumentList,
    FunctionCall,
    TemplateLiteral,

    // 16
    // NewWithoutArgList,

    // 15
    // PostfixIncrement,
    // PostfixDecrement,

    // 14
    LogicalNot,
    // BitwiseNot,
    UnaryPlus,
    UnaryMinus,
    // PrefixIncrement,
    // PrefixDecrement,
    Typeof,
    Void,
    Delete,
    Await,
    Throw,

    // 13
    Exponentiation,

    // 12
    Multiplication,
    Division,
    Remainder,

    // 11
    Addition,
    Subtraction,

    // 10
    // BitwiseLeftShift,
    // BitwiseRightShift,
    // BitwiseUnsignedRightShift,

    // 9
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
    In,
    Instanceof,

    // 8
    Equals,    // always strict
    NotEquals, // always strict
    // StrictEquals,
    // StrictNotEquals,

    // 7
    // BitwiseAnd,

    // 6
    // BitwiseXor,

    // 5
    // BitwiseOr,

    // 4
    LogicalAnd,

    // 3
    LogicalOr,
    NullishCoalescing,

    // 2
    Assignment,
    Conditional,
    // Arrow, - superseded by `fn`
    Yield,
    YieldStar,
    Spread,
    // 1
    // CommaSequence,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct OpInfo {
    prec: Precedence,
    assoc: Associativity,
}

impl OpInfo {
    pub fn new_prefix(prec: Precedence) -> Self {
        Self {
            prec,
            assoc: Associativity::Neither,
        }
    }

    pub fn new_infix(prec: Precedence, assoc: Associativity) -> Self {
        Self { prec, assoc }
    }

    pub fn new_postfix(prec: Precedence) -> Self {
        Self {
            prec,
            assoc: Associativity::Neither,
        }
    }

    pub fn normalized_prec(&self) -> Precedence {
        self.prec * 10
    }

    pub fn infix_postfix_prec(&self) -> Precedence {
        let normalized = self.normalized_prec();
        match self.assoc {
            Associativity::Left => normalized,
            Associativity::Right => normalized - 1,
            Associativity::Neither => normalized + 1,
        }
    }
}

lazy_static! {
    pub static ref PRECEDENCE_TABLE: HashMap<Operator, OpInfo> = {
        let mut table: HashMap<Operator, OpInfo> = HashMap::new();

        table.insert(
            Operator::MemberAccess,
            OpInfo::new_infix(17, Associativity::Left),
        );
        table.insert(
            Operator::OptionalChaining,
            OpInfo::new_infix(17, Associativity::Left),
        );
        table.insert(Operator::ComputedMemberAccess, OpInfo::new_postfix(17));
        table.insert(Operator::NewWithArgumentList, OpInfo::new_postfix(17));
        table.insert(Operator::FunctionCall, OpInfo::new_postfix(17));
        table.insert(Operator::TemplateLiteral, OpInfo::new_postfix(17));

        table.insert(Operator::LogicalNot, OpInfo::new_prefix(14));
        table.insert(Operator::UnaryPlus, OpInfo::new_prefix(14));
        table.insert(Operator::UnaryMinus, OpInfo::new_prefix(14));
        table.insert(Operator::Typeof, OpInfo::new_prefix(14));
        table.insert(Operator::Void, OpInfo::new_prefix(14));
        table.insert(Operator::Delete, OpInfo::new_prefix(14));
        table.insert(Operator::Await, OpInfo::new_prefix(14));
        table.insert(Operator::Throw, OpInfo::new_prefix(14));

        table.insert(
            Operator::Exponentiation,
            OpInfo::new_infix(13, Associativity::Right),
        );

        table.insert(
            Operator::Multiplication,
            OpInfo::new_infix(12, Associativity::Left),
        );
        table.insert(
            Operator::Division,
            OpInfo::new_infix(12, Associativity::Left),
        );
        table.insert(
            Operator::Remainder,
            OpInfo::new_infix(12, Associativity::Left),
        );

        table.insert(
            Operator::Addition,
            OpInfo::new_infix(11, Associativity::Left),
        );
        table.insert(
            Operator::Subtraction,
            OpInfo::new_infix(11, Associativity::Left),
        );

        table.insert(
            Operator::LessThan,
            OpInfo::new_infix(9, Associativity::Left),
        );
        table.insert(
            Operator::LessThanOrEqual,
            OpInfo::new_infix(9, Associativity::Left),
        );
        table.insert(
            Operator::GreaterThan,
            OpInfo::new_infix(9, Associativity::Left),
        );
        table.insert(
            Operator::GreaterThanOrEqual,
            OpInfo::new_infix(9, Associativity::Left),
        );
        table.insert(Operator::In, OpInfo::new_infix(9, Associativity::Left));
        table.insert(
            Operator::Instanceof,
            OpInfo::new_infix(9, Associativity::Left),
        );

        table.insert(Operator::Equals, OpInfo::new_infix(8, Associativity::Left));
        table.insert(
            Operator::NotEquals,
            OpInfo::new_infix(8, Associativity::Left),
        );

        table.insert(
            Operator::LogicalAnd,
            OpInfo::new_infix(4, Associativity::Left),
        );

        table.insert(
            Operator::LogicalOr,
            OpInfo::new_infix(3, Associativity::Left),
        );
        table.insert(
            Operator::NullishCoalescing,
            OpInfo::new_infix(3, Associativity::Left),
        );

        table.insert(
            Operator::Assignment,
            OpInfo::new_infix(2, Associativity::Neither),
        );
        table.insert(
            Operator::Conditional,
            OpInfo::new_infix(2, Associativity::Left),
        );
        table.insert(Operator::Yield, OpInfo::new_infix(2, Associativity::Right));
        table.insert(
            Operator::YieldStar,
            OpInfo::new_infix(2, Associativity::Right),
        );
        table.insert(Operator::Spread, OpInfo::new_infix(2, Associativity::Right));

        table
    };
}
