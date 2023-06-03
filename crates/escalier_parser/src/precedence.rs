use lazy_static::lazy_static;
use std::collections::hash_map::HashMap;

type Precedence = u8;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Associativity {
    Left,
    Right,
    NotApplicable,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Operator {
    // 18
    Grouping,

    // 17
    MemberAccess,
    OptionalChaining,
    ComputedMemberAccess,
    NewWithArgumentList,
    FunctionCall,

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
    // Conditional, // ternary
    Arrow,
    Yield,
    YieldStar,
    Spread,
    // 1
    // CommaSequence,
}

lazy_static! {
    pub static ref PRECEDENCE_TABLE: HashMap<Operator, (Precedence, Associativity)> = {
        let mut table = HashMap::new();

        table.insert(Operator::Grouping, (18, Associativity::NotApplicable));

        table.insert(Operator::MemberAccess, (17, Associativity::Left));
        table.insert(Operator::OptionalChaining, (17, Associativity::Left));
        table.insert(
            Operator::ComputedMemberAccess,
            (17, Associativity::NotApplicable),
        );
        table.insert(
            Operator::NewWithArgumentList,
            (17, Associativity::NotApplicable),
        );
        table.insert(Operator::FunctionCall, (17, Associativity::NotApplicable));

        table.insert(Operator::LogicalNot, (14, Associativity::NotApplicable));
        table.insert(Operator::UnaryPlus, (14, Associativity::NotApplicable));
        table.insert(Operator::UnaryMinus, (14, Associativity::NotApplicable));
        table.insert(Operator::Typeof, (14, Associativity::NotApplicable));
        table.insert(Operator::Void, (14, Associativity::NotApplicable));
        table.insert(Operator::Delete, (14, Associativity::NotApplicable));
        table.insert(Operator::Await, (14, Associativity::NotApplicable));

        table.insert(Operator::Exponentiation, (13, Associativity::Right));

        table.insert(Operator::Multiplication, (12, Associativity::Left));
        table.insert(Operator::Division, (12, Associativity::Left));
        table.insert(Operator::Remainder, (12, Associativity::Left));

        table.insert(Operator::Addition, (11, Associativity::Left));
        table.insert(Operator::Subtraction, (11, Associativity::Left));

        table.insert(Operator::LessThan, (9, Associativity::Left));
        table.insert(Operator::LessThanOrEqual, (9, Associativity::Left));
        table.insert(Operator::GreaterThan, (9, Associativity::Left));
        table.insert(Operator::GreaterThanOrEqual, (9, Associativity::Left));
        table.insert(Operator::In, (9, Associativity::Left));
        table.insert(Operator::Instanceof, (9, Associativity::Left));

        table.insert(Operator::Equals, (8, Associativity::Left));
        table.insert(Operator::NotEquals, (8, Associativity::Left));

        table.insert(Operator::LogicalAnd, (4, Associativity::Left));

        table.insert(Operator::LogicalOr, (3, Associativity::Left));
        table.insert(Operator::NullishCoalescing, (3, Associativity::Left));

        table.insert(Operator::Assignment, (2, Associativity::Right));
        table.insert(Operator::Arrow, (2, Associativity::Right));
        table.insert(Operator::Yield, (2, Associativity::Right));
        table.insert(Operator::YieldStar, (2, Associativity::Right));
        table.insert(Operator::Spread, (2, Associativity::Right));

        table
    };
}
