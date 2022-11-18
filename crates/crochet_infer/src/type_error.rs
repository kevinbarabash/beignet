use error_stack::Context;
use std::fmt;

use crochet_ast::types::Type;
use crochet_ast::values::Assign;

#[derive(Debug, PartialEq, Eq)]
pub enum TypeError {
    TODO,
    UnificationError(Box<Type>, Box<Type>),
    UnificationIsUndecidable,
    InfiniteType,

    // async/await
    AwaitOutsideOfAsync,

    // if-else
    ConsequentMustReturnVoid,

    // mutability
    NonMutableBindingAssignment(Box<Assign>),
    UnexpectedImutableValue,

    // tuples and arrays
    InvalidIndex(Box<Type>),
    IndexOutOfBounds(usize, Box<Type>),
    TupleSpreadOutsideTuple, // include types
    NotEnoughElementsToUnpack,

    // property access on objects
    InvalidKey(Box<Type>),
    MissingKey(String),
    NotAnObject(Box<Type>),
    PossiblyNotAnObject(Box<Type>),

    // JSX
    InvalidComponent,

    // functions
    InvalidFix,
    TooFewArguments,

    // Indexed Access Types
    InvalidTypeIndex(Box<Type>),
    MissingTypeIndex,
    NotAnObjectType(Box<Type>),

    // Patterns
    DuplicateIdentInPat(String),

    // Context
    CantFindIdent(String),
    // This was originally "mismatch between the number of qualifiers and type params"
    // TODO: make this error enum better
    TypeInstantiationFailure,
}

impl fmt::Display for TypeError {
    // TODO: print out the provenance chain for types referenced by errors
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "TypeError::")?;
        match self {
            TypeError::AwaitOutsideOfAsync => write!(fmt, "AwaitOutsideOfAsync"),
            TypeError::ConsequentMustReturnVoid => write!(fmt, "ConsequentMustReturnVoid"),
            TypeError::IndexOutOfBounds(index, tuple_t) => {
                write!(fmt, "IndexOutOfBounds: {index} out of bounds for {tuple_t}")
            }
            TypeError::InfiniteType => write!(fmt, "InfiniteType"),
            TypeError::InvalidComponent => write!(fmt, "InvalidComponent"),
            TypeError::InvalidFix => write!(fmt, "InvalidFix"),
            TypeError::InvalidIndex(index) => {
                write!(fmt, "InvalidIndex: {index} is not a valid index")
            }
            TypeError::InvalidKey(key) => write!(fmt, "InvalidKey: {key} is not a valid key"),
            TypeError::InvalidTypeIndex(index) => write!(
                fmt,
                "InvalidTypeIndex: {index} is not a valid index for indexed access types"
            ),
            TypeError::MissingKey(key) => {
                write!(fmt, "MissingKey: object doesn't have a {key} property")
            }
            TypeError::MissingTypeIndex => write!(fmt, "MissingTypeIndex"),
            TypeError::NonMutableBindingAssignment(_) => write!(fmt, "NonMutableBindingAssignment"),
            TypeError::NotAnObject(t) => write!(fmt, "NotAnObject: {t} is not an object"),
            TypeError::NotAnObjectType(t) => {
                write!(fmt, "NotAnObjectType: {t} is not an object type an can't be used in indexed access types")
            }
            TypeError::NotEnoughElementsToUnpack => write!(fmt, "NotEnoughElementsToUnpack"),
            TypeError::PossiblyNotAnObject(t) => {
                write!(fmt, "PossiblyNotAnObject: {t} might not be an object")
            }
            TypeError::TODO => write!(fmt, "TODO"),
            TypeError::TooFewArguments => write!(fmt, "TooFewArguments"),
            TypeError::TupleSpreadOutsideTuple => write!(fmt, "TupleSpreadOutsideTuple"),
            TypeError::UnexpectedImutableValue => write!(fmt, "UnexpectedImutableValue"),
            // TODO: differentiate between sub-typing unification and exact unification
            TypeError::UnificationError(t1, t2) => {
                write!(fmt, "UnificationError: {t1}, {t2}")
            }
            TypeError::UnificationIsUndecidable => write!(fmt, "UnificationIsUndecidable"),
            TypeError::DuplicateIdentInPat(name) => write!(fmt, "DuplicateIdentInPat: {name}"),
            TypeError::CantFindIdent(name) => write!(fmt, "CantFindIdent: {name}"),
            TypeError::TypeInstantiationFailure => write!(fmt, "TypeInstantiationFailure"),
        }
    }
}

impl Context for TypeError {}
