use std::fmt;

use escalier_ast::types::Type;
use escalier_ast::values::{Assign, Statement};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TypeError {
    UnificationError(Box<Type>, Box<Type>),
    UnificationIsUndecidable,
    Unspecified,
    InfiniteType,
    PrimitivesCantBeMutable(Box<Type>),
    TuplesCantBeMutable(Box<Type>),

    // Async/Await
    AwaitOutsideOfAsync,

    // if-else
    ConsequentMustReturnVoid,

    // Mutability
    NonMutableBindingAssignment(Box<Assign<Type>>),
    UnexpectedImutableValue,
    ObjectIsNotMutable,
    PropertyIsNotMutable,

    // Tuples and Arrays
    InvalidIndex(Box<Type>, Box<Type>),
    IndexOutOfBounds(Box<Type>, Box<Type>),
    TupleSpreadOutsideTuple, // include types
    NotEnoughElementsToUnpack,
    MoreThanOneRestPattern,

    // Property access on objects
    InvalidKey(Box<Type>),
    MissingKey(String),
    NotAnObject(Box<Type>),
    PossiblyNotAnObject(Box<Type>),

    // Function calls
    ObjectIsNotCallable(Box<Type>),
    NoValidCallable,
    NoValidOverload,
    InvalidSpread(Box<Type>),
    TooFewArguments(Box<Type>, Box<Type>), // application, lambda

    // Recursive functions
    InvalidFix,

    // JSX
    InvalidComponent,

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

    // Types
    MissingTypeAnnotation(Box<Statement<Type>>),
    AliasTypeMismatch,

    // Objects
    CantInferTypeFromItKeys,

    // Classes
    MethodsMustHaveTypes,
    PropertiesMustHaveTypes,
}

impl fmt::Display for TypeError {
    // TODO: print out the provenance chain for types referenced by errors
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "TypeError::")?;
        match self {
            TypeError::AwaitOutsideOfAsync => write!(fmt, "AwaitOutsideOfAsync"),
            TypeError::ConsequentMustReturnVoid => write!(fmt, "ConsequentMustReturnVoid"),
            TypeError::IndexOutOfBounds(tuple_t, index) => {
                write!(fmt, "IndexOutOfBounds: {index} out of bounds for {tuple_t}")
            }
            TypeError::InfiniteType => write!(fmt, "InfiniteType"),
            TypeError::InvalidComponent => write!(fmt, "InvalidComponent"),
            TypeError::InvalidFix => write!(fmt, "InvalidFix"),
            TypeError::InvalidIndex(obj, index) => {
                write!(fmt, "InvalidIndex: {index} is not a valid index on {obj}")
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
            TypeError::ObjectIsNotMutable => write!(fmt, "ObjectIsNotMutable"),
            TypeError::PropertyIsNotMutable => write!(fmt, "PropertyIsNotMutable"),
            TypeError::NotAnObject(t) => write!(fmt, "NotAnObject: {t} is not an object"),
            TypeError::NotAnObjectType(t) => {
                write!(fmt, "NotAnObjectType: {t} is not an object type an can't be used in indexed access types")
            }
            TypeError::NotEnoughElementsToUnpack => write!(fmt, "NotEnoughElementsToUnpack"),
            TypeError::PossiblyNotAnObject(t) => {
                write!(fmt, "PossiblyNotAnObject: {t} might not be an object")
            }
            TypeError::TooFewArguments(_, _) => write!(fmt, "TooFewArguments"),
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
            TypeError::MoreThanOneRestPattern => write!(fmt, "MoreThanOneRestPattern"),
            TypeError::ObjectIsNotCallable(obj) => write!(fmt, "ObjectIsNotCallable: {obj}"),
            TypeError::NoValidCallable => write!(fmt, "NoValidCallable"),
            TypeError::NoValidOverload => write!(fmt, "NoValidOverload"),
            TypeError::InvalidSpread(spread) => write!(fmt, "InvalidSpread: {spread}"),
            TypeError::MissingTypeAnnotation(_) => {
                // TODO: include information about the statement in the error message
                write!(fmt, "MissingTypeAnnotation")
            }
            TypeError::AliasTypeMismatch => write!(fmt, "AliasTypeMismatch"),
            TypeError::CantInferTypeFromItKeys => write!(fmt, "CantInferTypeFromItKeys"),
            TypeError::Unspecified => write!(fmt, "Unspecified"),
            TypeError::PrimitivesCantBeMutable(t) => write!(fmt, "PrimitivesCantBeMutable: {t}"),
            TypeError::TuplesCantBeMutable(t) => write!(fmt, "TuplesCantBeMutable: {t}"),
            TypeError::MethodsMustHaveTypes => write!(fmt, "MethodsMustHaveTypes"),
            TypeError::PropertiesMustHaveTypes => write!(fmt, "PropertiesMustHaveTypes"),
        }
    }
}
