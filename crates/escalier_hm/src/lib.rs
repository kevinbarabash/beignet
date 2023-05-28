// Based on https://github.com/tcr/rust-hindley-milner/blob/master/src/lib.rs
pub mod ast;
pub mod context;
pub mod errors;
pub mod infer;
mod infer_pattern;
pub mod parser;
pub mod types;
mod unify;
pub mod util;
mod visitor;

pub use infer::{infer_expression, infer_program};
pub use parser::parse;
