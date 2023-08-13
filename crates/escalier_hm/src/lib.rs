// Based on https://github.com/tcr/rust-hindley-milner/blob/master/src/lib.rs
mod ast_utils;
pub mod context;
pub mod errors;
mod folder;
pub mod infer;
mod infer_pattern;
mod key_value_store;
mod provenance;
pub mod types;
mod unify;
pub mod util;
mod visitor;

pub use infer::{infer_expression, infer_program};
