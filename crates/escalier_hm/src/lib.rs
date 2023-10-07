// Based on https://github.com/tcr/rust-hindley-milner/blob/master/src/lib.rs
mod ast_utils;
mod folder;
mod infer_class;
mod infer_pattern;
mod key_value_store;
mod provenance;
mod unify;
mod visitor;

pub mod checker;
pub mod context;
pub mod diagnostic;
pub mod infer;
pub mod type_error;
pub mod types;
pub mod util;
