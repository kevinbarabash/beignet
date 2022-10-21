use std::collections::HashMap;

use crochet_ast::types::Type;

// NOTE: This is the same as the Env type in context.rs
pub type Assump = HashMap<String, Type>;
