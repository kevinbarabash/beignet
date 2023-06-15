use crate::pattern::Pattern;
use crate::type_ann::TypeAnn;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FuncParam {
    pub pattern: Pattern,
    pub type_ann: Option<TypeAnn>,
    pub optional: bool,
}
