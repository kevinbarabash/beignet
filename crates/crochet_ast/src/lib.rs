pub mod expr;
pub mod ident;
pub mod jsx;
pub mod literal;
pub mod pattern;
pub mod span;
pub mod types;
pub mod prim;

pub use expr::*;
pub use ident::*;
pub use jsx::*;
pub use literal::*;
pub use pattern::*;
pub use span::*;
pub use types::*;
pub use prim::*;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        let result = 2 + 2;
        assert_eq!(result, 4);
    }
}
