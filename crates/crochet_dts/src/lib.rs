pub mod parse_dts;

#[cfg(test)]
mod tests {
    use crate::parse_dts::parse_dts;

    static LIB_ES5_D_TS: &str = "../../node_modules/typescript/lib/lib.es5.d.ts";

    #[test]
    fn it_works() {
        let dts = parse_dts(LIB_ES5_D_TS);

        println!("{dts:#?}");
    }
}
