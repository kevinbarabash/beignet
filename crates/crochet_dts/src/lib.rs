pub mod parse_dts;

#[cfg(test)]
mod tests {
    use crate::parse_dts::parse_dts;

    static LIB_ES5_D_TS: &str = "../../node_modules/typescript/lib/lib.es5.d.ts";

    #[test]
    fn parsing_lib_es5_d_ts() {
        let ctx = parse_dts(LIB_ES5_D_TS).unwrap();

        for (name, scheme) in ctx.types {
            println!("{name} = {scheme}")
        }
    }
}
