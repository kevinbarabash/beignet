use std::collections::BTreeMap;

use escalier_hm::checker::Checker;
use escalier_hm::types::{Object as TObject, Scheme, TObjElem, TPropKey, TypeKind};

pub fn merge_schemes(old_scheme: &Scheme, new_scheme: &Scheme, checker: &mut Checker) -> Scheme {
    let type_params_1 = &old_scheme.type_params;
    let type_params_2 = &new_scheme.type_params;

    let type_params = match (type_params_1, type_params_2) {
        (None, None) => None,
        (None, Some(_)) => panic!("Mismatch in type param count when attempting to merge type"),
        (Some(_), None) => panic!("Mismatch in type param count when attempting to merge type"),
        (Some(type_params_1), Some(type_params_2)) => {
            if type_params_1.len() != type_params_2.len() {
                panic!("Mismatch in type param count when attempting to merge type");
            }

            // TODO: check if the type params are in the same order
            for (type_param_1, type_param_2) in type_params_1.iter().zip(type_params_2.iter()) {
                if type_param_1.name != type_param_2.name {
                    panic!("Type params must have the same names when merging schemes");
                }
            }

            Some(type_params_1.to_owned())
        }
    };

    let t = match (
        &checker.arena[old_scheme.t].kind,
        &checker.arena[new_scheme.t].kind,
    ) {
        (TypeKind::Object(old_obj), TypeKind::Object(new_obj)) => {
            // TODO: Handle function overloads.  `map` only allows a single entry
            // per key which means that we end up with only the first declaration
            // instead of all of them.  This is complicated by the fact that the
            // Readonly variant may define the methods in slightly different ways.
            // See `reduce` in `Array` and `ReadonlyArray`.  The callback is
            // passed `T[]` in the first on and `readonly T[]` in the second one.
            let mut map: BTreeMap<String, TObjElem> = BTreeMap::new();

            let mut calls: Vec<TObjElem> = vec![];
            let mut constructors: Vec<TObjElem> = vec![];
            let mut mapped_types: Vec<TObjElem> = vec![];

            for elem in &old_obj.elems {
                match elem {
                    TObjElem::Call(_) => {
                        calls.push(elem.to_owned());
                    }
                    TObjElem::Constructor(_) => {
                        constructors.push(elem.to_owned());
                    }
                    TObjElem::Mapped(_) => {
                        mapped_types.push(elem.to_owned());
                    }
                    TObjElem::Prop(prop) => {
                        let key = match &prop.name {
                            TPropKey::StringKey(key) => key.to_owned(),
                            TPropKey::NumberKey(key) => key.to_owned(),
                        };
                        map.insert(key, elem.to_owned());
                    }
                }
            }

            for elem in &new_obj.elems {
                match elem {
                    TObjElem::Call(_) => {
                        calls.push(elem.to_owned());
                    }
                    TObjElem::Constructor(_) => {
                        constructors.push(elem.to_owned());
                    }
                    TObjElem::Mapped(_) => {
                        mapped_types.push(elem.to_owned());
                    }
                    TObjElem::Prop(prop) => {
                        let key = match &prop.name {
                            TPropKey::StringKey(key) => key.to_owned(),
                            TPropKey::NumberKey(key) => key.to_owned(),
                        };
                        match map.get(&key) {
                            Some(old_elem) => {
                                // NOTE: Properties can be marked as `readonly`
                                // in TS interfaces.  If the old properties is
                                // not mutable, but the new one is, we use the
                                // new one.
                                if let TObjElem::Prop(old_prop) = old_elem {
                                    if !old_prop.mutable && prop.mutable {
                                        map.insert(key, elem.to_owned());
                                    }
                                }
                            }
                            None => {
                                map.insert(key, elem.to_owned());
                            }
                        };
                    }
                }
            }

            let mut elems: Vec<TObjElem> = vec![];

            // TODO: Dedupe these while handling overloads.
            elems.append(&mut calls);
            elems.append(&mut constructors);
            elems.append(&mut mapped_types);

            for (_, elem) in map {
                elems.push(elem.to_owned());
            }

            checker.from_type_kind(TypeKind::Object(TObject {
                elems,
                // NOTE: This function is called from the code parsing interfaces
                // so this should always be true.
                // TODO: add a check to make sure since we shouldn't be trying to
                // merge things that aren't interfaces.
                // is_interface: true,
            }))
        }
        (_, _) => todo!(),
    };

    Scheme { t, type_params }
}

#[cfg(test)]
mod tests {
    use super::*;

    use escalier_ast::program::Program;
    use escalier_hm::context::Context;
    use escalier_parser::*;

    pub fn parse(input: &str) -> Result<Program, ParseError> {
        let mut parser = Parser::new(input);
        parser.parse_program()
    }

    #[test]
    fn if_obj_is_not_mutating_all_methods_are_not_mutating() {
        let src = r#"
        type A = {
            foo: fn (self) -> boolean,
        }
        type B = {
            bar: fn (self) -> boolean,
        }
        "#;
        let mut checker = Checker::default();
        let mut ctx = Context::default();
        let mut program = parse(src).unwrap();
        checker.infer_program(&mut program, &mut ctx).unwrap();

        let scheme_a = ctx.schemes.get("A").unwrap();
        let scheme_b = ctx.schemes.get("B").unwrap();

        let result = merge_schemes(scheme_a, scheme_b, &mut checker);

        assert_eq!(
            checker.print_scheme(&result),
            "{bar: (self: Self) -> boolean, foo: (self: Self) -> boolean}"
        );
    }

    #[test]
    fn new_is_mutating_and_old_is_mutating_with_different_method_names() {
        let src = r#"
        type A = {
            foo: fn (mut self) -> boolean,
        }
        type B = {
            bar: fn (mut self) -> boolean,
        }
        "#;
        let mut checker = Checker::default();
        let mut ctx = Context::default();
        let mut program = parse(src).unwrap();
        checker.infer_program(&mut program, &mut ctx).unwrap();

        let scheme_a = ctx.schemes.get("A").unwrap();
        let scheme_b = ctx.schemes.get("B").unwrap();

        let result = merge_schemes(scheme_a, scheme_b, &mut checker);

        assert_eq!(
            checker.print_scheme(&result),
            "{bar: (mut self: Self) -> boolean, foo: (mut self: Self) -> boolean}"
        );
    }

    #[test]
    fn old_is_mutating_and_new_is_not_mutating_with_same_method_names() {
        let src = r#"
        type A = {
            foo: fn (mut self) -> boolean,
        }
        type B = {
            foo: fn (self) -> boolean,
        }
        "#;
        let mut checker = Checker::default();
        let mut ctx = Context::default();
        let mut program = parse(src).unwrap();
        checker.infer_program(&mut program, &mut ctx).unwrap();

        let scheme_a = ctx.schemes.get("A").unwrap();
        let scheme_b = ctx.schemes.get("B").unwrap();

        let result = merge_schemes(scheme_a, scheme_b, &mut checker);

        assert_eq!(
            checker.print_scheme(&result),
            "{foo: (mut self: Self) -> boolean}"
        );
    }

    #[test]
    fn old_is_not_mutating_and_new_is_mutating_with_same_method_names() {
        let src = r#"
        type A = {
            foo: fn (self) -> boolean,
        }
        type B = {
            foo: fn (mut self) -> boolean,
        }
        "#;
        let mut checker = Checker::default();
        let mut ctx = Context::default();
        let mut program = parse(src).unwrap();
        checker.infer_program(&mut program, &mut ctx).unwrap();

        let scheme_a = ctx.schemes.get("A").unwrap();
        let scheme_b = ctx.schemes.get("B").unwrap();

        let result = merge_schemes(scheme_a, scheme_b, &mut checker);

        assert_eq!(
            checker.print_scheme(&result),
            "{foo: (self: Self) -> boolean}"
        );
    }

    #[test]
    fn multiple_mapped_types() {
        let src = r#"
        type A = {
            [P]: string for P in number,
        }
        type B = {
            [P]: number for P in string,
        }
        "#;
        let mut checker = Checker::default();
        let mut ctx = Context::default();
        let mut program = parse(src).unwrap();
        checker.infer_program(&mut program, &mut ctx).unwrap();

        let scheme_a = ctx.schemes.get("A").unwrap();
        let scheme_b = ctx.schemes.get("B").unwrap();

        let result = merge_schemes(scheme_a, scheme_b, &mut checker);

        assert_eq!(
            checker.print_scheme(&result),
            "{[P]: string for P in number, [P]: number for P in string}"
        );
    }

    #[test]
    fn callables() {
        let src = r#"
        type A = {
            fn () -> boolean,
        }
        type B = {
            fn () -> boolean,
        }
        "#;
        let mut checker = Checker::default();
        let mut ctx = Context::default();
        let mut program = parse(src).unwrap();
        checker.infer_program(&mut program, &mut ctx).unwrap();

        let scheme_a = ctx.schemes.get("A").unwrap();
        let scheme_b = ctx.schemes.get("B").unwrap();

        let result = merge_schemes(scheme_a, scheme_b, &mut checker);

        assert_eq!(
            checker.print_scheme(&result),
            "{fn() -> boolean, fn() -> boolean}"
        );
    }

    // #[test]
    // fn constructors() {
    //     let mut checker = Checker::default();
    //     let old_elems = vec![TObjElem::Constructor(TCallable {
    //         params: vec![],
    //         ret: Box::from(checker.from_type_kind(TypeKind::Keyword(TKeyword::Boolean))),
    //         type_params: None,
    //     })];
    //     let old_scheme = Scheme {
    //         t: Box::from(checker.from_type_kind(TypeKind::Object(TObject {
    //             elems: old_elems,
    //             is_interface: true,
    //         }))),
    //         type_params: None,
    //     };
    //     let new_elems = vec![TObjElem::Constructor(TCallable {
    //         params: vec![],
    //         ret: Box::from(checker.from_type_kind(TypeKind::Keyword(TKeyword::Boolean))),
    //         type_params: None,
    //     })];
    //     let new_scheme = Scheme {
    //         t: Box::from(checker.from_type_kind(TypeKind::Object(TObject {
    //             elems: new_elems,
    //             is_interface: true,
    //         }))),
    //         type_params: None,
    //     };

    //     let result = merge_schemes(&old_scheme, &new_scheme, &mut checker);
    //     assert_eq!(result.to_string(), "{new () => boolean, new () => boolean}");
    // }
}
