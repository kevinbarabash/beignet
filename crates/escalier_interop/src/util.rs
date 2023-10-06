use std::collections::BTreeMap;
use std::collections::HashSet;

use escalier_hm::checker::Checker;
use escalier_hm::types::{
    Function, MappedType, Object as TObject, Scheme, TMethod, TObjElem, TPat, TProp, TPropKey,
    TypeKind,
};

pub fn new_merge_schemes(schemes: &[Scheme], checker: &mut Checker) -> Scheme {
    let mut elems: Vec<TObjElem> = vec![];

    let type_params = schemes[0].type_params.to_owned();
    for scheme in schemes.iter().skip(1) {
        if scheme.type_params != type_params {
            panic!("Mismatch in type param count when attempting to merge type");
        }
    }

    for scheme in schemes {
        if let TypeKind::Object(obj) = &checker.arena[scheme.t].kind {
            for elem in &obj.elems {
                elems.push(elem.to_owned());
            }
        }
    }

    let t = checker.from_type_kind(TypeKind::Object(TObject { elems }));

    Scheme { t, type_params }
}

pub fn merge_readonly_and_mutable_schemes(
    readonly_scheme: &Scheme,
    mutable_scheme: &Scheme,
    checker: &mut Checker,
) -> Scheme {
    // BTreeMap is used here to ensure that the props are in alphabetical order
    // TODO: figure out how to handle overloads
    let mut methods: BTreeMap<String, TMethod> = BTreeMap::new();
    let mut mutating_methods: HashSet<String> = HashSet::new();

    let mut mapped_types: Vec<MappedType> = vec![];
    let mut callables: Vec<Function> = vec![];
    let mut newables: Vec<Function> = vec![];
    let mut props: Vec<TProp> = vec![];

    // TODO: check that the type params are the same for both schemes
    let type_params = mutable_scheme.type_params.to_owned();

    // the readonly_scheme should have fewer properties than mutable_scheme
    // if there's a property in both scheme's that's marked as `readonly` then
    // we should mark in the merged scheme as `readonly`

    if let TypeKind::Object(TObject { elems }) = &checker.arena[readonly_scheme.t].kind {
        for elem in elems {
            match elem {
                TObjElem::Call(callable) => {
                    callables.push(callable.to_owned());
                }
                TObjElem::Constructor(newable) => {
                    newables.push(newable.to_owned());
                }
                TObjElem::Mapped(mapped_type) => {
                    mapped_types.push(mapped_type.to_owned());
                }
                TObjElem::Method(method) => {
                    let key = match &method.name {
                        TPropKey::StringKey(key) => key,
                        TPropKey::NumberKey(key) => key,
                    };
                    methods.insert(key.to_owned(), method.to_owned());
                }
                TObjElem::Prop(prop) => {
                    props.push(prop.to_owned());
                }
            }
        }
    }

    if let TypeKind::Object(TObject { elems }) = &checker.arena[mutable_scheme.t].kind {
        for elem in elems {
            if let TObjElem::Method(method) = elem {
                let key = match &method.name {
                    TPropKey::StringKey(key) => key,
                    TPropKey::NumberKey(key) => key,
                };

                if !methods.contains_key(key) {
                    mutating_methods.insert(key.to_owned());
                    methods.insert(key.to_owned(), method.to_owned());
                }
            }
        }
    }

    // NOTE: we need to update methods that should have a `mut self` param after
    // iterating over the props in order to avoid borrowing `checker` as mut twice.
    for (key, method) in methods.iter_mut() {
        if mutating_methods.contains(key) {
            method.mutates = true;
        }
    }

    let mut elems: Vec<TObjElem> = vec![];

    for c in callables {
        elems.push(TObjElem::Call(c));
    }

    for n in newables {
        elems.push(TObjElem::Constructor(n));
    }

    for m in mapped_types {
        elems.push(TObjElem::Mapped(m));
    }

    for p in props {
        elems.push(TObjElem::Prop(p));
    }

    for (_, method) in methods {
        elems.push(TObjElem::Method(method));
    }

    let t = checker.from_type_kind(TypeKind::Object(TObject {
        elems,
        // NOTE: This function is called from the code parsing interfaces
        // so this should always be true.
        // TODO: add a check to make sure since we shouldn't be trying to
        // merge things that aren't interfaces.
        // is_interface: true,
    }));

    Scheme { t, type_params }
}

#[cfg(test)]
mod tests {
    use super::*;

    use escalier_ast::script::Script;
    use escalier_hm::context::Context;
    use escalier_parser::*;

    pub fn parse(input: &str) -> Result<Script, ParseError> {
        let mut parser = Parser::new(input);
        parser.parse_script()
    }

    #[test]
    fn if_obj_is_not_mutating_all_methods_are_not_mutating() {
        let src = r#"
        type A = {
            fn foo(self) -> boolean,
        }
        type B = {
            fn bar(self) -> boolean,
        }
        "#;
        let mut checker = Checker::default();
        let mut ctx = Context::default();
        let mut program = parse(src).unwrap();
        checker.infer_script(&mut program, &mut ctx).unwrap();

        let scheme_a = ctx.schemes.get("A").unwrap();
        let scheme_b = ctx.schemes.get("B").unwrap();

        let result = merge_readonly_and_mutable_schemes(scheme_a, scheme_b, &mut checker);

        assert_eq!(
            checker.print_scheme(&result),
            "{bar(mut self) -> boolean, foo(self) -> boolean}"
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
        checker.infer_script(&mut program, &mut ctx).unwrap();

        let scheme_a = ctx.schemes.get("A").unwrap().to_owned();
        let scheme_b = ctx.schemes.get("B").unwrap().to_owned();

        let result = new_merge_schemes(&[scheme_a, scheme_b], &mut checker);

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
        checker.infer_script(&mut program, &mut ctx).unwrap();

        let scheme_a = ctx.schemes.get("A").unwrap().to_owned();
        let scheme_b = ctx.schemes.get("B").unwrap().to_owned();

        let result = new_merge_schemes(&[scheme_a, scheme_b], &mut checker);

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
