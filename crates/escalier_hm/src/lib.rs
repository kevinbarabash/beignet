// Based on https://github.com/tcr/rust-hindley-milner/blob/master/src/lib.rs
mod literal;
mod syntax;
mod types;

use std::collections::{HashMap, HashSet};

use crate::literal::*;
use crate::syntax::*;
use crate::types::*;

#[derive(Debug)]
pub enum Errors {
    InferenceError(String),
    ParseError(String),
}

// Type inference machinery

#[derive(Clone, Debug, Default)]
pub struct Env(HashMap<String, ArenaType>);

/// Computes the type of the expression given by node.
///
/// The type of the node is computed in the context of the
/// supplied type environment env. Data types can be introduced into the
/// language simply by having a predefined set of identifiers in the initial
/// environment. environment; this way there is no need to change the syntax or, more
/// importantly, the type-checking program when extending the language.
///
/// Args:
///     node: The root of the abstract syntax tree.
///     env: The type environment is a mapping of expression identifier names
///         to type assignments.
///     non_generic: A set of non-generic variables, or None
///
/// Returns:
///     The computed type of the expression.
///
/// Raises:
///     InferenceError: The type of the expression could not be inferred, for example
///         if it is not possible to unify two types such as Integer and Bool
///     ParseError: The abstract syntax tree rooted at node could not be parsed
pub fn analyse(
    a: &mut Vec<Type>,
    node: &Syntax,
    env: &Env,
    non_generic: &HashSet<ArenaType>,
) -> Result<ArenaType, Errors> {
    match node {
        Syntax::Identifier(Identifier { name }) => get_type(a, name, env, non_generic),
        Syntax::Literal(literal) => {
            let t = new_lit_type(a, literal);
            Ok(t)
        }
        Syntax::Apply(Apply { func, args }) => {
            let func_type = analyse(a, func, env, non_generic)?;
            let arg_types = args
                .iter()
                .map(|arg| analyse(a, arg, env, non_generic))
                .collect::<Result<Vec<_>, _>>()?;
            let result_type = new_var_type(a);
            let call_type = new_call_type(a, &arg_types, result_type);
            unify(a, call_type, func_type)?;
            Ok(result_type)
        }
        Syntax::Lambda(Lambda { params, body }) => {
            let mut param_types = vec![];
            let mut new_env = env.clone();
            let mut new_non_generic = non_generic.clone();
            for param in params {
                let arg_type = new_var_type(a);
                new_env.0.insert(param.clone(), arg_type);
                new_non_generic.insert(arg_type);
                param_types.push(arg_type);
            }
            let result_type = analyse(a, body, &new_env, &new_non_generic)?;
            let t = new_func_type(a, &param_types, result_type);
            Ok(t)
        }
        Syntax::Let(Let { defn, v, body }) => {
            let defn_type = analyse(a, defn, env, non_generic)?;
            let mut new_env = env.clone();
            new_env.0.insert(v.clone(), defn_type);
            analyse(a, body, &new_env, non_generic)
        }
        Syntax::Letrec(Letrec { defn, v, body }) => {
            let new_type = new_var_type(a);
            let mut new_env = env.clone();
            new_env.0.insert(v.clone(), new_type);
            let mut new_non_generic = non_generic.clone();
            new_non_generic.insert(new_type);
            let defn_type = analyse(a, defn, &new_env, &new_non_generic)?;
            unify(a, new_type, defn_type)?;
            analyse(a, body, &new_env, non_generic)
        }
        Syntax::IfElse(IfElse {
            cond,
            consequent,
            alternate,
        }) => {
            let cond_type = analyse(a, cond, env, non_generic)?;
            let bool_type = new_constructor(a, "boolean", &[]);
            unify(a, cond_type, bool_type)?;
            let consequent_type = analyse(a, consequent, env, non_generic)?;
            let alternate_type = analyse(a, alternate, env, non_generic)?;
            let t = new_union_type(a, &[consequent_type, alternate_type]);
            Ok(t)
        }
    }
}

/// Get the type of identifier name from the type environment env.
///
/// Args:
///     name: The identifier name
///     env: The type environment mapping from identifier names to types
///     non_generic: A set of non-generic TypeVariables
///
/// Raises:
///     ParseError: Raised if name is an undefined symbol in the type
///         environment.
fn get_type(
    a: &mut Vec<Type>,
    name: &str,
    env: &Env,
    non_generic: &HashSet<ArenaType>,
) -> Result<ArenaType, Errors> {
    if let Some(value) = env.0.get(name) {
        let mat = non_generic.iter().cloned().collect::<Vec<_>>();
        Ok(fresh(a, *value, &mat))
    } else {
        Err(Errors::InferenceError(format!(
            "Undefined symbol {:?}",
            name
        )))
    }
}

/// Makes a copy of a type expression.
///
/// The type t is copied. The the generic variables are duplicated and the
/// non_generic variables are shared.
///
/// Args:
///     t: A type to be copied.
///     non_generic: A set of non-generic TypeVariables
fn fresh(a: &mut Vec<Type>, t: ArenaType, non_generic: &[ArenaType]) -> ArenaType {
    // A mapping of TypeVariables to TypeVariables
    let mut mappings = HashMap::default();

    fn freshrec(
        a: &mut Vec<Type>,
        tp: ArenaType,
        mappings: &mut HashMap<ArenaType, ArenaType>,
        non_generic: &[ArenaType],
    ) -> ArenaType {
        let p = prune(a, tp);
        // We clone here because we can't move out of a shared reference.
        // TODO: Consider using Rc<RefCell<Type>> to avoid unnecessary cloning.
        match &a.get(p).unwrap().clone().kind {
            TypeKind::Variable(_) => {
                if is_generic(a, p, non_generic) {
                    mappings
                        .entry(p)
                        .or_insert_with(|| new_var_type(a))
                        .to_owned()
                } else {
                    p
                }
            }
            TypeKind::Constructor(con) => {
                let b = con
                    .types
                    .iter()
                    .map(|x| freshrec(a, *x, mappings, non_generic))
                    .collect::<Vec<_>>();
                new_constructor(a, &con.name, &b)
            }
            TypeKind::Literal(lit) => new_lit_type(a, lit),
            TypeKind::Function(func) => {
                let params = func
                    .params
                    .iter()
                    .map(|x| freshrec(a, *x, mappings, non_generic))
                    .collect::<Vec<_>>();
                let ret = freshrec(a, func.ret, mappings, non_generic);
                new_func_type(a, &params, ret)
            }
            TypeKind::Call(call) => {
                let args = call
                    .args
                    .iter()
                    .map(|x| freshrec(a, *x, mappings, non_generic))
                    .collect::<Vec<_>>();
                let ret = freshrec(a, call.ret, mappings, non_generic);
                new_call_type(a, &args, ret)
            }
            TypeKind::Union(union) => {
                let args = union
                    .types
                    .iter()
                    .map(|x| freshrec(a, *x, mappings, non_generic))
                    .collect::<Vec<_>>();
                new_union_type(a, &args)
            }
        }
    }

    freshrec(a, t, &mut mappings, non_generic)
}

/// Unify the two types t1 and t2.
///
/// Makes the types t1 and t2 the same.
///
/// Args:
///     t1: The first type to be made equivalent
///     t2: The second type to be be equivalent
///
/// Returns:
///     None
///
/// Raises:
///     InferenceError: Raised if the types cannot be unified.
fn unify(alloc: &mut Vec<Type>, t1: ArenaType, t2: ArenaType) -> Result<(), Errors> {
    let a = prune(alloc, t1);
    let b = prune(alloc, t2);
    // Why do we clone here?
    let a_t = alloc.get(a).unwrap().clone();
    let b_t = alloc.get(b).unwrap().clone();
    match (&a_t.kind, &b_t.kind) {
        (TypeKind::Variable(_), _) => bind(alloc, a, b),
        (_, TypeKind::Variable(_)) => bind(alloc, b, a),
        (TypeKind::Constructor(con_a), TypeKind::Constructor(con_b)) => {
            // TODO: support type constructors with optional and default type params
            if con_a.name != con_b.name || con_a.types.len() != con_b.types.len() {
                return Err(Errors::InferenceError(format!("type mismatch: {a} != {b}")));
            }
            for (p, q) in con_a.types.iter().zip(con_b.types.iter()) {
                unify(alloc, *p, *q)?;
            }
            Ok(())
        }
        (TypeKind::Function(func_a), TypeKind::Function(func_b)) => {
            for (p, q) in func_a.params.iter().zip(func_b.params.iter()) {
                unify(alloc, *p, *q)?;
            }
            unify(alloc, func_a.ret, func_b.ret)?;
            Ok(())
        }
        (TypeKind::Call(call), TypeKind::Function(func)) => {
            for (p, q) in call.args.iter().zip(func.params.iter()) {
                unify(alloc, *p, *q)?;
            }
            unify(alloc, call.ret, func.ret)?;
            Ok(())
        }
        (TypeKind::Call(call_a), TypeKind::Call(call_b)) => {
            for (p, q) in call_a.args.iter().zip(call_b.args.iter()) {
                unify(alloc, *p, *q)?;
            }
            unify(alloc, call_a.ret, call_b.ret)?;
            Ok(())
        }
        (
            TypeKind::Literal(Literal::Number(_)),
            TypeKind::Constructor(Constructor { name, .. }),
        ) if name == "number" => Ok(()),
        (
            TypeKind::Literal(Literal::String(_)),
            TypeKind::Constructor(Constructor { name, .. }),
        ) if name == "string" => Ok(()),
        (
            TypeKind::Literal(Literal::Boolean(_)),
            TypeKind::Constructor(Constructor { name, .. }),
        ) if name == "boolean" => Ok(()),
        (TypeKind::Union(Union { types }), _) => {
            // All types in the union must be subtypes of t2
            for t in types.iter() {
                unify(alloc, *t, b)?;
            }
            Ok(())
        }
        (_, TypeKind::Union(Union { types })) => {
            for t2 in types.iter() {
                if unify(alloc, a, *t2).is_ok() {
                    return Ok(());
                }
            }

            Err(Errors::InferenceError(format!(
                "type mismatch: unify({a_t:?}, {b_t:?}) failed"
            )))
        }
        _ => Err(Errors::InferenceError(format!(
            "type mismatch: unify({a_t:?}, {b_t:?}) failed"
        ))),
    }
}

fn bind(alloc: &mut Vec<Type>, a: usize, b: usize) -> Result<(), Errors> {
    if a != b {
        if occurs_in_type(alloc, a, b) {
            // raise InferenceError("recursive unification")
            return Err(Errors::InferenceError("recursive unification".to_string()));
        }
        alloc.get_mut(a).unwrap().set_instance(b);
    }
    Ok(())
}

/// Returns the currently defining instance of t.
///
/// As a side effect, collapses the list of type instances. The function Prune
/// is used whenever a type expression has to be inspected: it will always
/// return a type expression which is either an uninstantiated type variable or
/// a type operator; i.e. it will skip instantiated variables, and will
/// actually prune them from expressions to remove long chains of instantiated
/// variables.
///
/// Args:
///     t: The type to be pruned
///
/// Returns:
///     An uninstantiated TypeVariable or a TypeOperator
fn prune(a: &mut Vec<Type>, t: ArenaType) -> ArenaType {
    let v2 = match a.get(t).unwrap().kind {
        // TODO: handle .unwrap() panicing
        TypeKind::Variable(Variable {
            instance: Some(value),
        }) => value,
        _ => {
            return t;
        }
    };

    let value = prune(a, v2);
    match &mut a.get_mut(t).unwrap().kind {
        // TODO: handle .unwrap() panicing
        TypeKind::Variable(Variable {
            ref mut instance, ..
        }) => {
            *instance = Some(value);
        }
        _ => {
            return t;
        }
    }
    value
}

/// Checks whether a given variable occurs in a list of non-generic variables
///
/// Note that a variables in such a list may be instantiated to a type term,
/// in which case the variables contained in the type term are considered
/// non-generic.
///
/// Note: Must be called with v pre-pruned
///
/// Args:
///     v: The TypeVariable to be tested for genericity
///     non_generic: A set of non-generic TypeVariables
///
/// Returns:
///     True if v is a generic variable, otherwise False
fn is_generic(a: &mut Vec<Type>, v: ArenaType, non_generic: &[ArenaType]) -> bool {
    !occurs_in(a, v, non_generic)
}

/// Checks whether a type variable occurs in a type expression.
///
/// Note: Must be called with v pre-pruned
///
/// Args:
///     v:  The TypeVariable to be tested for
///     type2: The type in which to search
///
/// Returns:
///     True if v occurs in type2, otherwise False
fn occurs_in_type(a: &mut Vec<Type>, v: ArenaType, type2: ArenaType) -> bool {
    let pruned_type2 = prune(a, type2);
    if pruned_type2 == v {
        return true;
    }
    // We clone here because we can't move out of a shared reference.
    // TODO: Consider using Rc<RefCell<Type>> to avoid unnecessary cloning.
    match a.get(pruned_type2).unwrap().clone().kind {
        TypeKind::Variable(_) => false, // leaf node
        TypeKind::Literal(_) => false,  // leaf node
        TypeKind::Constructor(Constructor { types, .. }) => occurs_in(a, v, &types),
        TypeKind::Function(Function { params, ret }) => {
            occurs_in(a, v, &params) || occurs_in_type(a, v, ret)
        }
        TypeKind::Call(Call { args, ret }) => occurs_in(a, v, &args) || occurs_in_type(a, v, ret),
        TypeKind::Union(Union { types }) => occurs_in(a, v, &types),
    }
}

/// Checks whether a types variable occurs in any other types.
///
/// Args:
///     t:  The TypeVariable to be tested for
///     types: The sequence of types in which to search
///
/// Returns:
///     True if t occurs in any of types, otherwise False
///
fn occurs_in(a: &mut Vec<Type>, t: ArenaType, types: &[ArenaType]) -> bool {
    for t2 in types.iter() {
        if occurs_in_type(a, t, *t2) {
            return true;
        }
    }
    false
}

//=====================================================

#[cfg(test)]
mod tests {
    use super::*;

    pub fn new_lambda(params: &[&str], body: Syntax) -> Syntax {
        Syntax::Lambda(Lambda {
            params: params.iter().map(|x| x.to_string()).collect(),
            body: Box::new(body),
        })
    }

    pub fn new_apply(func: Syntax, args: &[Syntax]) -> Syntax {
        Syntax::Apply(Apply {
            func: Box::new(func),
            args: args.to_owned(),
        })
    }

    pub fn new_let(v: &str, defn: Syntax, body: Syntax) -> Syntax {
        Syntax::Let(Let {
            v: v.to_string(),
            defn: Box::new(defn),
            body: Box::new(body),
        })
    }

    pub fn new_letrec(v: &str, defn: Syntax, body: Syntax) -> Syntax {
        Syntax::Letrec(Letrec {
            v: v.to_string(),
            defn: Box::new(defn),
            body: Box::new(body),
        })
    }

    pub fn new_identifier(name: &str) -> Syntax {
        // TODO: Check that `name` is a valid identifier
        Syntax::Identifier(Identifier {
            name: name.to_string(),
        })
    }

    pub fn new_number(value: &str) -> Syntax {
        Syntax::Literal(Literal::Number(value.to_owned()))
    }

    pub fn new_string(value: &str) -> Syntax {
        Syntax::Literal(Literal::String(value.to_owned()))
    }

    pub fn new_boolean(value: bool) -> Syntax {
        Syntax::Literal(Literal::Boolean(value))
    }

    pub fn new_if_else(cond: Syntax, consequent: Syntax, alternate: Syntax) -> Syntax {
        Syntax::IfElse(IfElse {
            cond: Box::new(cond),
            consequent: Box::new(consequent),
            alternate: Box::new(alternate),
        })
    }

    fn test_env() -> (Vec<Type>, Env) {
        let mut a = vec![];
        let mut my_env = Env::default();

        let var1 = new_var_type(&mut a);
        let var2 = new_var_type(&mut a);
        let pair_type = new_constructor(&mut a, "*", &[var1, var2]);
        my_env.0.insert(
            "pair".to_string(),
            new_func_type(&mut a, &[var1, var2], pair_type),
        );

        let int_t = new_constructor(&mut a, "number", &[]);
        let bool_t = new_constructor(&mut a, "boolean", &[]);
        my_env
            .0
            .insert("zero".to_string(), new_func_type(&mut a, &[int_t], bool_t));

        let num1_t = new_constructor(&mut a, "number", &[]);
        let num2_t = new_constructor(&mut a, "number", &[]);
        my_env
            .0
            .insert("pred".to_string(), new_func_type(&mut a, &[num1_t], num2_t));

        // It isn't necessary to create separate instances of `number` for each of
        // these.  When interferring types from code we'll want separate instances
        // so that we can link each back to the expression from which it was
        // inferred.
        let num1_t = new_constructor(&mut a, "number", &[]);
        let num2_t = new_constructor(&mut a, "number", &[]);
        let num3_t = new_constructor(&mut a, "number", &[]);
        my_env.0.insert(
            "times".to_string(),
            new_func_type(&mut a, &[num1_t, num2_t], num3_t),
        );
        my_env.0.insert(
            "add".to_string(),
            new_func_type(&mut a, &[num1_t, num2_t], num3_t),
        );

        my_env
            .0
            .insert("true".to_string(), new_bool_lit_type(&mut a, true));
        my_env
            .0
            .insert("false".to_string(), new_bool_lit_type(&mut a, false));

        (a, my_env)
    }

    /// Sets up some predefined types using the type constructors TypeVariable,
    /// TypeOperator and Function.  Creates a list of example expressions to be
    /// evaluated. Evaluates the expressions, printing the type or errors arising
    /// from each.

    // NOTE: This test requires Union types to work correctly.
    #[test]
    fn test_factorial() -> Result<(), Errors> {
        let (mut a, my_env) = test_env();

        // factorial
        let syntax = new_letrec(
            "factorial", // letrec factorial =
            new_lambda(
                &["n"], // fn n =>
                new_if_else(
                    new_apply(new_identifier("zero"), &[new_identifier("n")]),
                    new_number("1"),
                    new_apply(
                        // times(n, factorial(pred(n))
                        new_identifier("times"),
                        &[
                            new_identifier("n"),
                            new_apply(
                                new_identifier("factorial"),
                                &[new_apply(new_identifier("pred"), &[new_identifier("n")])],
                            ),
                        ],
                    ),
                ),
            ), // in
            new_identifier("factorial"),
        );

        let t = analyse(&mut a, &syntax, &my_env, &HashSet::default())?;
        assert_eq!(
            a[t].as_string(
                &a,
                &mut Namer {
                    value: 'a',
                    set: HashMap::default(),
                }
            ),
            r#"(number -> number)"#
        );
        Ok(())
    }

    #[should_panic]
    #[test]
    fn test_mismatch() {
        let (mut a, my_env) = test_env();

        // fn x => (pair(x(3) (x(true)))
        let syntax = new_lambda(
            &["x"],
            new_apply(
                new_identifier("pair"),
                &[
                    new_apply(new_identifier("x"), &[new_number("3")]),
                    new_apply(new_identifier("x"), &[new_boolean(true)]),
                ],
            ),
        );

        analyse(&mut a, &syntax, &my_env, &HashSet::default()).unwrap();
    }

    #[should_panic = "called `Result::unwrap()` on an `Err` value: InferenceError(\"Undefined symbol \\\"f\\\"\")"]
    #[test]
    fn test_pair() {
        let (mut a, my_env) = test_env();

        // pair(f(3), f(true))
        let syntax = new_apply(
            new_identifier("pair"),
            &[
                new_apply(new_identifier("f"), &[new_number("4")]),
                new_apply(new_identifier("f"), &[new_boolean(true)]),
            ],
        );

        analyse(&mut a, &syntax, &my_env, &HashSet::default()).unwrap();
    }

    #[test]
    fn test_mul() -> Result<(), Errors> {
        let (mut a, my_env) = test_env();

        let pair = new_apply(
            new_identifier("pair"),
            &[
                new_apply(new_identifier("f"), &[new_number("4")]),
                new_apply(new_identifier("f"), &[new_boolean(true)]),
            ],
        );

        // let f = (fn x => x) in ((pair (f 4)) (f true))
        let syntax = new_let("f", new_lambda(&["x"], new_identifier("x")), pair);

        let t = analyse(&mut a, &syntax, &my_env, &HashSet::default())?;
        assert_eq!(
            a[t].as_string(
                &a,
                &mut Namer {
                    value: 'a',
                    set: HashMap::default(),
                }
            ),
            r#"(4 * true)"#
        );
        Ok(())
    }

    #[should_panic = "recursive unification"]
    #[test]
    fn test_recursive() {
        let (mut a, my_env) = test_env();

        // fn f => f f (fail)
        let syntax = new_lambda(
            &["f"],
            new_apply(new_identifier("f"), &[new_identifier("f")]),
        );

        analyse(&mut a, &syntax, &my_env, &HashSet::default()).unwrap();
    }

    #[test]
    fn test_number_literal() -> Result<(), Errors> {
        let (mut a, my_env) = test_env();

        // let g = fn f => 5 in g g
        let syntax = new_let(
            "g",
            new_lambda(&["f"], new_number("5")),
            new_apply(new_identifier("g"), &[new_identifier("g")]),
        );

        let t = analyse(&mut a, &syntax, &my_env, &HashSet::default())?;
        assert_eq!(
            a[t].as_string(
                &a,
                &mut Namer {
                    value: 'a',
                    set: HashMap::default(),
                }
            ),
            r#"5"#
        );
        Ok(())
    }

    #[test]
    fn test_generic_nongeneric() -> Result<(), Errors> {
        let (mut a, my_env) = test_env();

        // example that demonstrates generic and non-generic variables:
        // fn g => let f = fn x => g in pair (f 3, f true)
        let syntax = new_lambda(
            &["g"],
            new_let(
                "f",
                new_lambda(&["x"], new_identifier("g")),
                new_apply(
                    new_identifier("pair"),
                    &[
                        new_apply(new_identifier("f"), &[new_number("3")]),
                        new_apply(new_identifier("f"), &[new_boolean(true)]),
                    ],
                ),
            ),
        );

        let t = analyse(&mut a, &syntax, &my_env, &HashSet::default())?;
        assert_eq!(
            a[t].as_string(
                &a,
                &mut Namer {
                    value: 'a',
                    set: HashMap::default(),
                }
            ),
            r#"(a -> (a * a))"#
        );
        Ok(())
    }

    #[test]
    fn test_composition() -> Result<(), Errors> {
        let (mut a, my_env) = test_env();

        // Function composition
        // fn f (fn g (fn arg (f g arg)))
        let syntax = new_lambda(
            &["f"],
            new_lambda(
                &["g"],
                new_lambda(
                    &["arg"],
                    new_apply(
                        new_identifier("g"),
                        &[new_apply(new_identifier("f"), &[new_identifier("arg")])],
                    ),
                ),
            ),
        );

        let t = analyse(&mut a, &syntax, &my_env, &HashSet::default())?;
        assert_eq!(
            a[t].as_string(
                &a,
                &mut Namer {
                    value: 'a',
                    set: HashMap::default(),
                }
            ),
            r#"((a -> b) -> ((b -> c) -> (a -> c)))"#
        );
        Ok(())
    }

    #[test]
    fn test_fun() -> Result<(), Errors> {
        let (mut a, my_env) = test_env();

        // Function composition
        // (fn (f, g, arg) -> (f g arg))
        let syntax = new_lambda(
            &["f", "g", "arg"],
            new_apply(
                new_identifier("g"),
                &[new_apply(new_identifier("f"), &[new_identifier("arg")])],
            ),
        );

        let t = analyse(&mut a, &syntax, &my_env, &HashSet::default())?;
        assert_eq!(
            a[t].as_string(
                &a,
                &mut Namer {
                    value: 'a',
                    set: HashMap::default(),
                }
            ),
            r#"((a -> b), (b -> c), a -> c)"#
        );
        Ok(())
    }

    #[test]
    fn test_subtype() -> Result<(), Errors> {
        let (mut a, my_env) = test_env();

        let syntax = new_apply(
            new_identifier("times"),
            &[new_number("5"), new_number("10")],
        );

        let t = analyse(&mut a, &syntax, &my_env, &HashSet::default())?;
        assert_eq!(
            a[t].as_string(
                &a,
                &mut Namer {
                    value: 'a',
                    set: HashMap::default(),
                }
            ),
            r#"number"#
        );
        Ok(())
    }

    #[test]
    fn test_union_subtype() -> Result<(), Errors> {
        let (mut a, mut my_env) = test_env();

        let lit1 = new_num_lit_type(&mut a, "5");
        let lit2 = new_num_lit_type(&mut a, "10");
        my_env
            .0
            .insert("foo".to_string(), new_union_type(&mut a, &[lit1, lit2]));

        let syntax = new_apply(
            new_identifier("times"),
            &[new_identifier("foo"), new_number("2")],
        );

        let t = analyse(&mut a, &syntax, &my_env, &HashSet::default())?;
        assert_eq!(
            a[t].as_string(
                &a,
                &mut Namer {
                    value: 'a',
                    set: HashMap::default(),
                }
            ),
            r#"number"#
        );
        Ok(())
    }

    #[test]
    #[should_panic = "called `Result::unwrap()` on an `Err` value: InferenceError(\"type mismatch: unify(Type { id: 22, kind: Literal(String(\\\"hello\\\")) }, Type { id: 18, kind: Constructor(Constructor { name: \\\"number\\\", types: [] }) }) failed\")"]
    fn test_subtype_error() {
        let (mut a, my_env) = test_env();

        let syntax = new_apply(
            new_identifier("times"),
            &[new_number("5"), new_string("hello")],
        );

        analyse(&mut a, &syntax, &my_env, &HashSet::default()).unwrap();
    }

    #[test]
    #[should_panic = "called `Result::unwrap()` on an `Err` value: InferenceError(\"type mismatch: unify(Type { id: 25, kind: Literal(String(\\\"hello\\\")) }, Type { id: 20, kind: Constructor(Constructor { name: \\\"number\\\", types: [] }) }) failed\")"]
    fn test_union_subtype_error() {
        let (mut a, mut my_env) = test_env();

        let lit1 = new_lit_type(&mut a, &Literal::Number("5".to_string()));
        let lit2 = new_lit_type(&mut a, &Literal::String("hello".to_string()));
        my_env
            .0
            .insert("foo".to_string(), new_union_type(&mut a, &[lit1, lit2]));

        let syntax = new_apply(
            new_identifier("times"),
            &[new_identifier("foo"), new_number("2")],
        );

        analyse(&mut a, &syntax, &my_env, &HashSet::default()).unwrap();
    }
}
