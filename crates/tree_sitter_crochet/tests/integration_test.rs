use tree_sitter::Parser;
use tree_sitter_crochet::language;

#[test]
fn test_can_load_grammar() {
    let mut parser = Parser::new();
    parser
        .set_language(language())
        .expect("Error loading crochet language");
}

#[test]
fn test_can_parse_simple_expressions() {
    let mut parser = Parser::new();
    parser
        .set_language(language())
        .expect("Error loading crochet language");

    let code = "const a = 1 + 2 * 3 - 4 / 5";

    let tree = parser.parse(code, None).unwrap();
    println!("tree = {tree:#?}");

    let root = tree.root_node();
    println!("root = {root:#?}");
    println!("child_count = {}", root.child_count());

    let child = root.child(0).unwrap();
    println!("child = {child:#?}");
    println!("child_count = {}", child.child_count());

    let child = child.child(0).unwrap();
    println!("child = {child:#?}");
    println!("child_count = {}", child.child_count());

    for i in 0..3 {
        let child = child.child(i).unwrap();
        println!("child[{i}] = {child:#?}");
    }
}
