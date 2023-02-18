use tree_sitter::Parser;
use tree_sitter_escalier::language;

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
    println!("root.child_count = {}", root.child_count());

    let child = root.child(0).unwrap();
    println!("child[0] = {child:#?}");
    println!("child.child_count = {}", child.child_count());

    let grandchild = child.child(0).unwrap();
    println!("child.child[0] = {grandchild:#?}");
    println!("child.child.child_count = {}", grandchild.child_count());

    let child = child.child(1).unwrap();
    println!("child[1] = {child:#?}");
    println!("child_count = {}", child.child_count());

    let grandchild = child.child(0).unwrap();
    println!("child.child[0] = {grandchild:#?}");
    println!("child.child.child_count = {}", grandchild.child_count());

    let grandchild = child.child(1).unwrap();
    println!("child.child[1] = {grandchild:#?}");
    println!("child.child.child_count = {}", grandchild.child_count());
    let range = grandchild.range();
    let slice = code.get(range.start_byte..range.end_byte).unwrap();
    println!("child.child[2] = {:#?}", slice);

    let grandchild = child.child(2).unwrap();
    println!("child.child[2] = {grandchild:#?}");
    println!("child.child.child_count = {}", grandchild.child_count());
    let range = grandchild.range();
    let slice = code.get(range.start_byte..range.end_byte).unwrap();
    println!("child.child[2] = {:#?}", slice);
}
