use std::fs;
use std::collections::HashMap;

use obsidian::frontend::macros;
use obsidian::frontend::parser;
use obsidian::frontend::ir;

fn main() {
    let contents = fs::read_to_string("example.obs").unwrap();
    let mut ast = parser::parse(&contents).unwrap();
    println!("{}", ast);
    let mut macros = HashMap::new();
    macros::add_macros(&mut macros, &ast).unwrap();
    macros::apply_macros(&macros, &mut ast).unwrap();
    println!("{}", ast);

    let root = ir::parse_ir(ast).unwrap();
    println!("{:#?}", root);
}
