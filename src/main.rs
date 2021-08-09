use std::collections::HashMap;

use std::fs;
use obsidian::frontend::parser;
use obsidian::frontend::macros;

fn main() {
    let contents = fs::read_to_string("example.obs").unwrap();
    let mut ast = parser::parse(&contents).unwrap();
    println!("{}", ast);

    let mut map = HashMap::new();
    macros::add_macros(&mut map, &ast).unwrap();
    println!("{:?}", map);
    macros::apply_macros(&map, &mut ast).unwrap();
    println!("{}", ast);
}
