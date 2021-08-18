use std::fs;
use std::collections::HashMap;

use obsidian::frontends::c::parser;

fn main() {
    let contents = fs::read_to_string("example.obs").unwrap();
    let mut ast = parser::parse(&contents).unwrap();
    println!("{:?}", ast);
}
