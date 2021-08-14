use std::fs;
use obsidian::frontend::parser;

fn main() {
    let contents = fs::read_to_string("example.obs").unwrap();
    let ast = parser::parse(&contents).unwrap();
    println!("{}", ast);
}
