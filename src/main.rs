use std::fs;
use obsidian::frontend::parser;
use obsidian::frontend::ir;

fn main() {
    let contents = fs::read_to_string("example.obs").unwrap();
    let ast = parser::parse(&contents).unwrap();
    println!("{}", ast);
    let root = ir::parse_ir(ast).unwrap();
    println!("{:#?}", root);
}
