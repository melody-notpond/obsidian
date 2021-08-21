use std::collections::HashMap;
use std::fs;

use obsidian::frontends::c::parser;
use obsidian::frontends::c::macros;

fn main() {
    let contents = fs::read_to_string("example.obs").unwrap();
    let mut ast = parser::parse(&contents).unwrap();
    println!("{}\n\n", ast);

    let mut macros = vec![];
    macros::get_macros(&mut ast, &mut macros);

    println!("{:?}\n\n", macros);

    macros::replace_macros(&mut ast, &macros);
    println!("{}", ast);
}
