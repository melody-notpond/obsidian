use std::fs;

use obsidian::frontends::c::macros;
use obsidian::frontends::c::ir;
use obsidian::middleend::correctness;

#[macro_use]
extern crate lalrpop_util;

lalrpop_mod!(parser);

fn main() {
    let mut asts = parser::FullParser::new().parse(&fs::read_to_string("example.obs").unwrap()).unwrap();
    println!("{:?}", asts);
    let mut macros = vec![];
    macros::get_macros(&mut asts, &mut macros);
    macros::replace_macros(&mut asts, &macros);

    println!("\n\n{:?}", asts);

    let mut ir = ir::lower_ast_to_ir(asts).unwrap();
    println!("\n\n{:?}", ir);
    println!("\n\n{:?}", correctness::check(&mut ir));
    println!("\n\n{:?}", ir.funcs);
}
