use std::collections::HashMap;

use super::parser::Ast;

/*
pub enum Ast {
    Int(i64),
    Word(u64),
    Float(f64),
    Char(u8),
    True,
    False,
    String(String),
    Symbol(String),
    Infix(String, Box<Ast>, Box<Ast>),
    Tuple(Vec<Ast>),
    StructInit(String, Vec<Ast>),
    Application(Box<Ast>, Vec<Ast>),
    Attribute(Vec<Ast>),
    Prefix(String, Box<Ast>),
    TypeCast(Type, Box<Ast>),
    Pattern(Pattern),
    Let(Box<Ast>, Box<Ast>),
    Block(Vec<Ast>, bool),
    Box(Vec<(String, Vec<Ast>)>),
    FuncDef(String, Vec<(String, Type)>, Type, Box<Ast>),
    Struct(String, HashMap<String, Type>),
    Enum(String, Type, Vec<(String, Option<u64>)>),
}
*/

#[derive(Debug, Clone)]
pub struct Signature {
    box_structure: Ast,
    replacement: Ast,
}

impl Signature {
    fn matches<'a>(box_structure: &Ast, ast: &'a Ast, var_map: &mut HashMap<String, Ast>) -> bool {
        match (box_structure, ast) {
            (Ast::Symbol(var), ast) => {
                var_map.insert(var.clone(), ast.clone());
                true
            }

            (Ast::Block(b1, _), Ast::Block(b2, _)) => {
                if b1.len() == 1 && matches!(b1.first(), Some(Ast::Symbol(_))) {
                    Signature::matches(b1.first().unwrap(), ast, var_map)
                } else {
                    for (b1, b2) in b1.iter().zip(b2.iter()) {
                        if !Signature::matches(b1, b2, var_map) {
                            return false;
                        }
                    }

                    true
                }
            }

            (Ast::Box(b1), Ast::Box(b2)) => {
                for ((name1, asts1), (name2, asts2)) in b1.iter().zip(b2.iter()) {
                    if name1 != name2 {
                        return false;
                    }

                    for (a1, a2) in asts1.iter().zip(asts2.iter()) {
                        if !Signature::matches(a1, a2, var_map) {
                            return false;
                        }
                    }
                }

                true
            }

            _ => false,
        }
    }

    fn replace(ast: &mut Ast, var_map: &HashMap<String, Ast>) {
        match ast {
            Ast::Int(_)
            | Ast::Word(_)
            | Ast::Float(_)
            | Ast::Char(_)
            | Ast::True
            | Ast::False
            | Ast::String(_)
            | Ast::Struct(_, _)
            | Ast::Enum(_, _, _)
            | Ast::Pattern(_) => (),

            Ast::Symbol(sym) => {
                if let Some(v) = var_map.get(sym) {
                    *ast = v.clone();
                }
            }

            Ast::Infix(_, left, right) => {
                Signature::replace(&mut **left, var_map);
                Signature::replace(&mut **right, var_map);
            }

            Ast::Tuple(v)
            | Ast::StructInit(_, v)
            | Ast::Application(_, v)
            | Ast::Attribute(v)
            | Ast::Block(v, _) => {
                for ast in v {
                    Signature::replace(ast, var_map);
                }
            }

            Ast::Prefix(_, v)
            | Ast::TypeCast(_, v)
            | Ast::Let(_, v)
            | Ast::FuncDef(_, _, _, v) => {
                Signature::replace(&mut **v, var_map);
            }

            Ast::Box(b) => {
                for (_, b) in b {
                    for b in b {
                        Signature::replace(b, var_map);
                    }
                }
            }
        }
    }

    fn replaced(&self, ast: &mut Ast) -> bool {
        let mut var_map = HashMap::new();

        if Signature::matches(&self.box_structure, ast, &mut var_map) {
            *ast = self.replacement.clone();
            Signature::replace(ast, &var_map);
            true
        } else {
            false
        }
    }
}

fn replace_macros_helper(ast: &mut Ast, macros: &[Signature]) {
    match ast {
        Ast::Int(_)
        | Ast::Word(_)
        | Ast::Float(_)
        | Ast::Char(_)
        | Ast::True
        | Ast::False
        | Ast::String(_)
        | Ast::Symbol(_)
        | Ast::Struct(_, _)
        | Ast::Enum(_, _, _)
        | Ast::Pattern(_) => (),

        Ast::Infix(_, left, right) => {
            replace_macros_helper(&mut **left, macros);
            replace_macros_helper(&mut **right, macros);
        }

        Ast::Tuple(v)
        | Ast::StructInit(_, v)
        | Ast::Application(_, v)
        | Ast::Attribute(v)
        | Ast::Block(v, _) => {
            for ast in v {
                replace_macros_helper(ast, macros);
            }
        }

        Ast::Prefix(_, v)
        | Ast::TypeCast(_, v)
        | Ast::Let(_, v)
        | Ast::FuncDef(_, _, _, v) => {
            replace_macros_helper(&mut **v, macros);
        }

        Ast::Box(_) => {
            let mut changed = false;
            for sig in macros {
                if sig.replaced(ast) {
                    changed = true;
                    break;
                }
            }

            if changed {
                replace_macros_helper(ast, macros);
            }
        }
    }
}

pub fn replace_macros(asts: &mut Vec<Ast>, macros: &[Signature]) {
    for ast in asts {
        replace_macros_helper(ast, macros);
    }
}

pub fn get_macros(asts: &mut Vec<Ast>, macros: &mut Vec<Signature>) {
    let mut remove = vec![];
    for (i, ast) in asts.iter_mut().enumerate() {
        if let Ast::Box(vec) = ast {
            if vec.len() == 1 {
                if let Some((name, blocks)) = vec.first_mut() {
                    if name == "macro"
                        && matches!(blocks.get(0), Some(Ast::Block(v, _)) if v.len() == 1)
                        && matches!(blocks.get(1), Some(Ast::Block(_, _)))
                        && blocks.len() == 2
                    {
                        use std::mem::swap;
                        let mut temp = vec![];
                        swap(&mut temp, blocks);

                        let replacement = temp.remove(1);
                        let box_structure = if let Ast::Block(mut v, _) = temp.remove(0) {
                            v.swap_remove(0)
                        } else {
                            unreachable!();
                        };

                        macros.push(Signature {
                            box_structure,
                            replacement
                        });

                        remove.push(i);
                    }
                }
            }
        }
    }

    for i in remove.into_iter().rev() {
        asts.remove(i);
    }
}
