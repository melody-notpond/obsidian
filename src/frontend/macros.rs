use std::collections::HashMap;

use logos::Span;

use super::parser::Ast;

#[derive(Hash, Eq, PartialEq, Debug)]
pub enum MacroArgs {
    Argument(String),
    Quote(String),
    VariadicArg(String),
    SExpr(Vec<MacroArgs>),
}

impl MacroArgs {
    fn parse(ast: &Ast) -> Result<MacroArgs, MacroError> {
        match ast {
            Ast::Int(_, _)
            | Ast::Float(_, _)
            | Ast::Word(_, _)
            | Ast::Char(_, _)
            | Ast::True(_)
            | Ast::False(_)
            | Ast::String(_, _)
            | Ast::Lifetime(_, _)
            | Ast::Program(_, _) => Err(MacroError),

            Ast::Symbol(_, sym) => Ok(MacroArgs::Argument(sym.clone())),

            Ast::SExpr(_, exprs) => {
                if let Some(Ast::Symbol(_, type_)) = exprs.first() {
                    match type_.as_str() {
                        "quote" => {
                            let mut quotes = vec![];
                            for quoted in exprs.iter().skip(1) {
                                if let Ast::Symbol(_, quoted) = quoted {
                                    quotes.push(MacroArgs::Quote(quoted.clone()));
                                } else {
                                    return Err(MacroError);
                                }
                            }

                            if quotes.is_empty() {
                                Err(MacroError)
                            } else if quotes.len() == 1 {
                                Ok(quotes.into_iter().next().unwrap())
                            } else {
                                Ok(MacroArgs::SExpr(quotes))
                            }
                        }

                        "..." => {
                            if exprs.len() != 2 {
                                return Err(MacroError);
                            }

                            if let Some(Ast::Symbol(_, var)) = exprs.get(1) {
                                Ok(MacroArgs::VariadicArg(var.clone()))
                            } else {
                                Err(MacroError)
                            }
                        }

                        _ => {
                            let vec: Result<Vec<_>, _> = exprs.iter().map(MacroArgs::parse).collect();
                            Ok(MacroArgs::SExpr(vec?))
                        }
                    }
                } else {
                    let vec: Result<Vec<_>, _> = exprs.iter().map(MacroArgs::parse).collect();
                    Ok(MacroArgs::SExpr(vec?))
                }
            }
        }
    }

    fn matches(&self, asts: &[Ast]) -> Option<Vec<(String, (Ast, bool))>> {
        match self {
            MacroArgs::Argument(arg) => {
                if asts.len() != 1 {
                    None
                } else {
                    Some(vec![(arg.clone(), (asts[0].clone(), false))])
                }
            }

            MacroArgs::Quote(quoted) => {
                if asts.len() != 1 {
                    None
                } else if let Ast::Symbol(_, name) = &asts[0] {
                    if name == quoted {
                        Some(vec![])
                    } else {
                        None
                    }
                } else {
                    None
                }
            }

            MacroArgs::VariadicArg(arg) => {
                Some(vec![(arg.clone(), (Ast::SExpr(Span {
                    start: 0,
                    end: 0,
                }, asts.to_vec()), true))])
            }

            MacroArgs::SExpr(sexpr) => {
                if sexpr.len() == asts.len() {
                    let mut vec = vec![];
                    for (i, (ast, arg)) in asts.iter().zip(sexpr.iter()).enumerate() {
                        if let Ast::SExpr(_, v) = ast {
                            if matches!(arg, MacroArgs::SExpr(_)) {
                                vec.extend(arg.matches(v)?);
                            } else {
                                vec.extend(arg.matches(&asts[i..i + 1])?);
                            }
                        } else {
                            vec.extend(arg.matches(&asts[i..i + 1])?);
                        }
                    }

                    Some(vec)
                } else if sexpr.len() - 1 <= asts.len() && matches!(sexpr.last(), Some(MacroArgs::VariadicArg(_))) {
                    let mut vec = vec![];
                    for (i, (ast, arg)) in asts.iter().zip(sexpr[..sexpr.len() - 1].iter()).enumerate() {
                        if let Ast::SExpr(_, v) = ast {
                            if matches!(arg, MacroArgs::SExpr(_)) {
                                vec.extend(arg.matches(v)?);
                            } else {
                                vec.extend(arg.matches(&asts[i..i + 1])?);
                            }
                        } else {
                            vec.extend(arg.matches(&asts[i..i + 1])?);
                        }
                    }

                    if sexpr.len() < asts.len() {
                        vec.extend(sexpr.last().unwrap().matches(&asts[sexpr.len() - 1..])?);
                    } else if let Some(MacroArgs::VariadicArg(arg)) = sexpr.last() {
                        vec.push((arg.clone(), (Ast::SExpr(Span {
                            start: 0,
                            end: 0
                        }, vec![]), true)));
                    }

                    Some(vec)
                } else {
                    None
                }
            }
        }
    }
}

pub type Macro = Vec<(MacroArgs, Ast)>;
pub type MacroMap = HashMap<String, Macro>;

#[derive(Debug)]
pub struct MacroError;

pub fn add_macros(map: &mut MacroMap, ast: &Ast) -> Result<(), MacroError> {
    match ast {
        Ast::Int(_, _)
        | Ast::Float(_, _)
        | Ast::Word(_, _)
        | Ast::Char(_, _)
        | Ast::True(_)
        | Ast::False(_)
        | Ast::String(_, _)
        | Ast::Symbol(_, _)
        | Ast::Lifetime(_, _) => (),

        Ast::SExpr(_, exprs) => {
            if let Some(Ast::Symbol(_, func)) = exprs.first() {
                if func == "macro" {
                    if let Some(Ast::Symbol(_, name)) = exprs.get(1) {
                        let mut macro_ = Vec::new();
                        for (args, body) in exprs.iter().skip(2).step_by(2).zip(exprs.iter().skip(3).step_by(2)) {
                            macro_.push((MacroArgs::parse(args)?, body.clone()));
                        }

                        map.insert(name.clone(), macro_);
                    } else {
                        return Err(MacroError);
                    }
                } else {
                    for expr in exprs.iter().skip(1) {
                        add_macros(map, expr)?;
                    }
                }
            } else {
                for expr in exprs {
                    add_macros(map, expr)?;
                }
            }
        }

        Ast::Program(_, exprs) => {
            for expr in exprs {
                add_macros(map, expr)?;
            }
        }
    }

    Ok(())
}

fn apply_macro_helper(map: &HashMap<String, (Ast, bool)>, ast: &mut Ast) {
    match ast {
        Ast::Int(_, _)
        | Ast::Float(_, _)
        | Ast::Word(_, _)
        | Ast::Char(_, _)
        | Ast::True(_)
        | Ast::False(_)
        | Ast::String(_, _)
        | Ast::Lifetime(_, _)
        | Ast::Program(_, _)
        | Ast::Symbol(_, _) => (),

        Ast::SExpr(_, sexpr) => {
            let mut inliners = vec![];
            for (i, expr) in sexpr.iter_mut().enumerate() {
                if let Ast::Symbol(_, sym) = expr {
                    if let Some((value, inlined)) = map.get(sym) {
                        if *inlined {
                            inliners.push((i, value));
                        } else {
                            *expr = value.clone();
                        }
                    }
                } else if let Ast::SExpr(_, _) = expr {
                    apply_macro_helper(map, expr);
                }
            }

            for (i, value) in inliners {
                if let Ast::SExpr(_, values) = value {
                    sexpr.remove(i);
                    for (j, value) in values.iter().enumerate() {
                        sexpr.insert(i + j, value.clone());
                    }
                } else {
                    *sexpr.get_mut(i).unwrap() = value.clone();
                }
            }
        }
    }
}

pub fn apply_macros(map: &MacroMap, ast: &mut Ast) -> Result<(), MacroError> {
    match ast {
        Ast::Int(_, _)
        | Ast::Float(_, _)
        | Ast::Word(_, _)
        | Ast::Char(_, _)
        | Ast::True(_)
        | Ast::False(_)
        | Ast::String(_, _)
        | Ast::Symbol(_, _)
        | Ast::Lifetime(_, _) => Ok(()),

        Ast::SExpr(_, sexpr) => {
            if let Some(Ast::Symbol(_, macro_)) = sexpr.first() {
                if let Some(macro_) = map.get(macro_) {
                    for (args, body) in macro_ {
                        if let Some(map) = args.matches(&sexpr[1..]) {
                            let map: HashMap<_, _> = map.into_iter().collect();
                            let mut body = body.clone();
                            apply_macro_helper(&map, &mut body);
                            *ast = body;
                            return Ok(());
                        }
                    }

                    Err(MacroError)
                } else {
                    for expr in sexpr.iter_mut().skip(1) {
                        apply_macros(map, expr)?;
                    }

                    Ok(())
                }
            } else {
                for expr in sexpr {
                    apply_macros(map, expr)?;
                }

                Ok(())
            }
        }

        Ast::Program(_, exprs) => {
            for expr in exprs {
                apply_macros(map, expr)?;
            }
            Ok(())
        }
    }
}

