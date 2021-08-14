use std::collections::HashSet;

use super::parser::Ast;

#[derive(Clone, Debug)]
pub enum Type {
    Unassigned,
    Unknown(usize),

    Nil,
    Bool,

    UnassignedInt,
    UnknownInt(usize),
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,

    UnassignedFloat,
    UnknownFloat(usize),
    F32,
    F64,

    ConstRef(Option<String>, Box<Type>),
    MutRef(Option<String>, Box<Type>),

    ConstPtr(Box<Type>),
    MutPtr(Box<Type>),

    Generic(String),
    TypeName(String, Vec<Type>, Vec<String>),
    Tuple(Vec<Type>),
    VarArgs(Option<Box<Type>>),
}

pub enum TypeParseError {
    NotAType,
    NotASymbol,
    DoesNotTakeGenerics,
    InvalidReference,
    InvalidPointer,
    InvalidTuple,
    InvalidVarArgs,
}

pub fn parse_type(ast: Ast, generics_set: &HashSet<&String>) -> Result<Type, TypeParseError> {
    match ast {
        Ast::Int(_, _)
        | Ast::Float(_, _)
        | Ast::Word(_, _)
        | Ast::Char(_, _)
        | Ast::True(_)
        | Ast::False(_)
        | Ast::String(_, _)
        | Ast::Lifetime(_, _)
        | Ast::Program(_, _) => Err(TypeParseError::NotAType),

        Ast::Symbol(_, sym) => {
            match sym.as_str() {
                "i8" => Ok(Type::I8),
                "i16" => Ok(Type::I16),
                "i32" => Ok(Type::I32),
                "i64" => Ok(Type::I64),
                "u8" => Ok(Type::U8),
                "u16" => Ok(Type::U16),
                "u32" => Ok(Type::U32),
                "u64" => Ok(Type::U64),
                "bool" => Ok(Type::Bool),

                "f32" => Ok(Type::F32),
                "f64" => Ok(Type::F64),

                "nil" => Ok(Type::Nil),

                "..." => Ok(Type::VarArgs(None)),

                _ => Ok(Type::TypeName(sym, vec![], vec![]))
            }
        }

        Ast::SExpr(_, mut exprs) => {
            if exprs.is_empty() {
                Ok(Type::Nil)
            } else {
                let first = exprs.remove(0);
                if let Ast::Symbol(_, sym) = first {
                    let (generics, lifetimes): (Vec<_>, Vec<_>) = exprs.into_iter().map(|v| if let Ast::Lifetime(_, v) = v { (None, Some(v)) } else { (Some(parse_type(v, generics_set)), None) }).unzip();
                    let generics: Result<Vec<_>, _> = generics.into_iter().flatten().collect();
                    let mut generics = generics?;
                    let mut lifetimes: Vec<_> = lifetimes.into_iter().flatten().collect();

                    match sym.as_str() {
                          "i8" | "i16" | "i32" | "i64"
                        | "u8" | "u16" | "u32" | "u64"
                                       | "f32" | "f64"
                        | "bool"
                        => Err(TypeParseError::DoesNotTakeGenerics),

                        "&" => {
                            if generics.len() == 1 && lifetimes.is_empty() {
                                Ok(Type::ConstRef(None, Box::new(generics.swap_remove(0))))
                            } else if generics.len() == 1 && lifetimes.len() == 1 {
                                Ok(Type::ConstRef(Some(lifetimes.swap_remove(0)), Box::new(generics.swap_remove(0))))
                            } else {
                                Err(TypeParseError::InvalidReference)
                            }
                        }

                        "&mut" => {
                            if generics.len() == 1 && lifetimes.is_empty() {
                                Ok(Type::MutRef(None, Box::new(generics.swap_remove(0))))
                            } else if generics.len() == 1 && lifetimes.len() == 1 {
                                Ok(Type::MutRef(Some(lifetimes.swap_remove(0)), Box::new(generics.swap_remove(0))))
                            } else {
                                Err(TypeParseError::InvalidReference)
                            }
                        }

                        "*" => {
                            if generics.len() == 1 && lifetimes.is_empty() {
                                Ok(Type::ConstPtr(Box::new(generics.swap_remove(0))))
                            } else {
                                Err(TypeParseError::InvalidPointer)
                            }
                        }

                        "*mut" => {
                            if generics.len() == 1 && lifetimes.is_empty() {
                                Ok(Type::MutPtr(Box::new(generics.swap_remove(0))))
                            } else {
                                Err(TypeParseError::InvalidPointer)
                            }
                        }

                        "tuple" => {
                            if !generics.is_empty() && lifetimes.is_empty() {
                                Ok(Type::Tuple(generics))
                            } else {
                                Err(TypeParseError::InvalidTuple)
                            }
                        }

                        "..." => {
                            if generics.len() == 1 && lifetimes.is_empty() {
                                Ok(Type::VarArgs(Some(Box::new(generics.swap_remove(0)))))
                            } else {
                                Err(TypeParseError::InvalidVarArgs)
                            }
                        }

                        _ if generics_set.contains(&sym) => {
                            Ok(Type::Generic(sym))
                        }

                        _ => {
                            Ok(Type::TypeName(sym, generics, lifetimes))
                        }
                    }
                } else {
                    Err(TypeParseError::NotASymbol)
                }
            }
        }
    }
}
