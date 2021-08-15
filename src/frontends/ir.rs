use logos::Span;
use std::collections::{HashMap, HashSet};

use super::super::middleend::ir::{Ir, IrModule, SExpr, SExprMetadata, Pattern, NativeOperation};
use super::super::middleend::types::Type;
use super::types::{self, TypeParseError};
use super::parser::Ast;

#[derive(Debug, Clone)]
pub enum IrParseError {
    InvalidPattern,
    NonRootPassedIntoRootFunction,
    StrayLifetime,
    ProgramInProgram,
    InvalidFunction,
    InvalidMutRef,
    MissingIfClauses,
    InvalidLoopArgs,
    InvalidFunctionName,
    InvalidFunctionValue,
    InvalidFunctionGeneric,
    TypeParseError(TypeParseError),
    InvalidFunctionArg,
    InvalidLet,
    InvalidAssign,
    InvalidStructName,
    InvalidStructGeneric,
    InvalidStructField,
    InvalidEnum,
    InvalidEnumVariant,
    InvalidTypeDef,
    InvalidAttribute,
    MissingTryClauses,
}

fn parse_pattern(ast: Ast) -> Result<Pattern, IrParseError> {
    match ast {
        Ast::Symbol(_, sym) if sym == "_" => Ok(Pattern::Wildcard),
        Ast::Symbol(_, sym) => Ok(Pattern::Name(sym)),
        Ast::SExpr(_, mut sexpr) => {
            if sexpr.is_empty() {
                return Err(IrParseError::InvalidPattern);
            }

            match &sexpr[0] {
                Ast::Symbol(_, sym) => {
                    match sym.as_str() {
                        "mut" => {
                            if sexpr.len() == 2 && matches!(&sexpr[1], Ast::Symbol(_, _)){
                                if let Ast::Symbol(_, sym) = sexpr.remove(1)  {
                                    Ok(Pattern::MutName(sym))
                                } else {
                                    unreachable!()
                                }
                            } else {
                                Err(IrParseError::InvalidPattern)
                            }
                        }

                        "tuple" => Ok(Pattern::Tuple(
                            sexpr.into_iter().skip(1).map(parse_pattern).collect::<Result<Vec<_>, _>>()?
                        )),

                        "any" => Ok(Pattern::Or(
                            sexpr.into_iter().skip(1).map(parse_pattern).collect::<Result<Vec<_>, _>>()?
                        )),

                        "enum" => {
                            if sexpr.len() == 2 && matches!(&sexpr[1], Ast::Symbol(_, _)){
                                if let Ast::Symbol(_, sym) = sexpr.remove(1)  {
                                    Ok(Pattern::Enum(sym))
                                } else {
                                    unreachable!()
                                }
                            } else {
                                Err(IrParseError::InvalidPattern)
                            }
                        }

                        _ => {
                            let name = if let Ast::Symbol(_, sym) = sexpr.remove(0) {
                                sym
                            } else {
                                unreachable!();
                            };
                            let mut map = HashMap::new();
                            for expr in sexpr.into_iter() {
                                match expr {
                                    Ast::SExpr(_, mut sexpr) if sexpr.len() == 2 && matches!(&sexpr[0], Ast::Symbol(_, _)) => {
                                        let pattern = sexpr.remove(1);
                                        if let Ast::Symbol(_, sym) = sexpr.remove(0) {
                                            map.insert(sym, parse_pattern(pattern)?);
                                        }
                                    }

                                    _ => return Err(IrParseError::InvalidPattern),
                                }
                            }

                            Ok(Pattern::Struct(name, map))
                        }
                    }
                }

                Ast::Int(_, i) => {
                    if sexpr.len() != 2 || !matches!(&sexpr[0], Ast::Symbol(_, sym) if sym == "-") || !matches!(sexpr[1], Ast::Int(_, _)) {
                        Err(IrParseError::InvalidPattern)
                    } else if let Ast::Int(_, i2) = sexpr[1] {
                        Ok(Pattern::SRange(*i, i2))
                    } else {
                        unreachable!();
                    }
                }

                Ast::Word(_, w) => {
                    if sexpr.len() != 2 || !matches!(&sexpr[0], Ast::Symbol(_, sym) if sym == "-") || !matches!(sexpr[1], Ast::Word(_, _)) {
                        Err(IrParseError::InvalidPattern)
                    } else if let Ast::Word(_, w2) = sexpr[1] {
                        Ok(Pattern::URange(*w, w2))
                    } else {
                        unreachable!();
                    }
                }

                Ast::Float(_, f) => {
                    if sexpr.len() != 2 || !matches!(&sexpr[0], Ast::Symbol(_, sym) if sym == "-") || !matches!(sexpr[1], Ast::Float(_, _)) {
                        Err(IrParseError::InvalidPattern)
                    } else if let Ast::Float(_, f2) = sexpr[1] {
                        Ok(Pattern::FRange(*f, f2))
                    } else {
                        unreachable!();
                    }
                }

                _ => Err(IrParseError::InvalidPattern),
            }
        }

        _ => Err(IrParseError::InvalidPattern),
    }
}

fn parse_exprs(exprs: Vec<Ast>, ir: &mut Ir, module: &mut IrModule) -> Result<Vec<SExpr>, IrParseError> {
    exprs.into_iter().map(|v| parse_ir_helper(v, ir, module)).collect::<Result<Vec<_>, _>>()
}

fn parse_ir_helper(ast: Ast, ir: &mut Ir, module: &mut IrModule) -> Result<SExpr, IrParseError> {
    match ast {
        Ast::Int(span, i) => Ok(SExpr::Int(SExprMetadata::new_with_type(span, Type::UnassignedInt), i)),
        Ast::Float(span, f) => Ok(SExpr::Float(SExprMetadata::new_with_type(span, Type::UnassignedFloat), f)),
        Ast::Word(span, i) => Ok(SExpr::Word(SExprMetadata::new_with_type(span, Type::UnassignedInt), i)),
        Ast::Char(span, c) => Ok(SExpr::Int(SExprMetadata::new_with_type(span, Type::U8), c as i64)),
        Ast::True(span) => Ok(SExpr::Bool(SExprMetadata::new_with_type(span, Type::Bool), true)),
        Ast::False(span) => Ok(SExpr::Bool(SExprMetadata::new_with_type(span, Type::Bool), false)),
        Ast::String(span, s) => Ok(SExpr::String(SExprMetadata::new(span), s)),
        Ast::Symbol(span, sym) if sym == "continue" => Ok(SExpr::Continue(SExprMetadata::new(span), None)),
        Ast::Symbol(span, sym) if sym == "break" => Ok(SExpr::Break(SExprMetadata::new(span), None)),
        Ast::Symbol(span, sym) if sym == "return" => Ok(SExpr::Return(SExprMetadata::new(span), None)),
        Ast::Symbol(span, sym) if sym == "unreachable" => Ok(SExpr::Unreachable(SExprMetadata::new(span))),
        Ast::Symbol(span, sym) => Ok(SExpr::Symbol(SExprMetadata::new(span), sym)),
        Ast::Lifetime(_, _) => Err(IrParseError::StrayLifetime),
        Ast::Program(_, _) => Err(IrParseError::ProgramInProgram),

        Ast::SExpr(span, mut exprs) => {
            if exprs.is_empty() {
                return Ok(SExpr::Nil(SExprMetadata::new_with_type(span, Type::Nil)));
            }

            let f = exprs.remove(0);

            match f {
                Ast::Int(_, _)
                | Ast::Float(_, _)
                | Ast::Word(_, _)
                | Ast::Char(_, _)
                | Ast::True(_)
                | Ast::False(_)
                | Ast::String(_, _)
                | Ast::Program(_, _)
                | Ast::Lifetime(_, _) => Err(IrParseError::InvalidFunction),

                Ast::Symbol(f_span, sym) => {
                    match sym.as_str() {
                        // Native operators (and also ref/deref)
                        "+" => Ok(SExpr::Native(SExprMetadata::new(span), NativeOperation::Add, parse_exprs(exprs, ir, module)?)),
                        "-" => Ok(SExpr::Native(SExprMetadata::new(span), NativeOperation::Sub, parse_exprs(exprs, ir, module)?)),
                        "*" if exprs.len() == 1 => Ok(SExpr::Deref(SExprMetadata::new(span), Box::new(parse_ir_helper(exprs.remove(0), ir, module)?))),
                        "*" => Ok(SExpr::Native(SExprMetadata::new(span), NativeOperation::Mul, parse_exprs(exprs, ir, module)?)),
                        "/" => Ok(SExpr::Native(SExprMetadata::new(span), NativeOperation::Div, parse_exprs(exprs, ir, module)?)),
                        "%" => Ok(SExpr::Native(SExprMetadata::new(span), NativeOperation::Mod, parse_exprs(exprs, ir, module)?)),
                        "<<" => Ok(SExpr::Native(SExprMetadata::new(span), NativeOperation::Bsl, parse_exprs(exprs, ir, module)?)),
                        ">>" => Ok(SExpr::Native(SExprMetadata::new(span), NativeOperation::Bsr, parse_exprs(exprs, ir, module)?)),
                        "&" if exprs.len() == 1 => Ok(SExpr::Ref(SExprMetadata::new(span), Box::new(parse_ir_helper(exprs.remove(0), ir, module)?))),

                        "&mut" => {
                            if exprs.len() == 1 {
                                Ok(SExpr::Ref(SExprMetadata::new(span), Box::new(parse_ir_helper(exprs.remove(0), ir, module)?)))
                            } else {
                                Err(IrParseError::InvalidMutRef)
                            }
                        }

                        "&" => Ok(SExpr::Native(SExprMetadata::new(span), NativeOperation::BitAnd, parse_exprs(exprs, ir, module)?)),
                        "|" => Ok(SExpr::Native(SExprMetadata::new(span), NativeOperation::BitOr, parse_exprs(exprs, ir, module)?)),
                        "^" => Ok(SExpr::Native(SExprMetadata::new(span), NativeOperation::BitXor, parse_exprs(exprs, ir, module)?)),
                        "and" => Ok(SExpr::Native(SExprMetadata::new(span), NativeOperation::BoolAnd, parse_exprs(exprs, ir, module)?)),
                        "or" => Ok(SExpr::Native(SExprMetadata::new(span), NativeOperation::BoolOr, parse_exprs(exprs, ir, module)?)),
                        "xor" => Ok(SExpr::Native(SExprMetadata::new(span), NativeOperation::BoolXor, parse_exprs(exprs, ir, module)?)),
                        "<" => Ok(SExpr::Native(SExprMetadata::new(span), NativeOperation::Lt, parse_exprs(exprs, ir, module)?)),
                        ">" => Ok(SExpr::Native(SExprMetadata::new(span), NativeOperation::Gt, parse_exprs(exprs, ir, module)?)),
                        "<=" => Ok(SExpr::Native(SExprMetadata::new(span), NativeOperation::Lte, parse_exprs(exprs, ir, module)?)),
                        ">=" => Ok(SExpr::Native(SExprMetadata::new(span), NativeOperation::Gte, parse_exprs(exprs, ir, module)?)),
                        "==" => Ok(SExpr::Native(SExprMetadata::new(span), NativeOperation::Eq, parse_exprs(exprs, ir, module)?)),
                        "!=" => Ok(SExpr::Native(SExprMetadata::new(span), NativeOperation::Ne, parse_exprs(exprs, ir, module)?)),

                        // Control flow keywords
                        "continue" => {
                            if exprs.is_empty() {
                                Ok(SExpr::Continue(SExprMetadata::new(span), None))
                            } else if exprs.len() == 1 {
                                Ok(SExpr::Continue(SExprMetadata::new(span), Some(Box::new(parse_ir_helper(exprs.remove(0), ir, module)?))))
                            } else {
                                let exprs = parse_exprs(exprs, ir, module)?;
                                Ok(SExpr::Continue(SExprMetadata::new(span), Some(Box::new(SExpr::Tuple(SExprMetadata::new(Span {
                                    start: exprs.first().unwrap().get_metadata().span.start,
                                    end: exprs.last().unwrap().get_metadata().span.end
                                }), exprs)))))
                            }
                        }

                        "break" => {
                            if exprs.is_empty() {
                                Ok(SExpr::Break(SExprMetadata::new(span), None))
                            } else if exprs.len() == 1 {
                                Ok(SExpr::Break(SExprMetadata::new(span), Some(Box::new(parse_ir_helper(exprs.remove(0), ir, module)?))))
                            } else {
                                let exprs = parse_exprs(exprs, ir, module)?;
                                Ok(SExpr::Break(SExprMetadata::new(span), Some(Box::new(SExpr::Tuple(SExprMetadata::new(Span {
                                    start: exprs.first().unwrap().get_metadata().span.start,
                                    end: exprs.last().unwrap().get_metadata().span.end
                                }), exprs)))))
                            }
                        }

                        "return" => {
                            if exprs.is_empty() {
                                Ok(SExpr::Return(SExprMetadata::new(span), None))
                            } else if exprs.len() == 1 {
                                Ok(SExpr::Return(SExprMetadata::new(span), Some(Box::new(parse_ir_helper(exprs.remove(0), ir, module)?))))
                            } else {
                                let exprs = parse_exprs(exprs, ir, module)?;
                                Ok(SExpr::Return(SExprMetadata::new(span), Some(Box::new(SExpr::Tuple(SExprMetadata::new(Span {
                                    start: exprs.first().unwrap().get_metadata().span.start,
                                    end: exprs.last().unwrap().get_metadata().span.end
                                }), exprs)))))
                            }
                        }

                        "throw" => {
                            if exprs.is_empty() {
                                Ok(SExpr::Throw(SExprMetadata::new(span), Box::new(SExpr::Nil(SExprMetadata::new_with_type(f_span, Type::Nil)))))
                            } else if exprs.len() == 1 {
                                Ok(SExpr::Throw(SExprMetadata::new(span), Box::new(parse_ir_helper(exprs.remove(0), ir, module)?)))
                            } else {
                                let exprs = parse_exprs(exprs, ir, module)?;
                                Ok(SExpr::Throw(SExprMetadata::new(span), Box::new(SExpr::Tuple(SExprMetadata::new(Span {
                                    start: exprs.first().unwrap().get_metadata().span.start,
                                    end: exprs.last().unwrap().get_metadata().span.end
                                }), exprs))))
                            }
                        }

                        "if" => {
                            let mut exprs = exprs.into_iter();
                            match exprs.next() {
                                Some(v) => {
                                    let cond = parse_ir_helper(v, ir, module)?;
                                    let body = match exprs.next() {
                                        Some(v) => parse_ir_helper(v, ir, module)?,
                                        None => return Err(IrParseError::MissingIfClauses),
                                    };
                                    let elsy = match exprs.next() {
                                        Some(Ast::Symbol(_, s)) if s == "else" => {
                                            match exprs.next() {
                                                Some(v) => Some(Box::new(parse_ir_helper(v, ir, module)?)),
                                                None => return Err(IrParseError::MissingIfClauses),
                                            }
                                        }

                                        Some(_) => return Err(IrParseError::MissingIfClauses),

                                        None => None,
                                    };

                                    Ok(SExpr::If(SExprMetadata::new(span), Box::new(cond), Box::new(body), elsy))
                                }

                                None => {
                                    Err(IrParseError::MissingIfClauses)
                                }
                            }
                        }

                        "cond" => {
                            let mut vec = vec![];
                            let mut elsy = None;
                            for pair in exprs {
                                if elsy.is_some() {
                                    return Err(IrParseError::MissingIfClauses);
                                }

                                match pair {
                                    Ast::SExpr(_, mut v) if v.len() == 2 => {
                                        match v.first() {
                                            Some(Ast::Symbol(_, sym)) if sym == "else" => {
                                                elsy = Some(Box::new(parse_ir_helper(v.remove(1), ir, module)?));
                                            }

                                            _ => {
                                                let body = parse_ir_helper(v.remove(1), ir, module)?;
                                                let cond = parse_ir_helper(v.remove(0), ir, module)?;
                                                vec.push((cond, body));
                                            }
                                        }
                                    }

                                    _ => return Err(IrParseError::MissingIfClauses),
                                }
                            }

                            let last = vec.remove(vec.len() - 1);
                            let mut result = SExpr::If(SExprMetadata::new(Span {
                                start: last.0.get_metadata().span.start,
                                end: if let Some(v) = &elsy {
                                    v.get_metadata().span.end
                                } else {
                                    last.1.get_metadata().span.end
                                }
                            }), Box::new(last.0), Box::new(last.1), elsy);
                            for i in (0..vec.len()).rev() {
                                let last = vec.remove(i);
                                result = SExpr::If(SExprMetadata::new(Span {
                                    start: last.0.get_metadata().span.start,
                                    end: result.get_metadata().span.end,
                                }), Box::new(last.0), Box::new(last.1), Some(Box::new(result)));
                            }

                            result.get_mut_metadata().span.start = span.start;
                            Ok(result)
                        }

                        "try" => {
                            if exprs.len() > 1 {
                                let body = parse_ir_helper(exprs.remove(0), ir, module)?;

                                enum State {
                                    Catch,
                                    Pattern,
                                    Statement
                                }
                                let mut state = State::Catch;
                                let mut pairs = vec![];
                                for expr in exprs {
                                    match state {
                                        State::Catch => {
                                            state = State::Pattern;
                                        }

                                        State::Pattern => {
                                            pairs.push((parse_pattern(expr)?, None));
                                            state = State::Statement;
                                        }

                                        State::Statement => {
                                            pairs.last_mut().unwrap().1 = Some(parse_ir_helper(expr, ir, module)?);
                                            state = State::Catch;
                                        }
                                    }
                                }

                                if !matches!(state, State::Catch) {
                                    Err(IrParseError::MissingTryClauses)
                                } else {
                                    let pairs = pairs.into_iter().map(|v| (v.0, v.1.unwrap())).collect();
                                    Ok(SExpr::Try(SExprMetadata::new(span), Box::new(body), pairs))
                                }
                            } else {
                                Err(IrParseError::MissingTryClauses)
                            }
                        }

                        "match" => {
                            let mut vec = vec![];
                            let mut elsy = None;
                            for pair in exprs {
                                if elsy.is_some() {
                                    return Err(IrParseError::MissingIfClauses);
                                }

                                match pair {
                                    Ast::SExpr(_, mut v) if v.len() == 2 => {
                                        match v.first() {
                                            Some(Ast::Symbol(_, sym)) if sym == "else" => {
                                                elsy = Some(Box::new(parse_ir_helper(v.remove(1), ir, module)?));
                                            }

                                            _ => {
                                                let body = parse_ir_helper(v.remove(1), ir, module)?;
                                                let cond = parse_pattern(v.remove(0))?;
                                                vec.push((cond, body));
                                            }
                                        }
                                    }

                                    _ => return Err(IrParseError::MissingIfClauses),
                                }
                            }

                            Ok(SExpr::Match(SExprMetadata::new(span), vec, elsy))
                        }

                        "loop" => {
                            if exprs.len() == 1 {
                                Ok(SExpr::Loop(SExprMetadata::new(span), None, Box::new(parse_ir_helper(exprs.remove(0), ir, module)?)))
                            } else if exprs.len() == 3 {
                                let body = parse_ir_helper(exprs.remove(2), ir, module)?;
                                let cont = parse_ir_helper(exprs.remove(1), ir, module)?;
                                let pattern = parse_pattern(exprs.remove(0))?;
                                Ok(SExpr::Loop(SExprMetadata::new(span), Some((pattern, Box::new(cont))), Box::new(body)))
                            } else {
                                Err(IrParseError::InvalidLoopArgs)
                            }
                        }

                        "fn" => {
                            if exprs.len() < 2 {
                                return Err(IrParseError::InvalidFunctionValue);
                            }

                            let (name, generics, lifetimes) = match exprs.remove(0) {
                                Ast::Symbol(_, sym) => (sym, vec![], vec![]),
                                Ast::SExpr(_, mut sexpr) => {
                                    if sexpr.is_empty() {
                                        return Err(IrParseError::InvalidFunctionName);
                                    }

                                    let name = match sexpr.remove(0) {
                                        Ast::Symbol(_, sym) => sym,
                                        _ => return Err(IrParseError::InvalidFunctionName),
                                    };

                                    let mut generics = vec![];
                                    let mut lifetimes = vec![];
                                    for val in sexpr {
                                        match val {
                                            Ast::Symbol(_, generic) => generics.push(generic),
                                            Ast::Lifetime(_, lifetime) => lifetimes.push(lifetime),
                                            _ => return Err(IrParseError::InvalidFunctionGeneric),
                                        }
                                    }

                                    (name, generics, lifetimes)
                                }

                                _ => return Err(IrParseError::InvalidFunctionName)
                            };

                            let body = parse_ir_helper(exprs.remove(exprs.len() - 1), ir, module)?;

                            let mut args = vec![];
                            let mut return_type = Type::Unassigned;

                            if !exprs.is_empty() {
                                let generics_set = generics.iter().collect();
                                let include_types = !generics.is_empty() || matches!(exprs[0], Ast::SExpr(_, _));
                                if include_types {
                                    return_type = match types::parse_type(exprs.remove(exprs.len() - 1), &generics_set) {
                                        Ok(v) => v,
                                        Err(e) => return Err(IrParseError::TypeParseError(e)),
                                    };
                                }

                                for arg in exprs {
                                    match arg {
                                        Ast::SExpr(_, mut vals) if include_types && vals.len() == 2 => {
                                            let type_ = match types::parse_type(vals.remove(1), &generics_set) {
                                                Ok(v) => v,
                                                Err(e) => return Err(IrParseError::TypeParseError(e)),
                                            };
                                            let name = match vals.remove(0) {
                                                Ast::Symbol(_, sym) => sym,
                                                _ => return Err(IrParseError::InvalidFunctionArg),
                                            };
                                            args.push((name, type_));
                                        }

                                        Ast::Symbol(_, sym) if !include_types => args.push((sym, Type::Unassigned)),

                                        _ => return Err(IrParseError::InvalidFunctionArg),
                                    }
                                }
                            }

                            Ok(SExpr::Function(SExprMetadata::new(span), name, generics, lifetimes, args, return_type, Box::new(body)))
                        }

                        "let" => {
                            if exprs.len() == 2 {
                                let value = parse_ir_helper(exprs.remove(1), ir, module)?;
                                let pattern = parse_pattern(exprs.remove(0))?;
                                Ok(SExpr::Let(SExprMetadata::new(span), pattern, Type::Unassigned, Box::new(value)))
                            } else if exprs.len() == 3 {
                                let value = parse_ir_helper(exprs.remove(2), ir, module)?;
                                let type_ = match types::parse_type(exprs.remove(1), &HashSet::new()) {
                                    Ok(v) => v,
                                    Err(e) => return Err(IrParseError::TypeParseError(e)),
                                };
                                let pattern = parse_pattern(exprs.remove(0))?;
                                Ok(SExpr::Let(SExprMetadata::new(span), pattern, type_, Box::new(value)))
                            } else {
                                Err(IrParseError::InvalidLet)
                            }
                        }

                        "=" => {
                            if exprs.len() == 2 {
                                let value = parse_ir_helper(exprs.remove(1), ir, module)?;
                                let var = parse_ir_helper(exprs.remove(0), ir, module)?;
                                Ok(SExpr::Assign(SExprMetadata::new(span), Box::new(var), None, Box::new(value)))
                            } else {
                                Err(IrParseError::InvalidAssign)
                            }
                        }

                        "+=" => {
                            if exprs.len() == 2 {
                                let value = parse_ir_helper(exprs.remove(1), ir, module)?;
                                let var = parse_ir_helper(exprs.remove(0), ir, module)?;
                                Ok(SExpr::Assign(SExprMetadata::new(span), Box::new(var), Some(NativeOperation::Add), Box::new(value)))
                            } else {
                                Err(IrParseError::InvalidAssign)
                            }
                        }

                        "-=" => {
                            if exprs.len() == 2 {
                                let value = parse_ir_helper(exprs.remove(1), ir, module)?;
                                let var = parse_ir_helper(exprs.remove(0), ir, module)?;
                                Ok(SExpr::Assign(SExprMetadata::new(span), Box::new(var), Some(NativeOperation::Sub), Box::new(value)))
                            } else {
                                Err(IrParseError::InvalidAssign)
                            }
                        }

                        "*=" => {
                            if exprs.len() == 2 {
                                let value = parse_ir_helper(exprs.remove(1), ir, module)?;
                                let var = parse_ir_helper(exprs.remove(0), ir, module)?;
                                Ok(SExpr::Assign(SExprMetadata::new(span), Box::new(var), Some(NativeOperation::Mul), Box::new(value)))
                            } else {
                                Err(IrParseError::InvalidAssign)
                            }
                        }

                        "/=" => {
                            if exprs.len() == 2 {
                                let value = parse_ir_helper(exprs.remove(1), ir, module)?;
                                let var = parse_ir_helper(exprs.remove(0), ir, module)?;
                                Ok(SExpr::Assign(SExprMetadata::new(span), Box::new(var), Some(NativeOperation::Div), Box::new(value)))
                            } else {
                                Err(IrParseError::InvalidAssign)
                            }
                        }

                        "%=" => {
                            if exprs.len() == 2 {
                                let value = parse_ir_helper(exprs.remove(1), ir, module)?;
                                let var = parse_ir_helper(exprs.remove(0), ir, module)?;
                                Ok(SExpr::Assign(SExprMetadata::new(span), Box::new(var), Some(NativeOperation::Mod), Box::new(value)))
                            } else {
                                Err(IrParseError::InvalidAssign)
                            }
                        }

                        "<<=" => {
                            if exprs.len() == 2 {
                                let value = parse_ir_helper(exprs.remove(1), ir, module)?;
                                let var = parse_ir_helper(exprs.remove(0), ir, module)?;
                                Ok(SExpr::Assign(SExprMetadata::new(span), Box::new(var), Some(NativeOperation::Bsl), Box::new(value)))
                            } else {
                                Err(IrParseError::InvalidAssign)
                            }
                        }

                        ">>=" => {
                            if exprs.len() == 2 {
                                let value = parse_ir_helper(exprs.remove(1), ir, module)?;
                                let var = parse_ir_helper(exprs.remove(0), ir, module)?;
                                Ok(SExpr::Assign(SExprMetadata::new(span), Box::new(var), Some(NativeOperation::Bsr), Box::new(value)))
                            } else {
                                Err(IrParseError::InvalidAssign)
                            }
                        }

                        "&=" => {
                            if exprs.len() == 2 {
                                let value = parse_ir_helper(exprs.remove(1), ir, module)?;
                                let var = parse_ir_helper(exprs.remove(0), ir, module)?;
                                Ok(SExpr::Assign(SExprMetadata::new(span), Box::new(var), Some(NativeOperation::BitAnd), Box::new(value)))
                            } else {
                                Err(IrParseError::InvalidAssign)
                            }
                        }

                        "|=" => {
                            if exprs.len() == 2 {
                                let value = parse_ir_helper(exprs.remove(1), ir, module)?;
                                let var = parse_ir_helper(exprs.remove(0), ir, module)?;
                                Ok(SExpr::Assign(SExprMetadata::new(span), Box::new(var), Some(NativeOperation::BitOr), Box::new(value)))
                            } else {
                                Err(IrParseError::InvalidAssign)
                            }
                        }

                        "^=" => {
                            if exprs.len() == 2 {
                                let value = parse_ir_helper(exprs.remove(1), ir, module)?;
                                let var = parse_ir_helper(exprs.remove(0), ir, module)?;
                                Ok(SExpr::Assign(SExprMetadata::new(span), Box::new(var), Some(NativeOperation::BitXor), Box::new(value)))
                            } else {
                                Err(IrParseError::InvalidAssign)
                            }
                        }

                        "." => {
                            if exprs.len() >= 2 {
                                let exprs = exprs.into_iter().map(|v| if let Ast::Symbol(_, sym) = v {
                                    Ok(sym)
                                } else {
                                    Err(IrParseError::InvalidAttribute)
                                }).collect::<Result<Vec<_>, _>>()?;
                                Ok(SExpr::Attribute(SExprMetadata::new(span), exprs))
                            } else {
                                Err(IrParseError::InvalidAttribute)
                            }
                        }

                        "struct" => {
                            if exprs.is_empty() {
                                Err(IrParseError::InvalidStructName)
                            } else {
                                let (name, generics, lifetimes) = match exprs.remove(0) {
                                    Ast::Symbol(_, sym) => (sym, vec![], vec![]),
                                    Ast::SExpr(_, mut sexpr) => {
                                        let name = match sexpr.remove(0) {
                                            Ast::Symbol(_, sym) => sym,
                                            _ => return Err(IrParseError::InvalidStructName),
                                        };

                                        let mut generics = vec![];
                                        let mut lifetimes = vec![];
                                        for expr in sexpr {
                                            match expr {
                                                Ast::Symbol(_, generic) => generics.push(generic),
                                                Ast::Lifetime(_, lifetime) => lifetimes.push(lifetime),
                                                _ => return Err(IrParseError::InvalidStructGeneric)
                                            }
                                        }

                                        (name, generics, lifetimes)
                                    }

                                    _ => return Err(IrParseError::InvalidStructName),
                                };

                                let mut fields = vec![];
                                let generics_set = generics.iter().collect();
                                for expr in exprs {
                                    match expr {
                                        Ast::SExpr(_, mut vals) if vals.len() == 2 => {
                                            let type_ = match types::parse_type(vals.remove(1), &generics_set) {
                                                Ok(v) => v,
                                                Err(e) => return Err(IrParseError::TypeParseError(e)),
                                            };

                                            let name = match vals.remove(0) {
                                                Ast::Symbol(_, sym) => sym,
                                                _ => return Err(IrParseError::InvalidStructField),
                                            };

                                            fields.push((name, type_));
                                        }

                                        _ => return Err(IrParseError::InvalidStructField)
                                    }
                                }

                                Ok(SExpr::Struct(SExprMetadata::new(span), name, generics, lifetimes, fields))
                            }
                        }

                        "enum" => {
                            if exprs.len() < 2 {
                                Err(IrParseError::InvalidEnum)
                            } else {
                                let (name, type_) = match exprs.remove(0) {
                                    Ast::Symbol(_, sym) => (sym, Type::I32),
                                    Ast::SExpr(_, mut vals) if vals.len() == 2 => {
                                        let type_ = match types::parse_type(vals.remove(1), &HashSet::new()) {
                                            Ok(v) => v,
                                            Err(e) => return Err(IrParseError::TypeParseError(e)),
                                        };
                                        let name = match vals.remove(0) {
                                            Ast::Symbol(_, sym) => sym,
                                            _ => return Err(IrParseError::InvalidEnum),
                                        };

                                        (name, type_)
                                    }

                                    _ => return Err(IrParseError::InvalidEnum)
                                };

                                let mut variants = vec![];
                                for expr in exprs {
                                    match expr {
                                        Ast::Symbol(_, sym) => variants.push((sym, None)),
                                        Ast::SExpr(_, mut vals) if vals.len() == 2 => {
                                            let val = match vals.remove(1) {
                                                Ast::Int(_, i) => i,
                                                _ => return Err(IrParseError::InvalidEnumVariant),
                                            };

                                            let name = match vals.remove(0) {
                                                Ast::Symbol(_, sym) => sym,
                                                _ => return Err(IrParseError::InvalidEnumVariant),
                                            };

                                            variants.push((name, Some(val)));
                                        }

                                        _ => return Err(IrParseError::InvalidEnumVariant)
                                    }
                                }

                                Ok(SExpr::Enum(SExprMetadata::new(span), name, type_, variants))
                            }
                        }

                        "tuple" => Ok(SExpr::Tuple(SExprMetadata::new(span), parse_exprs(exprs, ir, module)?)),

                        "type" => {
                            if exprs.len() != 2 {
                                Err(IrParseError::InvalidTypeDef)
                            } else {
                                let (name, generics, lifetimes) = match exprs.remove(0) {
                                    Ast::Symbol(_, sym) => (sym, vec![], vec![]),
                                    Ast::SExpr(_, mut vals) if vals.len() > 1 => {
                                        let name = match vals.remove(0) {
                                            Ast::Symbol(_, sym) => sym,
                                            _ => return Err(IrParseError::InvalidTypeDef),
                                        };

                                        let mut generics = vec![];
                                        let mut lifetimes = vec![];
                                        for v in vals {
                                            match v {
                                                Ast::Symbol(_, generic) => generics.push(generic),
                                                Ast::Lifetime(_, lifetime) => lifetimes.push(lifetime),
                                                _ => return Err(IrParseError::InvalidTypeDef),
                                            }
                                        }

                                        (name, generics, lifetimes)
                                    }

                                    _ => return Err(IrParseError::InvalidTypeDef),
                                };

                                let type_ = match types::parse_type(exprs.remove(0), &generics.iter().collect()) {
                                    Ok(v) => v,
                                    Err(e) => return Err(IrParseError::TypeParseError(e)),
                                };

                                Ok(SExpr::TypeDefinition(SExprMetadata::new(span), name, generics, lifetimes, type_))
                            }
                        }

                        "seq" => Ok(SExpr::Seq(SExprMetadata::new(span), parse_exprs(exprs, ir, module)?)),

                        _ => Ok(SExpr::Application(SExprMetadata::new(span), Box::new(SExpr::Symbol(SExprMetadata::new(f_span), sym)), parse_exprs(exprs, ir, module)?))
                    }
                }

                Ast::SExpr(_, _) => todo!(),
            }
        }
    }
}

pub fn parse_ir(ast: Ast) -> Result<Ir, IrParseError> {
    match ast {
        Ast::Program(span, exprs) => {
            let mut ir = Ir {
                modules: HashMap::new()
            };

            let mut module = IrModule {
                root: SExpr::Nil(SExprMetadata::default()),
                types: HashMap::new(),
                funcs: HashMap::new(),
            };

            let mut seq = vec![];
            for expr in exprs {
                seq.push(parse_ir_helper(expr, &mut ir, &mut module)?);
            }
            module.root = SExpr::Seq(SExprMetadata::new(span), seq);

            ir.modules.insert(vec![], module);
            Ok(ir)
        }

        _ => Err(IrParseError::NonRootPassedIntoRootFunction)
    }
}


