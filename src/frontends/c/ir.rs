use std::collections::HashMap;

use super::ast::Ast;
use super::super::super::middleend::ir::{IrModule, SExpr, SExprMetadata, NativeOperation, Pattern};
use super::super::super::middleend::types::Type;

#[derive(Debug)]
pub enum IrError {
    InvalidOp,
    InvalidAttribute,
    InvalidPatternPos,
    EmptyBox,
    InvalidContinue,
    InvalidBreak,
    InvalidReturn,
    InvalidThrow,
    InvalidUnreachable,
    InvalidIf,
    InvalidCond,
    InvalidTry,
    InvalidMatch,
    InvalidLoop,
    InvalidBox,
}

fn lowering_helper(ast: Ast) -> Result<SExpr, IrError> {
    match ast {
        Ast::Int(i) => Ok(SExpr::Int(SExprMetadata::new(0..0), i)),
        Ast::Word(w) => Ok(SExpr::Word(SExprMetadata::new(0..0), w)),
        Ast::Float(f) => Ok(SExpr::Float(SExprMetadata::new(0..0), f)),
        Ast::Char(c) => Ok(SExpr::Char(SExprMetadata::new(0..0), c)),
        Ast::True => Ok(SExpr::Bool(SExprMetadata::new_with_type(0..0, Type::Bool), true)),
        Ast::False => Ok(SExpr::Bool(SExprMetadata::new_with_type(0..0, Type::Bool), false)),
        Ast::String(s) => Ok(SExpr::String(SExprMetadata::new_with_type(0..0, Type::ConstRef(Box::new(Type::TypeName(String::from("str"), vec![])))), s)),
        Ast::Symbol(s) => Ok(SExpr::Symbol(SExprMetadata::new(0..0), s)),

        Ast::Infix(op, left, right) => {
            let (is_assign, op) = match op.as_str() {
                "*" => (false, Some(NativeOperation::Mul)),
                "/" => (false, Some(NativeOperation::Div)),
                "%" => (false, Some(NativeOperation::Mod)),
                "+" => (false, Some(NativeOperation::Add)),
                "-" => (false, Some(NativeOperation::Sub)),
                "<<"=> (false, Some(NativeOperation::Bsl)),
                ">>" => (false, Some(NativeOperation::Bsr)),
                "&" => (false, Some(NativeOperation::BitAnd)),
                "|" => (false, Some(NativeOperation::BitOr)),
                "^" => (false, Some(NativeOperation::BitXor)),
                "&&" => (false, Some(NativeOperation::BoolAnd)),
                "||" => (false, Some(NativeOperation::BoolOr)),
                "^^" => (false, Some(NativeOperation::BoolXor)),
                "<" => (false, Some(NativeOperation::Lt)),
                ">" => (false, Some(NativeOperation::Gt)),
                "<=" => (false, Some(NativeOperation::Lte)),
                ">=" => (false, Some(NativeOperation::Gte)),
                "==" => (false, Some(NativeOperation::Eq)),
                "!=" => (false, Some(NativeOperation::Ne)),

                "=" => (true, None),
                "*=" => (true, Some(NativeOperation::Mul)),
                "/=" => (true, Some(NativeOperation::Div)),
                "%=" => (true, Some(NativeOperation::Mod)),
                "+=" => (true, Some(NativeOperation::Add)),
                "-=" => (true, Some(NativeOperation::Sub)),
                "<<="=> (true, Some(NativeOperation::Bsl)),
                ">>=" => (true, Some(NativeOperation::Bsr)),
                "&=" => (true, Some(NativeOperation::BitAnd)),
                "|=" => (true, Some(NativeOperation::BitOr)),
                "^=" => (true, Some(NativeOperation::BitXor)),

                _ => return Err(IrError::InvalidOp),
            };

            if is_assign {
                Ok(SExpr::Assign(SExprMetadata::new(0..0), Box::new(lowering_helper(*left)?), op, Box::new(lowering_helper(*right)?)))
            } else {
                Ok(SExpr::Native(SExprMetadata::new(0..0), op.unwrap(), vec![lowering_helper(*left)?, lowering_helper(*right)?]))
            }
        }

        Ast::Tuple(tuple) => {
            let mut vec = vec![];
            for v in tuple {
                vec.push(lowering_helper(v)?);
            }

            if vec.is_empty() {
                Ok(SExpr::Nil(SExprMetadata::new_with_type(0..0, Type::Nil)))
            } else {
                Ok(SExpr::Tuple(SExprMetadata::new(0..0), vec))
            }
        }

        Ast::StructInit(s, f) => {
            Ok(SExpr::StructInit(SExprMetadata::new(0..0), s, f.into_iter().map(|v| {
                match lowering_helper(v.1) {
                    Ok(f) => Ok((v.0, f)),
                    Err(e) => Err(e),
                }
            }).collect::<Result<HashMap<_, _>, _>>()?))
        }

        Ast::Application(f, t, a) => {
            let f = lowering_helper(*f)?;
            let mut vec = vec![];
            for a in a {
                vec.push(lowering_helper(a)?);
            }
            Ok(SExpr::Application(SExprMetadata::new_with_type(0..0, t), Box::new(f), vec))
        }

        Ast::Attribute(v) => {
            let mut attrs = vec![];

            for v in v {
                match v {
                    Ast::Symbol(v) => attrs.push(SExpr::Symbol(SExprMetadata::new(0..0), v)),
                    Ast::Application(_, _, _) => {
                        let mut v = lowering_helper(v)?;

                        if let SExpr::Application(_, _, a) = &mut v {
                            if attrs.len() == 1 {
                                a.insert(0, attrs.remove(0));
                            } else {
                                a.insert(0, SExpr::Attribute(SExprMetadata::new(0..0), attrs));
                            }
                            attrs = vec![v];
                        } else {
                            unreachable!();
                        }
                    }

                    _ => return Err(IrError::InvalidAttribute),
                }
            }

            if attrs.is_empty() {
                Err(IrError::InvalidAttribute)
            } else if attrs.len() == 1 {
                Ok(attrs.remove(0))
            } else {
                Ok(SExpr::Attribute(SExprMetadata::new(0..0), attrs))
            }
        }

        Ast::Prefix(op, val) => {
            let val = lowering_helper(*val)?;
            match op.as_str() {
                "*" => Ok(SExpr::Deref(SExprMetadata::new(0..0), Box::new(val))),
                "&" => Ok(SExpr::Ref(SExprMetadata::new(0..0), Box::new(val))),
                "&mut" => Ok(SExpr::MutRef(SExprMetadata::new(0..0), Box::new(val))),
                "-" => Ok(SExpr::Native(SExprMetadata::new(0..0), NativeOperation::Neg, vec![val])),
                "!" => Ok(SExpr::Native(SExprMetadata::new(0..0), NativeOperation::Invert, vec![val])),
                _ => Err(IrError::InvalidOp),
            }
        }

        Ast::TypeCast(t, e) => {
            let mut e = lowering_helper(*e)?;
            e.get_mut_metadata().type_ = t;
            Ok(e)
        }

        Ast::Pattern(_) => Err(IrError::InvalidPatternPos),

        Ast::Let(p, v) => {
            let p = if let Ast::Pattern(p) = *p {
                p
            } else {
                unreachable!();
            };
            Ok(SExpr::Let(SExprMetadata::new(0..0), p, Type::Unassigned, Box::new(lowering_helper(*v)?)))
        }

        Ast::Block(vals, returns_value) => {
            let mut vals = vals.into_iter().map(lowering_helper).collect::<Result<Vec<_>, _>>()?;
            if !returns_value {
                vals.push(SExpr::Nil(SExprMetadata::new_with_type(0..0, Type::Nil)));
            }
            Ok(SExpr::Seq(SExprMetadata::new(0..0), vals))
        }

        Ast::Box(mut vals) => {
            if vals.is_empty() {
                Err(IrError::EmptyBox)
            } else {
                match vals[0].0.as_str() {
                    "continue" => {
                        if vals.len() == 1 {
                            let value = if vals[0].1.is_empty() {
                                None
                            } else if vals[0].1.len() == 1{
                                Some(Box::new(lowering_helper(vals[0].1.remove(0))?))
                            } else {
                                return Err(IrError::InvalidContinue);
                            };

                            Ok(SExpr::Return(SExprMetadata::new(0..0), value))
                        } else {
                            Err(IrError::InvalidContinue)
                        }
                    }

                    "break" => {
                        if vals.len() == 1 {
                            let value = if vals[0].1.is_empty() {
                                None
                            } else if vals[0].1.len() == 1{
                                Some(Box::new(lowering_helper(vals[0].1.remove(0))?))
                            } else {
                                return Err(IrError::InvalidBreak);
                            };

                            Ok(SExpr::Return(SExprMetadata::new(0..0), value))
                        } else {
                            Err(IrError::InvalidBreak)
                        }
                    }

                    "return" => {
                        if vals.len() == 1 {
                            let value = if vals[0].1.is_empty() {
                                None
                            } else if vals[0].1.len() == 1{
                                Some(Box::new(lowering_helper(vals[0].1.remove(0))?))
                            } else {
                                return Err(IrError::InvalidReturn);
                            };

                            Ok(SExpr::Return(SExprMetadata::new(0..0), value))
                        } else {
                            Err(IrError::InvalidReturn)
                        }
                    }

                    "throw" => {
                        if vals.len() == 1 {
                            let value = if vals[0].1.len() == 1{
                                Box::new(lowering_helper(vals[0].1.remove(0))?)
                            } else {
                                return Err(IrError::InvalidThrow);
                            };

                            Ok(SExpr::Throw(SExprMetadata::new(0..0), value))
                        } else {
                            Err(IrError::InvalidThrow)
                        }
                    }

                    "unreachable" => {
                        if vals.len() == 1 && (vals[0].1.is_empty() || vals[0].1.len() == 1) {
                            Ok(SExpr::Unreachable(SExprMetadata::new(0..0)))
                        } else {
                            Err(IrError::InvalidUnreachable)
                        }
                    }

                    "if" => {
                        if vals.len() == 1 {
                            let (_, mut v) = vals.remove(0);
                            if v.len() == 2 {
                                let body = lowering_helper(v.remove(1))?;
                                let cond = lowering_helper(v.remove(0))?;
                                Ok(SExpr::If(SExprMetadata::new(0..0), Box::new(cond), Box::new(body), None))
                            } else {
                                Err(IrError::InvalidIf)
                            }
                        } else if vals.len() == 2 {
                            let (name, mut elsy) = vals.remove(1);
                            let (_, mut then) = vals.remove(0);

                            if name == "else" && then.len() == 2 && elsy.len() == 1 {
                                let elsy = lowering_helper(elsy.remove(0))?;
                                let body = lowering_helper(then.remove(1))?;
                                let cond = lowering_helper(then.remove(0))?;
                                Ok(SExpr::If(SExprMetadata::new(0..0), Box::new(cond), Box::new(body), Some(Box::new(elsy))))
                            } else {
                                Err(IrError::InvalidIf)
                            }
                        } else {
                            Err(IrError::InvalidIf)
                        }
                    }

                    "cond" => {
                        if vals.len() == 1 && vals[0].1.len() == 1 {
                            let (_, mut vals) = vals.remove(0);
                            let body = vals.remove(0);

                            if let Ast::Block(mut cases, _) = body {
                                let mut top = match cases.last_mut() {
                                    Some(Ast::Box(v)) if v.len() == 1 && v[0].0 == "else" && v[0].1.len() == 1 => {
                                        let (_, mut v) = v.remove(v.len() - 1);
                                        let v = v.remove(0);
                                        Some(lowering_helper(v)?)
                                    }
                                    _ => None
                                };
                                if top.is_some() {
                                    cases.pop();
                                }

                                while let Some(v) = cases.pop() {
                                    if let Ast::Box(mut v) = v {
                                        if v.len() == 1 && v[0].0 == "case" && v[0].1.len() == 2 {
                                            let (_, mut v) = v.remove(0);
                                            let body = lowering_helper(v.remove(1))?;
                                            let cond = lowering_helper(v.remove(0))?;
                                            top = Some(SExpr::If(SExprMetadata::new(0..0), Box::new(cond), Box::new(body), top.map(Box::new)));
                                        } else {
                                            return Err(IrError::InvalidCond);
                                        }
                                    } else {
                                        return Err(IrError::InvalidCond);
                                    }
                                }

                                if let Some(v) = top {
                                    Ok(v)
                                } else {
                                    Err(IrError::InvalidCond)
                                }
                            } else {
                                Err(IrError::InvalidCond)
                            }
                        } else {
                            Err(IrError::InvalidCond)
                        }
                    }

                    "try" => {
                        let mut vals = vals.into_iter();
                        let (_, mut try_block) = vals.next().unwrap();
                        if try_block.len() != 1 {
                            return Err(IrError::InvalidTry);
                        }

                        let try_block = lowering_helper(try_block.remove(0))?;
                        let mut catches = vec![];
                        for (catch, mut vals) in vals {
                            if catch != "catch" || vals.len() != 2 {
                                return Err(IrError::InvalidTry);
                            }

                            let body = lowering_helper(vals.remove(1))?;

                            let pat = if let Ast::Pattern(p) = vals.remove(0) {
                                p
                            } else {
                                return Err(IrError::InvalidTry);
                            };

                            catches.push((pat, body));
                        }

                        Ok(SExpr::Try(SExprMetadata::new(0..0), Box::new(try_block), catches))
                    }

                    "match" => {
                        if vals.len() == 1 && vals[0].1.len() == 2 {
                            let body = vals[0].1.remove(1);
                            let value = lowering_helper(vals[0].1.remove(0))?;

                            if let Ast::Block(v, _) = body {
                                let mut vec = vec![];

                                for v in v {
                                    match v {
                                        Ast::Box(mut v) if v.len() == 1 && v[0].0 == "case" && v[0].1.len() == 2 => {
                                            let (_, mut v) = v.remove(0);
                                            let body = lowering_helper(v.remove(1))?;
                                            let pat = if let Ast::Pattern(p) = v.remove(0) {
                                                p
                                            } else {
                                                return Err(IrError::InvalidMatch);
                                            };
                                            vec.push((pat, body));
                                        }

                                        _ => return Err(IrError::InvalidMatch),
                                    }
                                }

                                Ok(SExpr::Match(SExprMetadata::new(0..0), Box::new(value), vec))
                            } else {
                                Err(IrError::InvalidMatch)
                            }
                        } else {
                            Err(IrError::InvalidMatch)
                        }
                    }

                    "loop" => {
                        if vals.len() == 1 && vals[0].1.len() == 1 {
                            Ok(SExpr::Loop(SExprMetadata::new(0..0), None, Box::new(lowering_helper(vals[0].1.remove(0))?)))
                        } else if vals.len() == 2 && vals[0].1.len() == 1 && vals[1].0 == "as" && vals[1].1.len() == 2 {
                            let (_, mut v) = vals.remove(1);
                            let body = lowering_helper(v.remove(1))?;
                            let pat = v.remove(0);
                            let pat = if let Ast::Pattern(p) = pat {
                                p
                            } else if let Ast::Symbol(s) = pat {
                                Pattern::Name(s)
                            } else {
                                return Err(IrError::InvalidLoop);
                            };
                            let (_, mut val) = vals.remove(0);
                            let val = lowering_helper(val.remove(0))?;
                            Ok(SExpr::Loop(SExprMetadata::new(0..0), Some((pat, Box::new(val))), Box::new(body)))
                        } else {
                            Err(IrError::InvalidLoop)
                        }
                    }

                    _ => Err(IrError::InvalidBox)
                }
            }
        }

        Ast::FuncDef(func_name, args, ret_type, body) => {
            let body = lowering_helper(*body)?;
            Ok(SExpr::Function(SExprMetadata::new(0..0), func_name, args, ret_type, Box::new(body)))
        }

        Ast::Struct(name, fields) => {
            Ok(SExpr::Struct(SExprMetadata::new(0..0), name, fields.into_iter().collect()))
        }

        Ast::Enum(name, t, variants) => {
            Ok(SExpr::Enum(SExprMetadata::new(0..0), name, t, variants))
        }
    }
}

pub fn lower_ast_to_ir(asts: Vec<Ast>) -> Result<IrModule, IrError> {
    let mut module = IrModule {
        sexprs: vec![],
        types: HashMap::new(),
        funcs: HashMap::new(),
    };
    for ast in asts {
        module.sexprs.push(lowering_helper(ast)?);
    }
    Ok(module)
}
