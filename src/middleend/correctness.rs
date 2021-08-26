use std::collections::HashMap;

use crate::middleend::ir::{SExprMetadata, Signature};

use super::ir::{IrModule, SExpr};
use super::types::Type;

#[derive(Debug)]
pub enum CorrectnessError {
    UntypedRootFunction,
    DuplicateSignature,
    InvalidEnumType,
    MismatchedTypes,
    UnknownSymbol,
    InvalidFunctionSignature,
}

fn collect_root_functions(root: &mut IrModule, errors: &mut Vec<CorrectnessError>) {
    let mut funcs = vec![];
    for (i, sexpr) in root.sexprs.iter().enumerate() {
        match sexpr {
            SExpr::Function(_, _, _, Type::Unassigned, _) => {
                errors.push(CorrectnessError::UntypedRootFunction);
            }

            SExpr::Function(_, _, _, _, _) => {
                funcs.push(i);
            }

            _ => (),
        }
    }

    for (i, func) in funcs.into_iter().enumerate() {
        let func = root.sexprs.remove(func - i);
        let (name, args, ret_type) =
            if let SExpr::Function(_, name, arguments, ret_type, _) = &func {
                (name, arguments.iter().map(|v| &v.1), ret_type)
            } else {
                unreachable!()
            };

        if let Some(signatures) = root.funcs.get_mut(name) {
            let mut contained = false;
            for (signature, _) in signatures.iter() {
                if signature.args.iter().zip(args.clone()).all(|v| v.0 == v.1) && *ret_type == signature.ret_type {
                    errors.push(CorrectnessError::DuplicateSignature);
                    contained = true;
                    break;
                }
            }

            if !contained {
                signatures.push((Signature {
                    args: args.cloned().collect(),
                    ret_type: ret_type.clone(),
                }, func));
            }
        } else {
            let name = name.clone();
            let signature = Signature {
                args: args.cloned().collect(),
                ret_type: ret_type.clone(),
            };
            root.funcs.insert(name, vec![(signature, func)]);
        }
    }
}

fn collect_types(root: &mut IrModule, errors: &mut Vec<CorrectnessError>) {
    let mut types = vec![];
    for (i, sexpr) in root.sexprs.iter().enumerate() {
        match sexpr {
            SExpr::Struct(_, _, _) => {
                types.push(i);
            }

            SExpr::Enum(_, _, Type::I(_) | Type::U(_), _) => {
                types.push(i);
            }

            SExpr::Enum(_, _, _, _) => {
                errors.push(CorrectnessError::InvalidEnumType);
            }

            _ => (),
        }
    }

    for (i, type_) in types.into_iter().enumerate() {
        let type_ = root.sexprs.remove(type_ - i);
        match &type_ {
            SExpr::Struct(_, name, _)
            | SExpr::Enum(_, name, _, _) => {
                root.types.insert(name.clone(), type_);
            }

            _ => unreachable!(),
        }
    }
}

fn check_helper(funcs: &HashMap<String, Vec<Signature>>, sexpr: &mut SExpr, errors: &mut Vec<CorrectnessError>, i: &mut usize) {
    match sexpr {
        SExpr::Int(m, _) => {
            if m.type_ == Type::UnassignedInt {
                m.type_ = Type::UnknownInt(*i);
                *i += 1;
            }
        }

        SExpr::Float(m, _) => {
            if m.type_ == Type::UnassignedFloat {
                m.type_ = Type::UnknownFloat(*i);
                *i += 1;
            }
        }

        SExpr::Word(m, _) => {
            if m.type_ == Type::UnassignedWord {
                m.type_ = Type::UnknownWord(*i);
                *i += 1;
            }
        }

        SExpr::Char(_, _) => (),
        SExpr::String(_, _) => (),

        SExpr::Symbol(m, s) => {
            // TODO: locals
            if let Some(v) = funcs.get(s) {
                m.signatures = v.clone();
            } else {
                errors.push(CorrectnessError::UnknownSymbol);
            }
        }

        SExpr::Native(_, _, _) => todo!(),
        SExpr::Nil(_) => (),
        SExpr::Ref(_, _) => todo!(),
        SExpr::MutRef(_, _) => todo!(),
        SExpr::Deref(_, _) => todo!(),

        SExpr::Application(m, func, args) => {
            check_helper(funcs, func, errors, i);
            for arg in args.iter_mut() {
                check_helper(funcs, arg, errors, i);
            }

            let signatures = &mut func.get_mut_metadata().signatures;
            let mut i = 0;
            while i < signatures.len() {
                let signature = &signatures[i];
                let mut fails = signature.args.len() != args.len() || (signature.args.len() <= args.len() && matches!(signature.args.last(), Some(Type::VarArgs(_))));
                if !fails {
                    for (arg1, arg2) in signature.args.iter().zip(args.iter().map(|v| &v.get_metadata().type_)) {
                        if !arg2.is_subtype(arg1, &HashMap::new()) {
                            fails = true;
                            break;
                        }
                    }
                }

                if fails || (!m.type_.is_unspecified() && !signature.ret_type.is_subtype(&m.type_, &HashMap::new())) {
                    signatures.remove(i);
                } else {
                    i += 1;
                }
            }

            if signatures.is_empty() {
                errors.push(CorrectnessError::InvalidFunctionSignature);
            }
        }

        SExpr::StructInit(_, _, _) => todo!(),
        SExpr::Continue(_, _) => todo!(),
        SExpr::Break(_, _) => todo!(),
        SExpr::Return(_, _) => todo!(),
        SExpr::Throw(_, _) => todo!(),
        SExpr::Unreachable(_) => todo!(),
        SExpr::If(_, _, _, _) => todo!(),
        SExpr::Try(_, _, _) => todo!(),
        SExpr::Match(_, _, _) => todo!(),
        SExpr::Loop(_, _, _) => todo!(),

        SExpr::Function(_, _, _args, ret_type, body) => {
            check_helper(funcs, body, errors, i);

            let type_ = &body.get_metadata().type_;
            if *type_ == Type::Unassigned
            || (*type_ == Type::UnassignedInt && matches!(ret_type, Type::I(_)))
            || (*type_ == Type::UnassignedWord && matches!(ret_type, Type::U(_)))
            || (*type_ == Type::UnassignedFloat && matches!(ret_type, Type::F32 | Type::F64)) {
                todo!();
            } else if type_ != ret_type {
                errors.push(CorrectnessError::MismatchedTypes);
            }
        }

        SExpr::Let(_, _, _, _) => todo!(),
        SExpr::Assign(_, _, _, _) => todo!(),
        SExpr::Attribute(_, _) => todo!(),
        SExpr::Struct(_, _, _) => todo!(),
        SExpr::Enum(_, _, _, _) => todo!(),
        SExpr::Tuple(_, _) => todo!(),
        SExpr::TypeDefinition(_, _, _, _, _) => todo!(),

        SExpr::Seq(m, block) => {
            for sexpr in block.iter_mut() {
                check_helper(funcs, sexpr, errors, i);
            }
            if let Some(sexpr) = block.last() {
                if sexpr.get_metadata().type_.is_unspecified() {
                    m.type_ = sexpr.get_metadata().type_.clone();
                }
            } else {
                m.type_ = Type::Nil;
            }
        }
    }
}

pub fn check(root: &mut IrModule) -> Result<(), Vec<CorrectnessError>> {
    let mut errors = vec![];

    collect_root_functions(root, &mut errors);
    collect_types(root, &mut errors);

    let sigs = root.funcs.iter().map(|v| (v.0.clone(), v.1.iter().map(|v| v.0.clone()).collect())).collect();
    for (_, signatures) in root.funcs.iter_mut() {
        for (_, sexpr) in signatures {
            use std::mem::swap;
            let mut temp = SExpr::Nil(SExprMetadata::new(0..0));
            swap(&mut temp, sexpr);

            let mut i = 0;
            check_helper(&sigs, &mut temp, &mut errors, &mut i);

            swap(&mut temp, sexpr);
        }
    }

    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}
