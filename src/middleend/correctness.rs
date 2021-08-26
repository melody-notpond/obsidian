use std::collections::HashMap;

use crate::middleend::ir::Signature;

use super::ir::{IrModule, SExpr};
use super::types::Type;

#[derive(Debug)]
pub enum CorrectnessError {
    UntypedRootFunction,
    DuplicateSignature,
    InvalidEnumType,
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

fn check_helper(sexpr: &mut SExpr) {
}

pub fn check(root: &mut IrModule) -> Result<(), Vec<CorrectnessError>> {
    let mut errors = vec![];

    collect_root_functions(root, &mut errors);
    collect_types(root, &mut errors);

    if !root.sexprs.is_empty() {
        return Err(errors);
    }

    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}
