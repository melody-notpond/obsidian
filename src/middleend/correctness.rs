use std::collections::HashMap;

use super::ir::{IrModule, SExpr};
use super::types::Type;

pub enum CorrectnessError {
    UntypedRootFunction,
    DuplicateSignature,
}

fn collect_root_functions(root: &mut IrModule, errors: &mut Vec<CorrectnessError>) {
    let mut funcs = vec![];
    for (i, sexpr) in root.sexprs.iter().enumerate() {
        match sexpr {
            SExpr::Function(_, _, _, _, _, Type::Unassigned, _) => {
                errors.push(CorrectnessError::UntypedRootFunction);
            }

            SExpr::Function(_, _, _, _, _, _, _) => {
                funcs.push(i);
            }

            _ => (),
        }
    }

    for (i, func) in funcs.into_iter().enumerate() {
        let func = root.sexprs.remove(func - i);
        let (name, args, ret_type) = if let SExpr::Function(_, name, _, _, arguments, ret_type, _) = &func {
            (name, arguments.iter().map(|v| &v.1), ret_type)
        } else {
            unreachable!()
        };

        if let Some(signatures) = root.funcs.get_mut(name) {
            let mut index = None;
            for (i, (args2, _)) in signatures.iter().enumerate() {
                if args2.iter().zip(args.clone()).all(|v| v.0 == v.1) {
                    index = Some(i);
                    break;
                }
            }

            if let Some(index) = index {
                use std::collections::hash_map::Entry::*;

                let (_, rets) = signatures.get_mut(index).unwrap();
                match rets.entry(ret_type.clone()) {
                    Occupied(_) => {
                        errors.push(CorrectnessError::DuplicateSignature);
                    }

                    Vacant(v) => {
                        v.insert(func);
                    }
                }
            } else {
                let mut map = HashMap::new();
                let args = args.cloned().collect();
                map.insert(ret_type.clone(), func);
                signatures.push((args, map));
            }
        } else {
            let mut map = HashMap::new();
            let name = name.clone();
            let args = args.cloned().collect();
            map.insert(ret_type.clone(), func);
            root.funcs.insert(name, vec![(args, map)]);
        }
    }
}

pub fn check(root: &mut IrModule) -> Result<(), Vec<CorrectnessError>> {
    let mut errors = vec![];

    collect_root_functions(root, &mut errors);

    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}
