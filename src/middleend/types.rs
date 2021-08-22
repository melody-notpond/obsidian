use std::collections::HashMap;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
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
    ISize,
    U8,
    U16,
    U32,
    U64,
    USize,

    UnassignedFloat,
    UnknownFloat(usize),
    F32,
    F64,

    ConstRef(Box<Type>),
    MutRef(Box<Type>),

    ConstPtr(Box<Type>),
    MutPtr(Box<Type>),

    Generic(String),
    TypeName(String, Vec<Type>),
    Tuple(Vec<Type>),
    VarArgs(Option<Box<Type>>),
    Wildcard,
}

impl Type {
    pub fn is_subtype(&self, supertype: &Type, generic_map: &HashMap<String, Type>) -> bool {
        use Type::*;

        match (self, supertype) {
            (Nil, Nil)
            | (Bool, Bool)
            | (I8, I8)
            | (I16, I16)
            | (I32, I32)
            | (I64, I64)
            | (ISize, ISize)
            | (U8, U8)
            | (U16, U16)
            | (U32, U32)
            | (U64, U64)
            | (USize, USize)
            | (F32, F32)
            | (F64, F64) => true,

            (ConstRef(t1), ConstRef(t2))
            | (MutRef(t1), MutRef(t2))
            | (ConstPtr(t1), ConstPtr(t2))
            | (MutPtr(t1), MutPtr(t2)) => t1.is_subtype(&**t2, generic_map),

            (t, Generic(s)) => {
                if let Some(st) = generic_map.get(s) {
                    t.is_subtype(st, generic_map)
                } else {
                    false
                }
            }

            (TypeName(name1, params1), TypeName(name2, params2)) =>
                name1 == name2 && params1.len() == params2.len() && params1.iter().zip(params2.iter()).all(|v| v.0.is_subtype(v.1, generic_map)),

            (Tuple(v1), Tuple(v2)) => v1.len() == v2.len() && v1.iter().zip(v2.iter()).all(|v| v.0.is_subtype(v.1, generic_map)),

            (VarArgs(_), VarArgs(None)) => true,
            (VarArgs(Some(v1)), VarArgs(Some(v2))) => v1.is_subtype(v2, generic_map),

            (_, Wildcard) => true,

            _ => false,
        }
    }
}
