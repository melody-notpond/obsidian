use std::collections::HashMap;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Type {
    Unassigned,
    Unknown(usize),

    Nil,

    UnassignedInt,
    UnassignedWord,
    UnknownInt(usize),
    UnknownWord(usize),
    I(usize),
    ISize,
    U(usize),
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
    pub fn is_unspecified(&self) -> bool {
        !matches!(self, Type::Unassigned | Type::UnassignedInt | Type::UnassignedFloat | Type::UnassignedWord | Type::Unknown(_) | Type::UnknownInt(_) | Type::UnknownFloat(_) | Type::UnknownWord(_) | Type::Wildcard)
    }

    pub fn is_subtype(&self, supertype: &Type, generic_map: &HashMap<String, Type>) -> bool {
        use Type::*;

        match (self, supertype) {
            (Nil, Nil)
            | (Unassigned, _)
            | (Unknown(_), _)
            | (ISize, ISize)
            | (USize, USize)
            | (F32, F32)
            | (F64, F64)
            | (UnassignedInt, I(_))
            | (UnknownInt(_), I(_))
            | (UnassignedWord, U(_))
            | (UnknownWord(_), U(_))
            | (UnassignedFloat, F32)
            | (UnassignedFloat, F64)
            | (UnknownFloat(_), F32)
            | (UnknownFloat(_), F64) => true,

            (I(i), I(j))
            | (U(i), U(j)) => i == j,

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
