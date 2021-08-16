pub mod checker;

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

    ConstRef(Option<String>, Box<Type>),
    MutRef(Option<String>, Box<Type>),

    ConstPtr(Box<Type>),
    MutPtr(Box<Type>),

    Generic(String),
    TypeName(String, Vec<Type>, Vec<String>),
    Tuple(Vec<Type>),
    VarArgs(Option<Box<Type>>),
}
