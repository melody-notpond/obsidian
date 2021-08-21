use std::collections::HashMap;

use logos::Span;

use super::types::Type;

#[derive(Clone, Debug)]
pub enum NativeOperation {
    Neg,
    Invert,

    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Bsl,
    Bsr,
    BitAnd,
    BitOr,
    BitXor,
    BoolAnd,
    BoolOr,
    BoolXor,
    Lt,
    Gt,
    Lte,
    Gte,
    Eq,
    Ne,
}

#[derive(Clone, Debug)]
pub struct SExprMetadata {
    pub span: Span,
    pub type_: Type,
}

impl SExprMetadata {
    pub fn new(span: Span) -> SExprMetadata {
        SExprMetadata {
            span,
            type_: Type::Unassigned,
        }
    }

    pub fn new_with_type(span: Span, type_: Type) -> SExprMetadata {
        SExprMetadata { span, type_ }
    }
}

impl Default for SExprMetadata {
    fn default() -> Self {
        Self::new(Span { start: 0, end: 0 })
    }
}

#[derive(Clone, Debug)]
pub enum Pattern {
    Wildcard,
    Name(String),
    MutName(String),
    Struct(String, HashMap<String, Pattern>),
    Tuple(Vec<Pattern>),
    SRange(i64, i64),
    URange(u64, u64),
    FRange(f64, f64),
    Or(Vec<Pattern>),
    Enum(String),
}

#[derive(Clone, Debug)]
pub enum SExpr {
    // Numbers
    Int(SExprMetadata, i64),
    Float(SExprMetadata, f64),
    Word(SExprMetadata, u64),
    Char(SExprMetadata, u8),
    Bool(SExprMetadata, bool),

    // String
    String(SExprMetadata, String),

    // Symbol (variables and stuff)
    Symbol(SExprMetadata, String),

    // Native operations
    Native(SExprMetadata, NativeOperation, Vec<SExpr>),

    // Nil
    Nil(SExprMetadata),

    // Reference
    Ref(SExprMetadata, Box<SExpr>),

    // Mutable reference
    MutRef(SExprMetadata, Box<SExpr>),

    // Dereference
    Deref(SExprMetadata, Box<SExpr>),

    // Function application
    Application(SExprMetadata, Box<SExpr>, Vec<SExpr>),

    // Struct initialisation
    StructInit(SExprMetadata, String, HashMap<String, SExpr>),

    // Control flow keywords
    Continue(SExprMetadata, Option<Box<SExpr>>),
    Break(SExprMetadata, Option<Box<SExpr>>),
    Return(SExprMetadata, Option<Box<SExpr>>),
    Throw(SExprMetadata, Box<SExpr>),
    Unreachable(SExprMetadata),

    // If expressions
    If(SExprMetadata, Box<SExpr>, Box<SExpr>, Option<Box<SExpr>>),

    // Try expressions
    Try(SExprMetadata, Box<SExpr>, Vec<(Pattern, SExpr)>),

    // Match expression
    Match(SExprMetadata, Vec<(Pattern, SExpr)>, Option<Box<SExpr>>),

    // Loop expression
    Loop(SExprMetadata, Option<(Pattern, Box<SExpr>)>, Box<SExpr>),

    // Functions
    //       metadata       name    arguments            return type  body
    Function(
        SExprMetadata,
        String,
        Vec<(String, Type)>,
        Type,
        Box<SExpr>,
    ),

    // Let expressions
    Let(SExprMetadata, Pattern, Type, Box<SExpr>),

    // Assignments
    Assign(
        SExprMetadata,
        Box<SExpr>,
        Option<NativeOperation>,
        Box<SExpr>,
    ),

    // Attributes
    Attribute(SExprMetadata, Vec<SExpr>),

    // Structs
    //     metadata       name    fields
    Struct(
        SExprMetadata,
        String,
        Vec<(String, Type)>,
    ),

    // Enums
    Enum(SExprMetadata, String, Type, Vec<(String, Option<u64>)>),

    // Tuples
    Tuple(SExprMetadata, Vec<SExpr>),

    // Type definitions
    TypeDefinition(SExprMetadata, String, Vec<String>, Vec<String>, Type),

    // Sequence of expressions executed in order
    Seq(SExprMetadata, Vec<SExpr>),
}

impl SExpr {
    pub fn get_metadata(&self) -> &SExprMetadata {
        match self {
            SExpr::Int(m, _)
            | SExpr::Float(m, _)
            | SExpr::Word(m, _)
            | SExpr::Char(m, _)
            | SExpr::Bool(m, _)
            | SExpr::String(m, _)
            | SExpr::Symbol(m, _)
            | SExpr::Native(m, _, _)
            | SExpr::Nil(m)
            | SExpr::Ref(m, _)
            | SExpr::MutRef(m, _)
            | SExpr::Deref(m, _)
            | SExpr::Application(m, _, _)
            | SExpr::StructInit(m, _, _)
            | SExpr::Continue(m, _)
            | SExpr::Break(m, _)
            | SExpr::Return(m, _)
            | SExpr::Throw(m, _)
            | SExpr::Unreachable(m)
            | SExpr::If(m, _, _, _)
            | SExpr::Try(m, _, _)
            | SExpr::Match(m, _, _)
            | SExpr::Loop(m, _, _)
            | SExpr::Function(m, _, _, _, _)
            | SExpr::Let(m, _, _, _)
            | SExpr::Assign(m, _, _, _)
            | SExpr::Attribute(m, _)
            | SExpr::Struct(m, _, _)
            | SExpr::Enum(m, _, _, _)
            | SExpr::Tuple(m, _)
            | SExpr::TypeDefinition(m, _, _, _, _)
            | SExpr::Seq(m, _) => m,
        }
    }

    pub fn get_mut_metadata(&mut self) -> &mut SExprMetadata {
        match self {
            SExpr::Int(m, _)
            | SExpr::Float(m, _)
            | SExpr::Word(m, _)
            | SExpr::Char(m, _)
            | SExpr::Bool(m, _)
            | SExpr::String(m, _)
            | SExpr::Symbol(m, _)
            | SExpr::Native(m, _, _)
            | SExpr::Nil(m)
            | SExpr::Ref(m, _)
            | SExpr::MutRef(m, _)
            | SExpr::Deref(m, _)
            | SExpr::Application(m, _, _)
            | SExpr::StructInit(m, _, _)
            | SExpr::Continue(m, _)
            | SExpr::Break(m, _)
            | SExpr::Return(m, _)
            | SExpr::Throw(m, _)
            | SExpr::Unreachable(m)
            | SExpr::If(m, _, _, _)
            | SExpr::Try(m, _, _)
            | SExpr::Match(m, _, _)
            | SExpr::Loop(m, _, _)
            | SExpr::Function(m, _, _, _, _)
            | SExpr::Let(m, _, _, _)
            | SExpr::Assign(m, _, _, _)
            | SExpr::Attribute(m, _)
            | SExpr::Struct(m, _, _)
            | SExpr::Enum(m, _, _, _)
            | SExpr::Tuple(m, _)
            | SExpr::TypeDefinition(m, _, _, _, _)
            | SExpr::Seq(m, _) => m,
        }
    }
}

//                         name         arg types       return type
pub type FuncMap = HashMap<String, Vec<(Vec<Type>, Vec<(Type, SExpr)>)>>;

#[derive(Debug)]
pub struct IrModule {
    pub sexprs: Vec<SExpr>,
    pub types: HashMap<String, SExpr>,
    pub funcs: FuncMap,
}
