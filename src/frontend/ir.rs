use logos::Span;
use std::collections::HashMap;

use super::types::{Type, TypeParseError};
use super::parser::Ast;

#[derive(Clone, Debug)]
pub enum NativeFunction {
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
    And,
    Or,
    Xor
}

#[derive(Clone, Debug)]
pub struct Pattern {
    pub name: Type,
    pub args: Vec<String>
}

#[derive(Clone, Debug)]
pub struct SExprMetadata {
    pub span: Span,
    pub type_: Type,
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

    // Lifetime
    Lifetime(SExprMetadata, String),

    // Native functions
    Native(SExprMetadata, NativeFunction, Vec<SExpr>),

    // Nil
    Nil(SExprMetadata),

    // Function application
    Application(SExprMetadata, Box<SExpr>, Vec<SExpr>),

    // Control flow keywords
    Continue(SExprMetadata),
    Break(SExprMetadata),
    Return(SExprMetadata, Option<Box<SExpr>>),
    Throw(SExprMetadata, Option<Box<SExpr>>),

    // If expressions
    If(SExprMetadata, Box<SExpr>, Box<SExpr>, Option<Box<SExpr>>),

    // If let expressions
    IfLet(SExprMetadata, Box<SExpr>, Box<SExpr>, Vec<(Pattern, SExpr)>),

    // Cond expression
    Cond(SExprMetadata, Vec<(SExpr, SExpr)>, Option<Box<SExpr>>),

    // Loop expression
    Loop(SExprMetadata, Box<SExpr>),

    // Functions
    //       metadata       name    generics     lifetimes    arguments            return type  body
    Function(SExprMetadata, String, Vec<String>, Vec<String>, Vec<(String, Type)>, Type,        Box<SExpr>),

    // Structs
    //     metadata       name    generics     lifetimes    fields
    Struct(SExprMetadata, String, Vec<String>, Vec<String>, Vec<(String, Type)>),

    // Enums
    Enum(SExprMetadata, String, Vec<(String, Option<isize>)>),

    // Tuples
    Tuple(SExprMetadata, Vec<SExpr>),

    // Type definitions
    TypeDefinition(SExprMetadata, String, Type),

    // Sequence of expressions executed in order
    Seq(SExprMetadata, Vec<SExpr>),

    // Namespace definition
    Namespace(SExprMetadata, Vec<String>),

    // Import as
    // (use (an example module) as uwu)
    UseAs(SExprMetadata, Vec<String>, Option<String>),

    // Import members
    // (use (an example module) (MyExampleThing (MyOtherExampleThing MOET)))
    UseMembers(SExprMetadata, Vec<String>, Vec<(String, String)>),
}

pub struct IrModule {
    pub root: SExpr,
    pub types: HashMap<String, SExpr>,
    pub funcs: HashMap<String, SExpr>,
}

pub struct Ir {
    pub modules: HashMap<Vec<String>, IrModule>,
}

pub enum IrParseError {
    TypeParseError(TypeParseError)
}

pub fn parse_ir(ast: Ast) -> Result<Ir, IrParseError> {
    Err(IrParseError::TypeParseError(TypeParseError::NotAType))
}
