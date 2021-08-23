use std::collections::HashMap;

use super::super::super::middleend::ir::Pattern;
use super::super::super::middleend::types::Type;

#[derive(Debug, Clone)]
pub enum Ast {
    Int(i64),
    Word(u64),
    Float(f64),
    Char(u8),
    True,
    False,
    String(String),
    Symbol(String),
    Infix(String, Box<Ast>, Box<Ast>),
    Tuple(Vec<Ast>),
    StructInit(String, HashMap<String, Ast>),
    Application(Box<Ast>, Type, Vec<Ast>),
    Attribute(Vec<Ast>),
    Prefix(String, Box<Ast>),
    TypeCast(Type, Box<Ast>),
    Pattern(Pattern),
    Let(Box<Ast>, Box<Ast>),
    Block(Vec<Ast>, bool),
    Box(Vec<(String, Vec<Ast>)>),
    FuncDef(String, Vec<(String, Type)>, Type, Box<Ast>),
    Struct(String, Vec<(String, Type)>),
    Enum(String, Type, Vec<(String, Option<u64>)>),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Error {
    NumberTooBig,
}

pub fn unescape_sequences(s: &str) -> String {
    let mut iter = s.chars();
    let mut s = String::new();

    while let Some(c) = iter.next() {
        if c == '\\' {
            match iter.next().unwrap() {
                '\\' => s.push('\\'),
                '\"' => s.push('\"'),
                '\'' => s.push('\''),
                'n' => s.push('\n'),
                'r' => s.push('\r'),
                't' => s.push('\t'),
                '0' => s.push('\0'),
                c => {
                    s.push('\\');
                    s.push(c)
                }
            };
        } else {
            s.push(c);
        }
    }

    s
}

pub fn strip_backslashes(s: &str) -> String {
    let mut result = String::with_capacity(s.len());

    for line in s.split('\n') {
        if let Some(stripped) = line.trim_start().strip_prefix("\\\\") {
            result.push_str(stripped);
            result.push('\n');
        }
    }

    result
}
