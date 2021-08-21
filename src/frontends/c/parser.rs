use std::collections::HashMap;

use pom::parser::*;

use super::super::super::middleend::ir::Pattern;
use super::super::super::middleend::types::Type;

type Parser<'a> = pom::parser::Parser<'a, u8, Ast>;

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
    StructInit(String, Vec<Ast>),
    Application(Box<Ast>, Vec<Ast>),
    Attribute(Vec<Ast>),
    Prefix(String, Box<Ast>),
    TypeCast(Type, Box<Ast>),
    Pattern(Pattern),
    Let(Box<Ast>, Box<Ast>),
    Block(Vec<Ast>, bool),
    Box(Vec<(String, Vec<Ast>)>),
    FuncDef(String, Vec<(String, Type)>, Type, Box<Ast>),
    Struct(String, HashMap<String, Type>),
    Enum(String, Type, Vec<(String, Option<u64>)>),
}

fn space<'a>(necessary: bool) -> pom::parser::Parser<'a, u8, ()> {
    one_of(b" \t\r\n")
        .repeat(if necessary { 1 } else { 0 }..)
        .discard()
}

fn int<'a>() -> Parser<'a> {
    let dec = one_of(b"0123456789")
        .repeat(1..)
        .convert(String::from_utf8)
        .convert(|v| v.parse())
        .map(Ast::Int);
    let hex = seq(b"0x")
        * one_of(b"0123456789abcdefABCDEF")
            .repeat(1..)
            .convert(String::from_utf8)
            .convert(|v| i64::from_str_radix(&v, 16))
            .map(Ast::Int);
    let bin = seq(b"0b")
        * one_of(b"01")
            .repeat(1..)
            .convert(String::from_utf8)
            .convert(|v| i64::from_str_radix(&v, 2))
            .map(Ast::Int);
    let oct = seq(b"0o")
        * one_of(b"01234567")
            .repeat(1..)
            .convert(String::from_utf8)
            .convert(|v| i64::from_str_radix(&v, 8))
            .map(Ast::Int);

    (sym(b'-').opt() + (hex | bin | oct | dec)).map(|v| match v {
        (Some(_), Ast::Int(i)) => Ast::Int(-i),
        (_, v) => v,
    }) - space(false)
}

fn word<'a>() -> Parser<'a> {
    let dec = (one_of(b"0123456789") - sym(b'u'))
        .repeat(1..)
        .convert(String::from_utf8)
        .convert(|v| v.parse())
        .map(Ast::Int);
    let hex = seq(b"0xu")
        * one_of(b"0123456789abcdefABCDEF")
            .repeat(1..)
            .convert(String::from_utf8)
            .convert(|v| i64::from_str_radix(&v, 16))
            .map(Ast::Int);
    let bin = seq(b"0bu")
        * one_of(b"01")
            .repeat(1..)
            .convert(String::from_utf8)
            .convert(|v| i64::from_str_radix(&v, 2))
            .map(Ast::Int);
    let oct = seq(b"0ou")
        * one_of(b"01234567")
            .repeat(1..)
            .convert(String::from_utf8)
            .convert(|v| i64::from_str_radix(&v, 8))
            .map(Ast::Int);
    hex | bin | oct | dec
}

fn float<'a>() -> Parser<'a> {
    let whole = one_of(b"0123456789").repeat(1..);
    let frac = sym(b'.').map(Some) + one_of(b"0123456789").repeat(1..);
    let exponent = one_of(b"eE") * (one_of(b"+-").opt() + one_of(b"0123456789").repeat(1..));
    (sym(b'-').opt() + whole + (frac | exponent))
        .collect()
        .convert(std::str::from_utf8)
        .convert(|v| v.parse())
        .map(Ast::Float)
        - space(false)
}

fn character<'a>() -> Parser<'a> {
    sym(b'\'')
        * (none_of(b"\\'")
            | (sym(b'\\')
                * one_of(b"nrt\\\'\"").map(|v| match v {
                    b'n' => b'\n',
                    b'r' => b'\r',
                    b't' => b'\t',
                    b'\\' => b'\\',
                    b'\'' => b'\'',
                    b'\"' => b'\"',
                    _ => unreachable!(),
                })))
        .map(Ast::Char)
        - sym(b'\'')
        - space(false)
}

fn boolean<'a>() -> Parser<'a> {
    (seq(b"true") | seq(b"false")).map(|v| match v {
        b"true" => Ast::True,
        b"false" => Ast::False,
        _ => unreachable!(),
    }) - !symbol()
        - space(false)
}

fn string<'a>() -> Parser<'a> {
    let quotes = sym(b'\"')
        * (none_of(b"\\\"")
            | (sym(b'\\')
                * one_of(b"nrt\\\'\"").map(|v| match v {
                    b'n' => b'\n',
                    b'r' => b'\r',
                    b't' => b'\t',
                    b'\\' => b'\\',
                    b'\'' => b'\'',
                    b'\"' => b'\"',
                    _ => unreachable!(),
                })))
        .repeat(..)
        .convert(String::from_utf8)
        .map(Ast::String)
        - sym(b'\"');

    let backslashes = (seq(b"\\\\") * none_of(b"\n\r").repeat(..) + one_of(b"\n\r").repeat(..))
        .collect()
        .repeat(1..)
        .map(|v| v.into_iter().flatten().cloned().collect())
        .convert(String::from_utf8)
        .map(Ast::String);

    (quotes | backslashes) - space(false)
}

fn symbol_helper<'a>(first: &'static [u8], second: &'static [u8]) -> Parser<'a> {
    (none_of(first) + none_of(second).repeat(..))
        .collect()
        .map(Vec::from)
        .convert(String::from_utf8)
        .map(Ast::Symbol)
        - space(false)
}

fn symbol<'a>() -> Parser<'a> {
    symbol_helper(
        b"`1234567890-=[]\\;',./~!@#$%^&*()+{}|:\"<>? \t\r\n",
        b"`!@#$%^&*()-+=[]\\;',./{}|:\"<>? \t\r\n",
    )
}

fn struct_init<'a>() -> Parser<'a> {
    (symbol() - space(false) - sym(b'@') - space(false) - sym(b'{') - space(false)
        + list(call(expr), sym(b',') + space(false))
        - space(false)
        - sym(b'}')
        - space(false))
    .map(|v| {
        Ast::StructInit(
            if let Ast::Symbol(v) = v.0 {
                v
            } else {
                unreachable!();
            },
            v.1,
        )
    })
}

fn application<'a>() -> Parser<'a> {
    (symbol() - space(false) - sym(b'@') - space(false) - sym(b'(') - space(false)
        + list(call(expr), sym(b',') + space(false))
        - space(false)
        - sym(b')')
        - space(false))
    .map(|v| Ast::Application(Box::new(v.0), v.1))
}

fn attribute<'a>() -> Parser<'a> {
    list(application() | symbol(), sym(b'.') + space(false)).convert(|mut v| {
        if v.is_empty() {
            Err(())
        } else if v.len() == 1 {
            Ok(v.remove(0))
        } else {
            Ok(Ast::Attribute(v))
        }
    })
}

fn tuple<'a>() -> Parser<'a> {
    (sym(b'(') * list(call(expr), sym(b',') + space(false)) - space(false) + sym(b',').opt()
        - space(false)
        - sym(b')')
        - space(false)
        - space(false))
    .map(|mut v| {
        if v.0.len() != 1 || v.1.is_some() {
            Ast::Tuple(v.0)
        } else {
            v.0.remove(0)
        }
    })
}

fn value<'a>() -> Parser<'a> {
    float()
        | word()
        | int()
        | string()
        | boolean()
        | character()
        | struct_init()
        | attribute()
        | tuple()
}

fn prefix<'a>() -> Parser<'a> {
    (((one_of(b"-*!").map(|v| vec![v])
        | (sym(b'&') - space(false) + (seq(b"mut") - !symbol() - space(false)).opt())
            .collect()
            .map(Vec::from))
        - space(false))
    .convert(String::from_utf8)
    .opt()
        + value())
    .map(|v| {
        let (op, ast) = v;
        match op {
            Some(op) => Ast::Prefix(op, Box::new(ast)),
            None => ast,
        }
    })
}

fn infixl_op<'a>(child: fn() -> Parser<'a>, op: pom::parser::Parser<'a, u8, String>) -> Parser<'a> {
    (child() + (op - space(false) + child()).repeat(..)).map(|v| {
        let (mut left, rights) = v;
        for (op, right) in rights {
            left = Ast::Infix(op, Box::new(left), Box::new(right));
        }
        left
    })
}

fn muldivmod<'a>() -> Parser<'a> {
    infixl_op(
        prefix,
        one_of(b"*/%").map(|v| vec![v]).convert(String::from_utf8),
    )
}

fn addsub<'a>() -> Parser<'a> {
    infixl_op(
        muldivmod,
        one_of(b"+-").map(|v| vec![v]).convert(String::from_utf8),
    )
}

fn bitshift<'a>() -> Parser<'a> {
    infixl_op(
        addsub,
        (seq(b"<<") | seq(b">>"))
            .map(Vec::from)
            .convert(String::from_utf8),
    )
}

fn bit_and<'a>() -> Parser<'a> {
    infixl_op(
        bitshift,
        sym(b'&').map(|v| vec![v]).convert(String::from_utf8),
    )
}

fn bit_or<'a>() -> Parser<'a> {
    infixl_op(
        bit_and,
        sym(b'|').map(|v| vec![v]).convert(String::from_utf8),
    )
}

fn bit_xor<'a>() -> Parser<'a> {
    infixl_op(
        bit_or,
        sym(b'^').map(|v| vec![v]).convert(String::from_utf8),
    )
}

fn bool_and<'a>() -> Parser<'a> {
    infixl_op(
        bit_xor,
        seq(b"&&").map(Vec::from).convert(String::from_utf8),
    )
}

fn bool_or<'a>() -> Parser<'a> {
    infixl_op(
        bool_and,
        seq(b"||").map(Vec::from).convert(String::from_utf8),
    )
}

fn bool_xor<'a>() -> Parser<'a> {
    infixl_op(
        bool_or,
        seq(b"^^").map(Vec::from).convert(String::from_utf8),
    )
}

fn assign<'a>() -> Parser<'a> {
    (bool_xor()
        + (((seq(b"<<") | seq(b">>")).map(Vec::from) | one_of(b"*/%+-&|^").map(|v| vec![v])).opt()
            + sym(b'=')
            - space(false)
            + bool_xor())
        .repeat(..))
    .convert::<_, std::string::FromUtf8Error, _>(|v| {
        let (first, ops) = v;

        let mut acc = None;
        let mut last_op = None;
        for ((op, eq), right) in ops.into_iter().rev() {
            let mut op = match op {
                Some(v) => v,
                None => vec![],
            };
            op.push(eq);

            if let Some(a) = acc {
                acc = Some(Ast::Infix(last_op.unwrap(), Box::new(right), Box::new(a)));
            } else {
                acc = Some(right);
            }

            last_op = Some(String::from_utf8(op)?);
        }

        match acc {
            Some(acc) => Ok(Ast::Infix(last_op.unwrap(), Box::new(first), Box::new(acc))),
            None => Ok(first),
        }
    })
}

fn type_<'a>() -> pom::parser::Parser<'a, u8, Type> {
    (sym(b'_').map(|_| Type::Wildcard)
        | seq(b"i8").map(|_| Type::I8)
        | seq(b"i16").map(|_| Type::I16)
        | seq(b"i32").map(|_| Type::I32)
        | seq(b"i64").map(|_| Type::I64)
        | seq(b"isize").map(|_| Type::ISize)
        | seq(b"u8").map(|_| Type::U8)
        | seq(b"u16").map(|_| Type::U16)
        | seq(b"u32").map(|_| Type::U32)
        | seq(b"u64").map(|_| Type::U64)
        | seq(b"usize").map(|_| Type::USize)
        | seq(b"f32").map(|_| Type::F32)
        | seq(b"f64").map(|_| Type::F64)
        | seq(b"bool").map(|_| Type::Bool)
        | seq(b"nil").map(|_| Type::Nil)
        | (sym(b'(') + space(false) + sym(b')')).map(|_| Type::Nil)
        | (symbol() - space(false)
            + (sym(b'[') * space(false) * list(call(type_), sym(b',') + space(false))
                - space(false)
                - sym(b']'))
            .opt())
        .map(|v| {
            let (name, params) = v;
            let name = if let Ast::Symbol(name) = name {
                name
            } else {
                unreachable!();
            };
            Type::TypeName(name, params.into_iter().flatten().collect())
        })
        | (sym(b'(') * space(false) * list(call(type_), sym(b',') + space(false))
            - space(false)
            - sym(b')'))
        .map(Type::Tuple)
        | (sym(b'\'') * symbol()).map(|v| {
            if let Ast::Symbol(v) = v {
                Type::Generic(v)
            } else {
                unreachable!()
            }
        })
        | (sym(b'&') * space(false) * (seq(b"mut") - !symbol() - space(false)).opt() + call(type_))
            .map(|v| {
                if v.0.is_some() {
                    Type::MutRef(Box::new(v.1))
                } else {
                    Type::ConstRef(Box::new(v.1))
                }
            })
        | (sym(b'*') * space(false) * (seq(b"mut") - !symbol() - space(false)).opt() + call(type_))
            .map(|v| {
                if v.0.is_some() {
                    Type::MutPtr(Box::new(v.1))
                } else {
                    Type::ConstPtr(Box::new(v.1))
                }
            })
        | (seq(b"...") * space(false) * call(type_).opt()).map(|v| Type::VarArgs(v.map(Box::new))))
        - space(false)
}

fn type_cast<'a>() -> Parser<'a> {
    (sym(b'[') * space(false) * type_() - sym(b']') - space(false) + call(expr))
        .map(|v| Ast::TypeCast(v.0, Box::new(v.1)))
}

/*
   SRange(i64, i64),
   URange(u64, u64),
   FRange(f64, f64),
* */
fn pattern_raw<'a>() -> Parser<'a> {
    (sym(b'_').map(|_| Ast::Pattern(Pattern::Wildcard))
        | (seq(b"mut")
            * !symbol()
            * space(false)
            * symbol().map(|v| {
                if let Ast::Symbol(v) = v {
                    Ast::Pattern(Pattern::MutName(v))
                } else {
                    unreachable!()
                }
            }))
        | (seq(b"enum")
            * space(true)
            * symbol().map(|v| {
                if let Ast::Symbol(v) = v {
                    Ast::Pattern(Pattern::Enum(v))
                } else {
                    unreachable!()
                }
            }))
        | (sym(b'(') * space(false) * list(call(pattern_or), sym(b',') + space(false))
            + sym(b',').opt()
            - space(false)
            - sym(b')'))
        .map(|v| {
            let (mut pats, final_comma) = v;
            if (pats.len() == 1 && final_comma.is_some()) || pats.len() > 1 {
                Ast::Pattern(Pattern::Tuple(
                    pats.into_iter()
                        .map(|v| {
                            if let Ast::Pattern(p) = v {
                                p
                            } else {
                                unreachable!();
                            }
                        })
                        .collect(),
                ))
            } else {
                pats.remove(0)
            }
        })
        | (symbol() - sym(b'{') - space(false)
            + list(
                symbol() - sym(b':') - space(false) + call(pattern_or),
                sym(b',') + space(false),
            )
            - sym(b'}'))
        .map(|v| {
            let (name, fields) = v;
            Ast::Pattern(Pattern::Struct(
                if let Ast::Symbol(name) = name {
                    name
                } else {
                    unreachable!()
                },
                fields
                    .into_iter()
                    .map(|v| {
                        if let (Ast::Symbol(field), Ast::Pattern(pat)) = v {
                            (field, pat)
                        } else {
                            unreachable!();
                        }
                    })
                    .collect(),
            ))
        })
        | symbol().map(|v| {
            if let Ast::Symbol(v) = v {
                Ast::Pattern(Pattern::Name(v))
            } else {
                unreachable!();
            }
        })
        | (int() + (sym(b'-') * space(false) * int()).opt()).map(|v| match v {
            (Ast::Int(start), Some(Ast::Int(end))) => Ast::Pattern(Pattern::SRange(start, end)),
            (Ast::Int(start), None) => Ast::Pattern(Pattern::SRange(start, start)),
            _ => unreachable!(),
        })
        | (word() + (sym(b'-') * space(false) * word()).opt()).map(|v| match v {
            (Ast::Word(start), Some(Ast::Word(end))) => Ast::Pattern(Pattern::URange(start, end)),
            (Ast::Word(start), None) => Ast::Pattern(Pattern::URange(start, start)),
            _ => unreachable!(),
        })
        | (float() + (sym(b'-') * space(false) * float()).opt()).map(|v| match v {
            (Ast::Float(start), Some(Ast::Float(end))) => Ast::Pattern(Pattern::FRange(start, end)),
            (Ast::Float(start), None) => Ast::Pattern(Pattern::FRange(start, start)),
            _ => unreachable!(),
        }))
        - space(false)
}

fn pattern_or<'a>() -> Parser<'a> {
    (pattern_raw() - space(false) + (sym(b'|') * space(false) * pattern_raw()).repeat(..)).map(
        |v| {
            let (head, mut tail) = v;

            if tail.is_empty() {
                head
            } else {
                tail.insert(0, head);
                Ast::Pattern(Pattern::Or(
                    tail.into_iter()
                        .map(|v| {
                            if let Ast::Pattern(p) = v {
                                p
                            } else {
                                unreachable!();
                            }
                        })
                        .collect(),
                ))
            }
        },
    )
}

fn pattern<'a>() -> Parser<'a> {
    sym(b':') * space(false) * pattern_or()
}

fn expr<'a>() -> Parser<'a> {
    box_() | block() | pattern() | type_cast() | assign()
}

fn let_<'a>() -> Parser<'a> {
    (seq(b"let") * space(true) * pattern_raw() - sym(b'=') - space(false) + expr())
        .map(|v| Ast::Let(Box::new(v.0), Box::new(v.1)))
}

fn statement<'a>() -> Parser<'a> {
    let_() | expr()
}

fn block<'a>() -> Parser<'a> {
    (sym(b'{') * space(false) * list(call(statement), sym(b';') + space(false)) + sym(b';').opt()
        - space(false)
        - sym(b'}')
        - space(false))
    .map(|v| Ast::Block(v.0, v.1.is_none()))
}

fn box_<'a>() -> Parser<'a> {
    (symbol().map(|v| {
        if let Ast::Symbol(v) = v {
            v
        } else {
            unreachable!();
        }
    }) + (block()
        | (sym(b'(') * space(false) * call(expr) - space(false) - sym(b')') - space(false)))
    .repeat(1..))
    .repeat(1..)
    .map(Ast::Box)
        | (symbol() + call(expr)).map(|v| {
            Ast::Box(vec![(
                if let Ast::Symbol(v) = v.0 {
                    v
                } else {
                    unreachable!();
                },
                vec![v.1],
            )])
        })
}

fn func_def<'a>() -> Parser<'a> {
    (seq(b"fn") * space(true) * symbol() - sym(b'(') - space(false)
        + list(
            symbol() - sym(b':') - space(false) + type_(),
            sym(b',') + space(false),
        )
        - sym(b')')
        - space(false)
        + expr())
    .map(|v| {
        if let ((Ast::Symbol(name), args), body) = v {
            let (ret_type, body) = match body {
                Ast::TypeCast(ret_type, body) => (ret_type, body),
                _ => (Type::Nil, Box::new(body)),
            };
            Ast::FuncDef(
                name,
                args.into_iter()
                    .map(|v| {
                        (
                            if let Ast::Symbol(v) = v.0 {
                                v
                            } else {
                                unreachable!();
                            },
                            v.1,
                        )
                    })
                    .collect(),
                ret_type,
                body,
            )
        } else {
            unreachable!();
        }
    })
}

fn struct_def<'a>() -> Parser<'a> {
    (seq(b"struct") * space(true) * symbol() - sym(b'{') - space(false)
        + list(
            symbol() - sym(b':') - space(false) + type_(),
            sym(b',') + space(false),
        )
        - sym(b'}')
        - space(false))
    .map(|v| {
        if let Ast::Symbol(name) = v.0 {
            Ast::Struct(
                name,
                v.1.into_iter()
                    .map(|v| {
                        if let Ast::Symbol(name) = v.0 {
                            (name, v.1)
                        } else {
                            unreachable!();
                        }
                    })
                    .collect(),
            )
        } else {
            unreachable!();
        }
    })
}

fn enum_def<'a>() -> Parser<'a> {
    (seq(b"enum") * space(true) * symbol() + (sym(b':') * space(false) * type_()).opt()
        - sym(b'{')
        - space(false)
        + list(
            symbol() + (sym(b'=') * space(false) * (word() | int())).opt(),
            sym(b',') + space(false),
        )
        - sym(b'}')
        - space(false))
    .map(|v| {
        if let Ast::Symbol(name) = v.0 .0 {
            Ast::Enum(
                name,
                v.0 .1.unwrap_or(Type::I32),
                v.1.into_iter()
                    .map(|v| match v {
                        (Ast::Symbol(variant), Some(Ast::Int(i))) => (variant, Some(i as u64)),
                        (Ast::Symbol(variant), Some(Ast::Word(w))) => (variant, Some(w)),
                        (Ast::Symbol(variant), None) => (variant, None),
                        _ => unreachable!(),
                    })
                    .collect(),
            )
        } else {
            unreachable!();
        }
    })
}

fn top<'a>() -> Parser<'a> {
    func_def() | struct_def() | enum_def() | (box_() - sym(b';') - space(false))
}

fn full<'a>() -> pom::parser::Parser<'a, u8, Vec<Ast>> {
    space(false) * top().repeat(..) - end()
}

pub fn parse(s: &str) -> pom::Result<Vec<Ast>> {
    full().parse(s.as_bytes())
}
