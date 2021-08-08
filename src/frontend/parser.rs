use logos::{Lexer, Logos, Span};

// convert_chars(&str) -> String
// Converts escaped characters into an unescaped string.
fn convert_chars(s: &str, off: usize) -> String {
    let mut iter = s[off..s.len() - off].chars();
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

// The tokens parsed by the lexer.
#[derive(Logos, PartialEq, Debug, Clone)]
pub enum Token {
    // Brackets
    #[token("(")]
    LParen,

    #[token(")")]
    RParen,

    #[token("[")]
    LBrack,

    #[token("]")]
    RBrack,

    #[token("{")]
    LBrace,

    #[token("}")]
    RBrace,

    // Whitespace
    #[regex(r"([ \t\f\r\n])+", logos::skip)]
    Whitespace,

    #[regex(r"//[^\n]*", logos::skip)]
    #[regex(r"/\*([^\*]\*+)+/", logos::skip)]
    Comment,

    // Error
    #[error]
    Error,

    // Numbers
    #[regex(r"[0-9]+", |lex| lex.slice().parse())]
    #[regex(r"0x[0-9a-fA-F]+", |lex| i64::from_str_radix(&lex.slice()[2..], 16))]
    #[regex(r"0b[01]+", |lex| i64::from_str_radix(&lex.slice()[2..], 2))]
    Int(i64),

    #[regex(r"[0-9]+(\.[0-9]*([eE][+-]?[0-9]+)?|[eE][+-]?[0-9]+)", |lex| lex.slice().parse())]
    Float(f64),

    #[regex(r"[0-9]+u", |lex| {
        let v = lex.slice();
        v[..v.len() - 1].parse()
    })]
    #[regex(r"[a-fA-F0-9]+h", |lex| {
        let v = lex.slice();
        u64::from_str_radix(&v[..v.len() - 1], 16)
    })]
    #[regex(r"[01]+b", |lex| {
        let v = lex.slice();
        u64::from_str_radix(&v[..v.len() - 1], 2)
    })]
    Word(u64),

    #[regex(r#"'([^\\']|\\[nrt'"0])'"#, |lex| convert_chars(lex.slice(), 1).bytes().next().unwrap())]
    Char(u8),

    // Symbols (variables and stuff)
    #[regex(r"[a-zA-Z_!@#$%^&*|\-=+;:,<.>/?~`][a-zA-Z0-9_!@#$%^&*|\-=+;:,<.>/?~`']*")]
    Symbol,

    #[regex(r"'[a-zA-Z_]+")]
    Lifetime,

    // Strings
    #[regex(r#""([^\\"]|\\.)*""#, |lex| convert_chars(lex.slice(), 1))]
    #[regex(r##"#"([^"]|"[^#])*"#"##, |lex| convert_chars(lex.slice(), 2))]
    String(String),

    // Booleans
    #[token("true")]
    True,

    #[token("false")]
    False,
}

// Represents a parser.
struct Parser<'a> {
    // The lexer the parser uses internally.
    lexer: Lexer<'a, Token>,

    // The tokens already parsed
    tokens: Vec<(Token, Span)>,

    // The current position of the parser.
    token_pos: usize,
}

impl<'a> Parser<'a> {
    // new(&str) -> Parser
    // Creates a new parser
    fn new(s: &str) -> Parser {
        Parser {
            lexer: Token::lexer(s),
            tokens: vec![],
            token_pos: 0,
        }
    }

    // next(&mut self) -> Option<&(Token, Span)>
    // Gets the next token.
    fn next(&mut self) -> Option<&(Token, Span)> {
        // Get token from list of already parsed tokens if it exists
        if self.token_pos < self.tokens.len() {
            let token = &self.tokens[self.token_pos];
            self.token_pos += 1;
            Some(token)

        // Otherwise get token from the lexer
        } else {
            self.tokens.push((self.lexer.next()?, self.lexer.span()));
            self.token_pos += 1;
            self.tokens.last()
        }
    }

    // peek(&mut self) -> Option<&(Token, Span)>
    // Peeks at the next token.
    fn peek(&mut self) -> Option<(&Token, Span)> {
        // Get token from list of already parsed tokens if it exists
        if self.token_pos < self.tokens.len() {
            let token = &self.tokens[self.token_pos];
            Some((&token.0, token.1.clone()))

        // Otherwise get token from lexer
        } else {
            self.tokens.push((self.lexer.next()?, self.lexer.span()));
            let token = self.tokens.last()?;
            Some((&token.0, token.1.clone()))
        }
    }

    // slice(&self) -> String
    // Returns the slice corresponding to the current token.
    fn slice(&mut self) -> String {
        if self.token_pos >= self.tokens.len() {
            self.peek();
        }

        if self.token_pos < self.tokens.len() {
            let range = &self.tokens[self.token_pos].1;
            String::from(&self.lexer.source()[range.start..range.end])
        } else {
            String::with_capacity(0)
        }
    }

    // span(&self) -> Span
    // Returns the current span.
    fn span(&mut self) -> Span {
        if let Some((_, s)) = self.peek() {
            s
        } else {
            self.lexer.span()
        }
    }

    // save_state(&self) -> usize
    // Saves the current token position by returning it.
    fn save_state(&self) -> usize {
        self.token_pos
    }

    // return_state(&mut self, usize) -> ()
    // Returns to a given state.
    fn return_state(&mut self, state: usize) {
        self.token_pos = state;
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Ast {
    // Numbers
    Int(Span, i64),
    Float(Span, f64),
    Word(Span, u64),
    Char(Span, u8),

    // Booleans
    True(Span),
    False(Span),

    // String
    String(Span, String),

    // Symbol (variables and stuff)
    Symbol(Span, String),

    // Lifetime
    Lifetime(Span, String),

    // S expressions
    SExpr(Span, Vec<Ast>),

    // Programs
    Program(Span, Vec<Ast>),
}

impl Ast {
    pub fn get_span(&self) -> Span {
        match self {
            Self::Int(s, _)
            | Self::Float(s, _)
            | Self::Word(s, _)
            | Self::Char(s, _)
            | Self::True(s)
            | Self::False(s)
            | Self::String(s, _)
            | Self::Lifetime(s, _)
            | Self::Symbol(s, _)
            | Self::SExpr(s, _)
            | Self::Program(s, _) => s.clone(),
        }
    }
}

#[derive(Debug)]
pub struct ParseError {
    pub span: Span,
    pub msg: String,
    fatal: bool,
}

// call_func(ident, ident, ident) -> Result<Ast, ParseError>
// Calls a function and returns if an error was encountered.
macro_rules! call_func {
    ($func: ident, $parser: ident, $state: ident) => {
        match $func($parser) {
            Ok(v) => v,
            Err(e) => {
                $parser.return_state($state);
                return Err(e);
            }
        }
    };
}

// call_optional(ident, ident) => Result<Ast, ParseError>
// Calls a function and only returns if a fatal error is encountered.
macro_rules! call_optional {
    ($func: ident, $parser: ident) => {
        match $func($parser) {
            Ok(v) => Ok(v),
            Err(e) if e.fatal => return Err(e),
            Err(e) => Err(e),
        }
    };
}

// consume_save(ident, ident, ident, literal, literal, literal, expr*) -> Result<Ast, ParseError>
// Consumes a token and saves it, returning if an error was encountered.
macro_rules! consume_save
{
    ($parser: ident, $token: ident, $state: ident, $fatal: literal, $format: literal $(,$vs: expr),*) => {
        match $parser.peek()
        {
            Some((Token::$token, s)) => {
                let v = ($parser.slice(), s);
                $parser.next();
                v
            }

            _ => {
                let span = $parser.span();
                $parser.return_state($state);
                return Err(ParseError {
                    span,
                    msg: format!($format $(,$vs),*),
                    fatal: $fatal
                })
            }
        };
    }
}

// value(&mut Parser) -> Result<Ast, ParseError>
// Gets the next value.
fn value(parser: &mut Parser) -> Result<Ast, ParseError> {
    // Parse symbols/accessing members
    let state = parser.save_state();

    // Get token
    let slice = parser.slice();
    let (token, span) = match parser.peek() {
        Some(v) => v,
        None => return Err(ParseError {
            span: parser.span(),
            msg: String::from("expected value"),
            fatal: false,
        }),
    };

    // Check for int
    if let Token::Int(n) = token {
        let n = *n;
        parser.next();
        Ok(Ast::Int(span, n))

    // Check for float
    } else if let Token::Float(n) = token {
        let n = *n;
        parser.next();
        Ok(Ast::Float(span, n))

    // Check for word
    } else if let Token::Word(n) = token {
        let n = *n;
        parser.next();
        Ok(Ast::Word(span, n))

    // Check for character
    } else if let Token::Char(c) = token {
        let c = *c;
        parser.next();
        Ok(Ast::Char(span, c))

    // Check for string
    } else if let Token::String(s) = token {
        let s = s.clone();
        parser.next();
        Ok(Ast::String(span, s))

    // True
    } else if let Token::True = token {
        parser.next();
        Ok(Ast::True(span))

    // False
    } else if let Token::False = token {
        parser.next();
        Ok(Ast::False(span))

    // Symbol
    } else if let Token::Symbol = token {
        let (token, span) = consume_save!(parser, Symbol, state, false, "");
        Ok(Ast::Symbol(span, token))

    // Lifetime
    } else if let Token::Lifetime = token {
        let (token, span) = consume_save!(parser, Lifetime, state, false, "");
        Ok(Ast::Lifetime(span, token))

    // S expressions
    } else if let Token::LParen = token {
        // Get value
        let state = parser.save_state();
        let (_, Span { start, end: _ }) = consume_save!(parser, LParen, state, false, "");

        let mut exprs = vec![];
        loop {
            if let Some((Token::RParen, _)) = parser.peek() {
                break;
            }

            let ast = call_func!(value, parser, state);
            exprs.push(ast);
        }
        let (_, Span { start: _, end }) = consume_save!(parser, RParen, state, true, "");

        Ok(Ast::SExpr(Span {
            start,
            end
        }, exprs))

    // Not a value
    } else {
        Err(ParseError {
            span,
            msg: format!("Invalid token {:?} (ie `{}`)", token, slice),
            fatal: true,
        })
    }
}

pub fn parse(s: &str) -> Result<Ast, ParseError> {
    let mut parser = Parser::new(s);
    let p = &mut parser;

    let mut asts = vec![];
    while let Ok(v) = call_optional!(value, p) {
        asts.push(v);
    }

    if parser.peek().is_none() {
        Ok(Ast::Program(Span {
            start: 0,
            end: s.len()
        }, asts))
    } else {
        Err(ParseError {
            span: parser.span(),
            msg: String::from("expected eof"),
            fatal: true,
        })
    }
}
