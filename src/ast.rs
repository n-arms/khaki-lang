use core::{fmt, hash};
use std::{collections::HashMap, ops::Range};

use chumsky::span::SimpleSpan;

use crate::ord_map::OrdMap;

#[derive(Copy, Clone)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

#[derive(Clone, Debug)]
pub struct Struct {
    pub name: String,
    pub generics: Vec<String>,
    pub fields: OrdMap<String, Type>,
    pub funcs: HashMap<String, Func>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct Func {
    pub name: String,
    pub args: Vec<(String, Type)>,
    pub result: Type,
    pub is_cor: bool,
    pub body: Expr,
}

impl Func {
    pub fn arg_types(&self) -> Vec<Type> {
        self.args.iter().map(|(_, typ)| typ.clone()).collect()
    }
}

pub fn cor_name(strukt: &str, func: &str) -> String {
    format!("{strukt}_{func}")
}

pub fn constructor_name(strukt: &str) -> String {
    strukt.to_owned()
}

#[derive(Clone)]
pub struct Type {
    pub kind: TypeKind,
    pub span: Span,
    pub children: Vec<Type>,
}

impl hash::Hash for Type {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.kind.hash(state);
        self.children.hash(state);
    }
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind && self.children == other.children
    }
}

impl fmt::Debug for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.kind {
            TypeKind::Func => {
                let mut tuple = f.debug_tuple("FuncType");
                for child in &self.children {
                    tuple.field(child);
                }
                tuple.finish()
            }
            TypeKind::Named(name) => {
                let mut tuple = f.debug_tuple(&name);
                for child in &self.children {
                    tuple.field(child);
                }
                tuple.finish()
            }
            TypeKind::Unif(u) => write!(f, "unif{u}"),
            TypeKind::Generic(name) => write!(f, "{name}"),
            TypeKind::Array(size) => write!(f, "[{:?} x {size}]", self.children[0]),
        }
    }
}

impl Eq for Type {}

pub struct IntType {
    width: usize,
    signed: bool,
}

const SIZE_WIDTH: usize = 32;

impl IntType {
    pub fn usize() -> Self {
        Self {
            width: SIZE_WIDTH,
            signed: false,
        }
    }
    pub fn isize() -> Self {
        Self {
            width: SIZE_WIDTH,
            signed: true,
        }
    }
    pub fn signed(width: usize) -> Self {
        Self {
            width,
            signed: true,
        }
    }
    pub fn unsigned(width: usize) -> Self {
        Self {
            width,
            signed: false,
        }
    }
    pub fn is_signed(&self) -> bool {
        self.signed
    }
    pub fn width(&self) -> usize {
        self.width
    }
    pub fn to_type(&self, span: Span) -> Type {
        Type::named(
            format!("{}{}", if self.signed { "I" } else { "U" }, self.width),
            vec![],
            span,
        )
    }
    pub fn from_type(typ: &Type) -> Option<Self> {
        let TypeKind::Named(name) = &typ.kind else {
            return None;
        };
        let first = name.chars().next()?;
        let signed = match first {
            'I' => true,
            'U' => false,
            _ => return None,
        };
        let width = name[1..].parse().ok()?;
        Some(Self { width, signed })
    }
}

impl Type {
    pub fn bool(span: Span) -> Type {
        Type {
            kind: TypeKind::Named("Bool".into()),
            span,
            children: Vec::new(),
        }
    }

    pub fn named(name: String, generics: Vec<Type>, span: Span) -> Self {
        Self {
            kind: TypeKind::Named(name),
            span,
            children: generics,
        }
    }

    pub fn generic(name: impl Into<String>, span: Span) -> Self {
        Self {
            kind: TypeKind::Generic(name.into()),
            span,
            children: Vec::new(),
        }
    }

    pub fn ptr(typ: Type, span: Span) -> Self {
        Self::named("Ptr".into(), vec![typ], span)
    }

    pub fn slice(typ: Type, span: Span) -> Self {
        Self::named("Slice".into(), vec![typ], span)
    }

    pub fn func(args: Vec<Type>, result: Type, span: Span) -> Type {
        let mut children = args;
        children.push(result);
        Type {
            kind: TypeKind::Func,
            span,
            children,
        }
    }

    pub fn base(kind: TypeKind, span: Span) -> Self {
        Self {
            kind,
            span,
            children: Vec::new(),
        }
    }

    pub fn unit(span: Span) -> Type {
        Type {
            kind: TypeKind::Named("Unit".into()),
            span,
            children: Vec::new(),
        }
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum TypeKind {
    Func,
    Named(String),
    Unif(usize),
    Generic(String),
    Array(usize),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Op {
    Builtin(String),
    Arith(Arith),
    Cmp(Cmp),
    Logic(Logic),
    Await,
    Yield,
    Ref,
    Deref,
    If,
    While,
    Constructor(String),
    // Slice[t], Int, t
    SliceIndex,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Arith {
    Add,
    Sub,
    Mul,
    Div,
    ShiftLeft,
    ShiftRight,
    BitAnd,
    BitOr,
    BitNot,
    BitXor,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Cmp {
    Lt,
    Le,
    Gt,
    Ge,
    Eq,
    Ne,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Logic {
    And,
    Or,
    Xor,
    Not,
}

#[derive(Clone, Debug)]
pub enum Expr {
    Var(String, Option<Type>, Span),
    /// all functions are of the form `struct[generics].func()`
    Func(String, String, Option<(Type, Vec<Type>)>, Span),
    Literal(Literal, Option<Type>),
    Op(Op, Vec<Expr>, Option<Type>, Span),
    Call(Box<Expr>, Vec<Expr>, Option<Type>, Span),
    Block(Vec<Stmt>, Option<Box<Expr>>, Span),
    Field(Box<Expr>, String, Option<(Type, usize)>, Span),
    MethodCall(Box<Expr>, String, Vec<Expr>, Option<Type>, Span),
    Array(usize, Option<Vec<Expr>>, Option<Type>, Span),
}

#[derive(Clone, Debug)]
pub enum Stmt {
    Let(String, Expr),
    Set(Expr, Expr),
    Expr(Expr),
}

impl Expr {
    pub fn get_type(&self) -> Type {
        match self {
            Expr::Var(_, typ, _)
            | Expr::Literal(_, typ)
            | Expr::Op(_, _, typ, _)
            | Expr::MethodCall(_, _, _, typ, _)
            | Expr::Call(_, _, typ, _) => typ.clone().unwrap(),
            Expr::Func(_, _, meta, _) => meta.as_ref().unwrap().0.clone(),
            Expr::Field(_, _, meta, _) => meta.clone().unwrap().0,
            Expr::Block(_, expr, span) => {
                if let Some(expr) = expr {
                    expr.get_type()
                } else {
                    return Type::unit(*span);
                }
            }
            Expr::Array(_, _, elem_type, span) => Type::slice(elem_type.clone().unwrap(), *span),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Literal {
    Bool(bool, Span),
    Number(String, Span),
    Unit(Span),
}

impl Literal {
    pub fn span(&self) -> Span {
        match self {
            Literal::Number(_, span) | Literal::Bool(_, span) | Literal::Unit(span) => *span,
        }
    }
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}..{}", self.start, self.end)
    }
}

impl From<SimpleSpan> for Span {
    fn from(value: SimpleSpan) -> Self {
        Self {
            start: value.start,
            end: value.end,
        }
    }
}

impl From<Range<usize>> for Span {
    fn from(value: Range<usize>) -> Self {
        Self {
            start: value.start,
            end: value.end,
        }
    }
}
