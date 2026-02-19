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
        }
    }
}

impl Eq for Type {}

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

    pub fn int(span: Span) -> Type {
        Type {
            kind: TypeKind::Named("Int".into()),
            span,
            children: Vec::new(),
        }
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

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum TypeKind {
    Func,
    Named(String),
    Unif(usize),
    Generic(String),
}

#[derive(Clone, Debug)]
pub enum Op {
    Builtin(String),
    Await,
    Yield,
}

#[derive(Clone, Debug)]
pub enum Expr {
    Var(String, Option<Type>, Span),
    /// all functions are of the form `struct[generics].func()`
    Func(String, String, Option<(Type, Vec<Type>)>, Span),
    Literal(Literal, Option<Type>),
    Op(Op, Vec<Expr>, Option<Type>, Span),
    Call(Box<Expr>, Vec<Expr>, Option<Type>, Span),
    Block(Vec<Stmt>, Box<Expr>),
}

#[derive(Clone, Debug)]
pub enum Stmt {
    Let(String, Expr),
}

impl Expr {
    pub fn get_type(&self) -> Type {
        match self {
            Expr::Var(_, typ, _)
            | Expr::Literal(_, typ)
            | Expr::Op(_, _, typ, _)
            | Expr::Call(_, _, typ, _) => typ.clone().unwrap(),
            Expr::Func(_, _, meta, _) => meta.as_ref().unwrap().0.clone(),
            Expr::Block(_, expr) => expr.get_type(),
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
