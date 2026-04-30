use core::{fmt, hash};
use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
};

use crate::ord_map::OrdMap;

pub type ByteIndex = usize;
pub type FileId = usize;

#[derive(Copy, Clone)]
pub struct Span {
    pub start: ByteIndex,
    pub end: ByteIndex,
    pub file_id: FileId,
}

#[derive(Clone, Debug)]
pub struct Path {
    pub path: Vec<String>,
    pub span: Span,
}

impl Path {
    pub fn new(path: Vec<String>, span: Span) -> Self {
        Self { path, span }
    }
    pub fn with(&self, name: String, span: Span) -> Self {
        let mut path = self.path.clone();
        path.push(name);
        Self::new(path, span)
    }
}

impl PartialEq for Path {
    fn eq(&self, other: &Self) -> bool {
        self.path == other.path
    }
}

impl Eq for Path {}

impl Hash for Path {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.path.hash(state);
    }
}

#[derive(Clone, Debug)]
pub struct Module {
    pub structs: HashMap<String, Struct>,
    pub funcs: HashMap<String, Func>,
}

#[derive(Clone, Debug)]
pub struct Struct {
    pub name: String,
    pub generics: Vec<String>,
    pub fields: OrdMap<String, Type>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct Func {
    pub name: String,
    pub generics: Vec<String>,
    pub args: Vec<(String, Type)>,
    pub result: Type,
    pub is_cor: bool,
    pub body: Expr,
}

impl Func {
    pub fn arg_types(&self) -> Vec<Type> {
        self.args.iter().map(|(_, typ)| typ.clone()).collect()
    }

    pub fn result_type(&self, path: &Path) -> Type {
        if self.is_cor {
            let generics = self
                .generics
                .iter()
                .map(|generic| Type::generic(generic.clone(), self.result.span))
                .collect();
            Type::named(path.clone(), self.name.clone(), generics, self.result.span)
        } else {
            self.result.clone()
        }
    }
}

pub fn cor_name(func: &str) -> String {
    format!("Cor_{func}")
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

pub fn generic_name(name: &str, id: usize) -> String {
    format!("{name}_{id}")
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
            TypeKind::Func(generics) => {
                let mut name = String::from("FuncType");
                if !generics.is_empty() {
                    name += "[ ";
                    for generic in generics {
                        name += generic;
                        name += " ";
                    }
                    name += "]";
                }
                let mut tuple = f.debug_tuple(&name);
                for child in &self.children {
                    tuple.field(child);
                }
                tuple.finish()
            }
            TypeKind::Any(any) => {
                write!(f, "any[{any}] {:?}", self.children[0])
            }
            TypeKind::Named(_, name) => {
                let mut tuple = f.debug_tuple(&format!("{name:?}"));
                for child in &self.children {
                    tuple.field(child);
                }
                tuple.finish()
            }
            TypeKind::Primitive(prim) => write!(f, "{prim:?}"),
            TypeKind::Unif(u) => write!(f, "unif{u}"),
            TypeKind::Generic(name, unif) => {
                if *unif == 0 {
                    write!(f, "{name}")
                } else {
                    write!(f, "{name}#{unif}")
                }
            }
            TypeKind::Array(size) => write!(f, "[{:?} x {size}]", self.children[0]),
        }
    }
}

impl Eq for Type {}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
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

    pub fn from_type(typ: &Type) -> Option<IntType> {
        match &typ.kind {
            TypeKind::Primitive(Prim::Int(int_type)) => Some(*int_type),
            _ => None,
        }
    }
}

impl Type {
    pub fn bool(span: Span) -> Type {
        Type {
            kind: TypeKind::Primitive(Prim::Bool),
            span,
            children: Vec::new(),
        }
    }

    pub fn named(path: Path, name: String, generics: Vec<Type>, span: Span) -> Self {
        Self {
            kind: TypeKind::Named(path, name),
            span,
            children: generics,
        }
    }

    pub fn generic(name: impl Into<String>, span: Span) -> Self {
        Self::skolem(name, 0, span)
    }

    pub fn skolem(name: impl Into<String>, id: usize, span: Span) -> Self {
        Self {
            kind: TypeKind::Generic(name.into(), id),
            span,
            children: Vec::new(),
        }
    }

    pub fn ptr(typ: Type, span: Span) -> Self {
        Self {
            kind: TypeKind::Primitive(Prim::Ptr),
            children: vec![typ],
            span,
        }
    }

    pub fn slice(typ: Type, span: Span) -> Self {
        Self::named(Path::new(vec![], span), "Slice".into(), vec![typ], span)
    }

    pub fn func(generics: Vec<String>, args: Vec<Type>, result: Type, span: Span) -> Type {
        let mut children = args;
        children.push(result);
        Type {
            kind: TypeKind::Func(generics),
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
            kind: TypeKind::Primitive(Prim::Unit),
            span,
            children: Vec::new(),
        }
    }

    pub fn int(typ: IntType, span: Span) -> Self {
        Self {
            kind: TypeKind::Primitive(Prim::Int(typ)),
            span,
            children: Vec::new(),
        }
    }

    pub fn unifs(&self) -> HashSet<usize> {
        let mut unifs: HashSet<_> = self
            .children
            .iter()
            .flat_map(|child| child.unifs())
            .collect();
        if let TypeKind::Unif(unif) = &self.kind {
            unifs.insert(*unif);
        }
        unifs
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum TypeKind {
    Func(Vec<String>),
    Any(String),
    Named(Path, String),
    Primitive(Prim),
    Unif(usize),
    Generic(String, usize),
    Array(usize),
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum Prim {
    Int(IntType),
    Bool,
    Unit,
    Ptr,
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
    // (Slice[t], Int) -> t
    SliceIndex,
    Open(Option<(String, usize)>),
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
    /// all functions are of the form path.func(args)
    Func(Path, String, Option<(Type, Vec<Type>)>, Span),
    Literal(Literal, Option<Type>),
    Op(Op, Vec<Expr>, Option<Type>, Span),
    Call(Box<Expr>, Vec<Expr>, Option<Type>, Span),
    Block(Vec<Stmt>, Option<Box<Expr>>, Span),
    Field(Box<Expr>, String, Option<(Type, usize)>, Span),
    Array(usize, Option<Vec<Expr>>, Option<Type>, Span),
    Any(Box<Expr>, Option<AnyMeta>, Span),
}

#[derive(Clone, Debug)]
pub struct AnyMeta {
    pub result: Type,
    pub existential: Type,
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
            Expr::Any(_, meta, _) => meta.as_ref().unwrap().result.clone(),
        }
    }

    pub fn span(&self) -> Span {
        match self {
            Expr::Var(_, _, span)
            | Expr::Func(_, _, _, span)
            | Expr::Op(_, _, _, span)
            | Expr::Call(_, _, _, span)
            | Expr::Block(_, _, span)
            | Expr::Field(_, _, _, span)
            | Expr::Array(_, _, _, span)
            | Expr::Any(_, _, span) => *span,
            Expr::Literal(literal, _) => match literal {
                Literal::Bool(_, span) | Literal::Number(_, span) | Literal::Unit(span) => *span,
            },
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
        write!(f, "[file {}]:{}..{}", self.file_id, self.start, self.end)
    }
}

impl Span {
    pub fn new(file_id: FileId, start: ByteIndex, end: ByteIndex) -> Self {
        Self {
            start,
            end,
            file_id,
        }
    }
}
