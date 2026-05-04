use std::collections::HashMap;

use chumsky::container::Seq;

use crate::ast::{Expr, Func, Span, Stmt, Type, TypeKind};

#[derive(Clone, Default, Debug)]
pub struct Sub {
    pub generics: HashMap<String, Type>,
    pub unifs: HashMap<usize, Type>,
}

impl Sub {
    pub fn set_generic(&mut self, name: String, typ: Type) {
        self.generics.insert(name, typ);
    }

    pub fn get_unif(&mut self, unif: usize) -> Option<&Type> {
        self.unifs.get(&unif)
    }

    pub fn set_unif(&mut self, unif: usize, typ: Type) {
        self.unifs.insert(unif, typ);
    }

    fn unif(&self, unif: usize, span: Span) -> Type {
        if let Some(typ) = self.unifs.get(&unif) {
            typ.clone()
        } else {
            Type {
                kind: TypeKind::Unif(unif),
                span,
                children: Vec::new(),
            }
        }
    }

    fn generic(&self, generic: &str, id: usize, span: Span) -> Type {
        if let Some(typ) = self.generics.get(generic) {
            typ.clone()
        } else {
            Type {
                kind: TypeKind::Generic(generic.to_string(), id),
                span,
                children: Vec::new(),
            }
        }
    }

    pub fn typ(&self, typ: &mut Type) {
        if let TypeKind::Func(generics) = &typ.kind {
            let mut inner = self.clone();
            inner.generics.retain(|name, _| !generics.contains(name));
            for child in &mut typ.children {
                inner.typ(child);
            }
            return;
        } else if let TypeKind::Any(generic) = &typ.kind {
            let mut inner = self.clone();
            inner.generics.retain(|name, _| name != generic);
            for child in &mut typ.children {
                inner.typ(child);
            }
            return;
        }
        for child in &mut typ.children {
            self.typ(child);
        }
        match &mut typ.kind {
            TypeKind::Func(_) | TypeKind::Any(_) => {
                unreachable!()
            }
            TypeKind::Primitive(..) => {}
            TypeKind::Named(..) => {}
            TypeKind::Unif(unif) => *typ = self.unif(*unif, typ.span),
            TypeKind::Generic(name, id) => *typ = self.generic(name, *id, typ.span),
            TypeKind::Array(_) => {}
        }
    }

    fn expr(&self, expr: &mut Expr) {
        match expr {
            Expr::Literal(_, typ) | Expr::Var(_, typ, _) => {
                if let Some(typ) = typ {
                    self.typ(typ)
                }
            }
            Expr::Field(_, _, meta, _) => {
                if let Some((typ, _)) = meta.as_mut() {
                    self.typ(typ);
                }
            }
            Expr::Func(_, _, meta, _) => {
                if let Some(typ) = meta.as_mut() {
                    self.typ(typ);
                }
            }
            Expr::Op(_, args, typ, _) => {
                if let Some(typ) = typ {
                    self.typ(typ);
                }
                for arg in args {
                    self.expr(arg);
                }
            }
            Expr::Call(func, args, typ, _) => {
                self.expr(func);
                for arg in args {
                    self.expr(arg);
                }
                if let Some(typ) = typ {
                    self.typ(typ);
                }
            }
            Expr::Block(stmts, result, _) => {
                for stmt in stmts {
                    match stmt {
                        Stmt::Set(_, val) | Stmt::Expr(val) | Stmt::Let(_, val) => self.expr(val),
                    }
                }
                if let Some(result) = result {
                    self.expr(result);
                }
            }
            Expr::Array(_, elems, elem_type, _) => {
                for elem in elems.iter_mut().flatten() {
                    self.expr(elem);
                }
                if let Some(typ) = elem_type {
                    self.typ(typ);
                }
            }
            Expr::Any(inner, meta, _) => {
                self.expr(inner);
                if let Some(meta) = meta {
                    self.typ(&mut meta.result);
                    self.typ(&mut meta.existential);
                }
            }
        }
    }

    pub fn func(&self, func: &mut Func) {
        for (_, typ) in &mut func.args {
            self.typ(typ);
        }
        self.typ(&mut func.result);
        self.expr(&mut func.body);
    }
}
