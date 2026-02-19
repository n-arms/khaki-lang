use std::collections::HashMap;

use crate::ast::{Expr, Func, Span, Stmt, Struct, Type, TypeKind};

#[derive(Default, Debug)]
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

    fn generic(&self, generic: &str, span: Span) -> Type {
        if let Some(typ) = self.generics.get(generic) {
            typ.clone()
        } else {
            Type {
                kind: TypeKind::Generic(generic.to_string()),
                span,
                children: Vec::new(),
            }
        }
    }

    pub fn typ(&self, typ: &mut Type) {
        for child in &mut typ.children {
            self.typ(child);
        }
        match &mut typ.kind {
            TypeKind::Func => {}
            TypeKind::Named(_) => {}
            TypeKind::Unif(unif) => *typ = self.unif(*unif, typ.span),
            TypeKind::Generic(name) => *typ = self.generic(name, typ.span),
        }
    }

    fn expr(&self, expr: &mut Expr) {
        match expr {
            Expr::Literal(_, typ) | Expr::Var(_, typ, _) => {
                if let Some(typ) = typ {
                    self.typ(typ)
                }
            }
            Expr::Func(_, _, meta, _) => {
                if let Some((typ, generics)) = meta.as_mut() {
                    self.typ(typ);
                    for typ in generics {
                        self.typ(typ);
                    }
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
            Expr::Block(stmts, result) => {
                for stmt in stmts {
                    match stmt {
                        Stmt::Let(_, val) => self.expr(val),
                    }
                }
                self.expr(result);
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

    pub fn strukt(&self, strukt: &mut Struct) {
        for typ in strukt.fields.values_mut() {
            self.typ(typ);
        }

        for func in strukt.funcs.values_mut() {
            self.func(func);
        }
    }
}
