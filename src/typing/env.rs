use std::collections::{HashMap, HashSet};

use crate::{
    ast::{IntType, Path, Prim, Span, Struct, Type, TypeKind},
    typing::{
        Error,
        sig::{FuncDef, ModuleSig},
        solve::{CorResult, unify},
        sub::Sub,
    },
};

pub struct Global {
    sigs: HashMap<Vec<String>, ModuleSig>,
}

impl Global {
    pub fn new(sigs: HashMap<Vec<String>, ModuleSig>) -> Self {
        Self { sigs }
    }
    fn get_module(&self, path: &Path) -> Result<&ModuleSig, Error> {
        self.sigs
            .get(&path.path)
            .ok_or_else(|| Error::UnknownPath(path.clone()))
    }
    pub fn get_struct(&self, path: &Path, name: &str) -> Result<&Struct, Error> {
        let strukt = self
            .get_module(path)?
            .structs
            .get(name)
            .ok_or_else(|| Error::UnknownName(name.to_owned(), path.span))?;
        Ok(strukt)
    }
    pub fn get_func(&self, path: &Path, name: &str) -> Result<&FuncDef, Error> {
        let func = self
            .get_module(path)?
            .func_defs
            .get(name)
            .ok_or_else(|| Error::UnknownName(name.to_owned(), path.span))?;
        Ok(func)
    }
    pub fn get_cor(&self, path: &Path, name: &str) -> Result<&CorResult, Error> {
        self.get_module(path)?
            .cor_defs
            .get(name)
            .ok_or_else(|| Error::UnknownName(name.to_owned(), path.span))
    }
}

pub struct Local {
    next_unif: usize,
    sub: Sub,
    unifs: HashSet<usize>,
    ints: Vec<(Type, Span)>,
    is_cor: bool,
}

impl Local {
    pub fn new(is_cor: bool) -> Self {
        Self {
            next_unif: 0,
            sub: Sub::default(),
            unifs: HashSet::new(),
            ints: Vec::new(),
            is_cor,
        }
    }

    pub fn fresh(&mut self, span: Span) -> Type {
        let unif = self.next_unif;
        self.unifs.insert(unif);
        self.next_unif += 1;
        Type {
            kind: TypeKind::Unif(unif),
            span,
            children: Vec::new(),
        }
    }

    pub fn skolem(&mut self, name: impl Into<String>, span: Span) -> Type {
        let id = self.next_unif;
        self.next_unif += 1;
        Type::skolem(name, id, span)
    }

    pub fn unify(&mut self, mut a: Type, mut b: Type, span: Span) -> Result<(), Error> {
        self.apply_type(&mut a);
        self.apply_type(&mut b);
        unify(&a, &b, span, &mut self.unifs, &mut self.sub)
    }

    pub fn apply_type(&self, typ: &mut Type) {
        self.sub.typ(typ);
    }

    pub fn solve(mut self, span: Span) -> Result<Sub, Error> {
        // All we need to do here is:
        // 1. Instatiate all the unknown unifs
        // 2. Check that all the integer unifs have been instaniated to integer types
        let isize_type = Type::int(IntType::isize(), span);
        while let Some(typ) = self.unifs.iter().next().copied() {
            self.unify(
                Type {
                    kind: TypeKind::Unif(typ),
                    span,
                    children: vec![],
                },
                isize_type.clone(),
                span,
            )?;
        }
        println!("Produced sub, {:?}", &self.sub);
        for (mut typ, span) in self.ints.drain(0..) {
            self.sub.typ(&mut typ);
            if !matches!(typ.kind, TypeKind::Primitive(Prim::Int(..))) {
                return Err(Error::BadInt(typ.clone(), span));
            }
        }
        Ok(self.sub)
    }

    pub fn is_cor(&self) -> bool {
        self.is_cor
    }

    pub fn unify_int(&mut self, typ: Type, span: Span) {
        self.ints.push((typ, span));
    }
}

#[derive(Clone, Default)]
pub struct Scope {
    vars: HashMap<String, Type>,
}

impl Scope {
    pub fn set_var(&mut self, var: String, typ: Type) {
        self.vars.insert(var, typ);
    }

    pub fn get_var(&self, var: &str, span: Span) -> Result<&Type, Error> {
        self.vars
            .get(var)
            .ok_or_else(|| Error::UnknownName(var.to_string(), span))
    }
}
