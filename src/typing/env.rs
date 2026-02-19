use std::collections::HashMap;

use crate::{
    ast::{Span, Struct, Type, TypeKind},
    typing::{
        Error,
        solve::{self, CorResult, Rule},
        sub::Sub,
    },
};

pub struct Global {
    structs: HashMap<String, Struct>,
}

impl Global {
    pub fn from_program(structs: impl IntoIterator<Item = Struct>) -> Self {
        Self {
            structs: structs
                .into_iter()
                .map(|strukt| (strukt.name.clone(), strukt))
                .collect(),
        }
    }

    pub fn get_struct(&self, name: &str, span: Span) -> Result<&Struct, Error> {
        let strukt = self
            .structs
            .get(name)
            .ok_or_else(|| Error::UnknownName(name.to_string(), span))?;
        Ok(strukt)
    }

    pub fn set_struct(&mut self, strukt: Struct) {
        self.structs.insert(strukt.name.clone(), strukt);
    }
}

pub struct Local {
    next_unif: usize,
    rules: Vec<Rule>,
    is_cor: bool,
}

impl Local {
    pub fn new(is_cor: bool) -> Self {
        Self {
            next_unif: 0,
            rules: Vec::new(),
            is_cor,
        }
    }

    pub fn fresh(&mut self, span: Span) -> Type {
        let unif = self.next_unif;
        self.next_unif += 1;
        Type {
            kind: TypeKind::Unif(unif),
            span,
            children: Vec::new(),
        }
    }

    pub fn unify(&mut self, a: Type, b: Type, span: Span) {
        self.rules.push(Rule::Unify(a, b, span));
    }

    pub fn unify_await(&mut self, cor_type: Type, await_type: Type, span: Span) {
        self.rules.push(Rule::UnifyAwait {
            cor_type,
            await_type,
            span,
        })
    }

    pub fn solve(self, cor_list: &HashMap<String, CorResult>) -> Result<Sub, Error> {
        solve::solve(self.rules, cor_list)
    }

    pub fn is_cor(&self) -> bool {
        self.is_cor
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
