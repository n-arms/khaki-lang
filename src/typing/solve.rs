use std::collections::{HashMap, HashSet};

use crate::{
    ast::{IntType, Span, Type, TypeKind},
    typing::{Error, sub::Sub},
};

#[derive(Clone, Debug)]
pub enum Rule {
    Unify(Type, Type, Span),
    UnifyAwait {
        cor_type: Type,
        await_type: Type,
        span: Span,
    },
    UnifyInt(Type, Span),
}

pub struct CorResult {
    pub generics: Vec<String>,
    pub result: Type,
}

impl Rule {
    /// returns Ok(true) if rule is solved, Ok(false) if it can be solved later but not now, Err if it failed.
    fn try_solve(
        &mut self,
        sub: &mut Sub,
        cor_list: &HashMap<String, CorResult>,
        unifs: &mut HashSet<usize>,
    ) -> Result<bool, Error> {
        match self {
            Rule::Unify(a, b, span) => {
                sub.typ(a);
                sub.typ(b);
                unify(a, b, *span, unifs, sub)?;
                Ok(true)
            }
            Rule::UnifyAwait {
                cor_type,
                await_type,
                span,
            } => {
                sub.typ(cor_type);
                sub.typ(await_type);
                let cor_name = match &cor_type.kind {
                    TypeKind::Func | TypeKind::Array(..) => {
                        return Err(Error::BadAwait(cor_type.clone(), *span));
                    }
                    TypeKind::Named(name) => name,
                    TypeKind::Unif(_) => return Ok(false),
                    TypeKind::Generic(g) => unreachable!("Unbound generic unif{g}"),
                };

                let cor_result = cor_list
                    .get(cor_name)
                    .ok_or_else(|| Error::UnknownName(cor_name.clone(), *span))?;

                let mut generic_sub = Sub::default();
                for (name, typ) in cor_result.generics.iter().zip(&cor_type.children) {
                    generic_sub.set_generic(name.clone(), typ.clone());
                }
                let mut result_type = cor_result.result.clone();
                generic_sub.typ(&mut result_type);

                unify(&result_type, await_type, *span, unifs, sub)?;

                Ok(true)
            }
            Rule::UnifyInt(int_type, span) => {
                sub.typ(int_type);
                match &int_type.kind {
                    TypeKind::Func | TypeKind::Array(_) => {
                        return Err(Error::BadInt(int_type.clone(), *span));
                    }
                    TypeKind::Named(..) => {}
                    TypeKind::Unif(_) => return Ok(false),
                    TypeKind::Generic(g) => unreachable!("Unbound generic {g}"),
                };
                if let Some(_) = IntType::from_type(&int_type) {
                    Ok(true)
                } else {
                    Err(Error::BadInt(int_type.clone(), *span))
                }
            }
        }
    }
}

pub fn solve(
    mut rules: Vec<Rule>,
    unifs: &HashSet<usize>,
    cor_list: &HashMap<String, CorResult>,
    span: Span,
) -> Result<Sub, Error> {
    let mut sub = Sub::default();
    let mut unifs = unifs.clone();

    while rules.iter().any(|rule| !matches!(rule, Rule::UnifyInt(..))) {
        let mut changed = false;
        for i in (0..rules.len()).rev() {
            let rule = &mut rules[i];
            if rule.try_solve(&mut sub, cor_list, &mut unifs)? {
                changed = true;
                rules.remove(i);
            }
        }
        if !changed {
            return Err(Error::TypeSolverStuck(rules.clone()));
        }
    }

    println!("before solving unconstrained unifs: {sub:?}");

    for unif in unifs {
        bind(
            unif,
            IntType::isize().to_type(span),
            span,
            &mut HashSet::new(),
            &mut sub,
        )?;
    }

    println!("after constraining: {sub:?}");

    Ok(sub)
}

fn unify(
    a: &Type,
    b: &Type,
    span: Span,
    unifs: &mut HashSet<usize>,
    sub: &mut Sub,
) -> Result<(), Error> {
    use TypeKind::*;
    if a.kind == b.kind {
        for (a, b) in a.children.iter().zip(&b.children) {
            unify(a, b, span, unifs, sub)?;
        }
    }
    match (&a.kind, &b.kind) {
        (Func, Func) => {}
        (Named(a), Named(b)) if a == b => {}
        (Generic(a), Generic(b)) if a == b => {}
        (Unif(u), _) => {
            bind(*u, b.clone(), span, unifs, sub)?;
        }
        (_, Unif(u)) => {
            bind(*u, a.clone(), span, unifs, sub)?;
        }
        _ => return Err(Error::TypeMismatch(a.clone(), b.clone(), span)),
    }
    Ok(())
}

fn bind(
    unif: usize,
    typ: Type,
    span: Span,
    unifs: &mut HashSet<usize>,
    sub: &mut Sub,
) -> Result<(), Error> {
    println!("binding unif {unif} to type {typ:?}");
    unifs.remove(&unif);
    let mut new_sub = Sub::default();
    new_sub.set_unif(unif, typ.clone());
    for existing in sub.unifs.values_mut() {
        new_sub.typ(existing);
    }
    if let Some(old) = sub.get_unif(unif).cloned() {
        unify(&old, &typ, span, unifs, sub)?;
    } else {
        sub.set_unif(unif, typ);
    }
    Ok(())
}
