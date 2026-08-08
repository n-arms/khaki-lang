use std::collections::HashSet;

use crate::{
    ast::{Span, Type, TypeKind},
    typing::{Error, sub::Sub},
};

#[derive(Debug)]
pub struct CorResult {
    pub generics: Vec<String>,
    pub result: Type,
}

pub fn unify(
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
        // TODO: support structural function/any type unification (ie should be able to unify `fn[T](T): T` and `fn[L](L): L`, or `any[T] T` and `any[L] L`)
        (Func(g1), Func(g2)) if g1 == g2 => {}
        (Any(g1), Any(g2)) if g1 == g2 => {}
        (Primitive(prim1), Primitive(prim2)) if prim1 == prim2 => {}
        (Named(path1, name1), Named(path2, name2)) if path1 == path2 && name1 == name2 => {}
        (Cor(path1, name1), Cor(path2, name2)) if path1 == path2 && name1 == name2 => {}
        (Generic(name1, id1), Generic(name2, id2)) if name1 == name2 && id1 == id2 => {}
        (Unif(u), Unif(v)) if u == v => {}
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
