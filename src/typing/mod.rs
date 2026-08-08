use core::fmt;
use std::{collections::HashMap, fmt::Display};

use crate::{
    ast::{Expr, Func, Module, Path, Span, Type},
    typing::{
        env::{Global, Local, Scope},
        infer::check_expr,
        sig::build_sig,
    },
};

pub mod env;
mod infer;
mod sig;
mod solve;
pub mod sub;

#[allow(dead_code)]
#[derive(Debug)]
pub enum Error {
    UnknownName(String, Span),
    TypeMismatch(Type, Type, Span),
    YieldOutsideCor(Span),
    AwaitOutsideCor(Span),
    BadAwait(Type, Span),
    NeedsTypeAnnotation(Expr, Span),
    BadLValue(Expr, Span),
    BadArraySize(usize, Span),
    BadInt(Type, Span),
    UnknownPath(Path),
    CantUnifyAcrossOpen(Type, Span),
    BadOpen(Expr, Span),
}

impl Error {
    pub fn span(&self) -> Span {
        match self {
            Error::UnknownName(_, span)
            | Error::TypeMismatch(_, _, span)
            | Error::YieldOutsideCor(span)
            | Error::AwaitOutsideCor(span)
            | Error::BadAwait(_, span)
            | Error::NeedsTypeAnnotation(_, span)
            | Error::BadLValue(_, span)
            | Error::BadArraySize(_, span)
            | Error::BadInt(_, span)
            | Error::CantUnifyAcrossOpen(_, span)
            | Error::BadOpen(_, span) => *span,
            Error::UnknownPath(path) => path.span,
        }
    }
}

pub fn type_program(modules: &mut HashMap<Vec<String>, Module>) -> Result<Global, Vec<Error>> {
    let sigs = modules
        .iter()
        .map(|(path, module)| (path.clone(), build_sig(module)))
        .collect();
    let env = Global::new(sigs);
    let errors: Vec<_> = modules
        .iter_mut()
        .flat_map(|(path, module)| module.funcs.iter_mut().map(|func| (path.clone(), func)))
        .filter_map(|(path, (_, func))| {
            let path = Path {
                path,
                span: func.result.span,
            };
            if let Err(err) = type_function(&path, func, &env) {
                Some(err)
            } else {
                None
            }
        })
        .collect();
    if errors.is_empty() {
        Ok(env)
    } else {
        Err(errors)
    }
}

fn type_function(path: &Path, func: &mut Func, global: &Global) -> Result<(), Error> {
    let mut local = Local::new(func.is_cor);
    let mut scope = Scope::default();

    for (name, typ) in &func.args {
        scope.set_var(name.clone(), typ.clone());
    }

    check_expr(&mut func.body, &func.result, &global, &mut local, &scope)?;

    let sub = local.solve(func.result.span)?;
    sub.func(func);
    Ok(())
}

impl Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::UnknownName(name, _) => write!(f, "Unknown name {name}"),
            Error::TypeMismatch(a, b, _) => write!(f, "Expected type {a}, got type {b}"),
            Error::YieldOutsideCor(_) => write!(f, "Yield outside of a coroutine"),
            Error::AwaitOutsideCor(_) => write!(f, "Await outside of a coroutine"),
            Error::BadAwait(typ, _) => write!(f, "Cannot await value of type {typ}"),
            Error::NeedsTypeAnnotation(..) => {
                write!(f, "Expression needs a type annotation")
            }
            Error::BadLValue(..) => {
                write!(f, "Expression is not a valid assignment target")
            }
            Error::BadArraySize(size, _) => write!(f, "Invalid array size {size}"),
            Error::BadInt(typ, _) => write!(f, "Bad integer type {typ}"),
            Error::UnknownPath(path) => write!(f, "Unknown path {path:?}"),
            Error::CantUnifyAcrossOpen(typ, _) => write!(f, "Cannot unify across open type {typ}"),
            Error::BadOpen(..) => write!(f, "Cannot open expression"),
        }
    }
}
