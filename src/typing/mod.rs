use std::collections::HashMap;

use rayon::iter::{IntoParallelRefIterator, IntoParallelRefMutIterator, ParallelIterator};

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

pub fn type_program(modules: &mut HashMap<Vec<String>, Module>) -> Result<Global, Vec<Error>> {
    let sigs = modules
        .par_iter()
        .map(|(path, module)| (path.clone(), build_sig(module)))
        .collect();
    let env = Global::new(sigs);
    let errors: Vec<_> = modules
        .par_iter_mut()
        .flat_map_iter(|(path, module)| module.funcs.iter_mut().map(|func| (path.clone(), func)))
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

    let expected_type = func.result_type(path);

    check_expr(&mut func.body, &expected_type, &global, &mut local, &scope)?;

    let sub = local.solve(func.result.span)?;
    sub.func(func);
    Ok(())
}
