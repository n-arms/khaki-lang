use std::collections::{HashMap, HashSet};

use crate::{
    ast::{Expr, Func, Op, Span, Struct, Type, cor_name},
    typing::{
        env::{Global, Local, Scope},
        infer::infer_expr,
        solve::{CorResult, Rule, solve},
        spec::spec_func,
        sub::Sub,
    },
};

mod env;
mod infer;
mod solve;
mod spec;
mod sub;

pub use spec::Spec;

#[derive(Debug)]
pub enum Error {
    UnknownName(String, Span),
    TypeMismatch(Type, Type, Span),
    TypeSolverStuck(Vec<Rule>),
    YieldOutsideCor(Span),
    AwaitOutsideCor(Span),
    BadAwait(Type, Span),
    BadRefLValue(Expr, Span),
    NeedsTypeAnnotation(Box<Expr>, Span),
}

pub fn type_program(program: &[Struct]) -> Result<HashMap<Spec, Struct>, Error> {
    let global = Global::from_program(program.iter().cloned());
    let mut cor_list = HashMap::new();

    let mut to_spec = Vec::new();
    let mut seen_specs = HashSet::new();
    let mut output = HashMap::new();

    for strukt in program {
        if strukt.generics.is_empty() {
            to_spec.push(Spec {
                struct_name: strukt.name.clone(),
                generics: Vec::new(),
            })
        }
        for func in strukt.funcs.values() {
            if func.is_cor {
                cor_list.insert(
                    cor_name(&strukt.name, &func.name),
                    CorResult {
                        generics: strukt.generics.clone(),
                        result: func.result.clone(),
                    },
                );
            }
        }
    }

    while let Some(spec) = to_spec.pop() {
        println!("Typing spec {spec:?}");
        let mut strukt = program
            .iter()
            .find(|strukt| strukt.name == spec.struct_name)
            .unwrap()
            .clone();
        let mut generic_sub = Sub::default();
        for (name, typ) in strukt.generics.iter().zip(&spec.generics) {
            generic_sub.set_generic(name.clone(), typ.clone());
        }
        strukt.generics.clear();
        generic_sub.strukt(&mut strukt);
        for func in strukt.funcs.values_mut() {
            println!("On func {:?}", func.name);
            println!("Func is: {:?}", func);
            let mut local = Local::new(func.is_cor);
            let mut scope = Scope::default();
            for (arg, typ) in &func.args {
                scope.set_var(arg.clone(), typ.clone());
            }
            infer_expr(&mut func.body, &global, &mut local, &scope)?;
            println!("Infered expr {:#?}", func.body);
            local.unify(func.result.clone(), func.body.get_type(), func.result.span);
            let sub = local.solve(&cor_list)?;
            println!("Got sub {sub:?}");
            println!("Unsubbed func: {func:#?}");
            sub.func(func);
            println!("Subbed into func: {func:#?}");
            for spec in spec_func(func) {
                if !seen_specs.contains(&spec) {
                    to_spec.push(spec.clone());
                    seen_specs.insert(spec);
                }
            }
        }

        output.insert(spec, strukt);
    }

    Ok(output)
}
