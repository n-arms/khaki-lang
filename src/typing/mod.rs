use std::collections::{HashMap, HashSet};

use crate::{
    ast::{Expr, FuncSpec, Span, Struct, Type, cor_name},
    typing::{
        env::{Global, Local, Scope},
        infer::infer_expr,
        solve::{CorResult, Rule},
        spec::{AnySpec, spec_func},
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
    NeedsTypeAnnotation(Box<Expr>, Span),
    BadLValue(Expr, Span),
    BadArraySize(usize, Span),
    BadInt(Type, Span),
}

// We must iterate over all the desired struct and function specs. We keep a single queue that contains both
// simple struct specs, and compound struct-function specs
pub fn type_program(program: &[Struct]) -> Result<HashMap<Spec, Struct>, Error> {
    let global = Global::from_program(program.iter().cloned());
    let mut cor_list = HashMap::new();

    let mut to_spec = Vec::new();
    let mut seen_specs = HashSet::new();
    let mut output = HashMap::new();

    for strukt in program {
        if strukt.generics.is_empty() {
            to_spec.push(AnySpec::Struct(Spec {
                struct_name: strukt.name.clone(),
                generics: Vec::new(),
            }))
        }
        for func in strukt.funcs.values() {
            if func.is_cor {
                cor_list.insert(
                    cor_name(&strukt.name, &func.name),
                    CorResult {
                        generics: strukt.generics.clone(),
                        func_generics: func.generics.clone(),
                        result: func.result.clone(),
                    },
                );
            }
        }
    }

    while let Some(spec) = to_spec.pop() {
        match spec {
            AnySpec::Struct(spec) => generate_spec(
                &spec,
                program,
                &mut seen_specs,
                &mut to_spec,
                &mut output,
                &global,
                &cor_list,
            )?,
            AnySpec::Func(spec, func_spec) => {
                generate_spec(
                    &spec,
                    program,
                    &mut seen_specs,
                    &mut to_spec,
                    &mut output,
                    &global,
                    &cor_list,
                )?;
            }
        }
    }

    Ok(output)
}

fn generate_spec(
    spec: &Spec,
    program: &[Struct],
    seen_specs: &mut HashSet<AnySpec>,
    to_spec: &mut Vec<AnySpec>,
    output: &mut HashMap<Spec, Struct>,
    global: &Global,
    cor_list: &HashMap<String, CorResult>,
) -> Result<(), Error> {
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
        if func.generics.is_empty() {
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
            let sub = local.solve(&cor_list, func.result.span)?;
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
    }

    output.insert(spec.clone(), strukt);
    Ok(())
}

fn generate_func_spec(
    spec: &Spec,
    func_spec: &FuncSpec,
    program: &[Struct],
    seen_specs: &mut HashSet<AnySpec>,
    to_spec: &mut Vec<AnySpec>,
    output: &mut HashMap<Spec, Struct>,
    global: &Global,
    cor_list: &HashMap<String, CorResult>,
) -> Result<(), Error> {
    println!("Typing spec {spec:?}");
    let strukt = program
        .iter()
        .find(|strukt| strukt.name == spec.struct_name)
        .unwrap()
        .clone();
    let mut generic_sub = Sub::default();
    for (name, typ) in strukt.generics.iter().zip(&spec.generics) {
        generic_sub.set_generic(name.clone(), typ.clone());
    }
    let mut func = strukt.funcs.get(&func_spec).unwrap().clone();
    for (name, typ) in func.generics.iter().zip(&func_spec.generics) {
        generic_sub.set_generic(name.clone(), typ.clone());
    }

    generic_sub.func(&mut func);
    func.generics.clear();

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
    let sub = local.solve(&cor_list, func.result.span)?;
    println!("Got sub {sub:?}");
    println!("Unsubbed func: {func:#?}");
    sub.func(&mut func);
    println!("Subbed into func: {func:#?}");
    for spec in spec_func(&func) {
        if !seen_specs.contains(&spec) {
            to_spec.push(spec.clone());
            seen_specs.insert(spec);
        }
    }

    output.entry(spec.clone()).and_modify(|strukt| {
        strukt.funcs.insert(func_spec.clone(), func);
    });
    Ok(())
}
