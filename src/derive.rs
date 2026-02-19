//! Implement derivation of pre-type checking structs / functions:
//! - structs get getters, setters, constructors
//! - cors get auto generated @struct.cor_name.poll structs + functions
//!   - might have todo's as function bodies? :check_mark:

use std::collections::HashMap;

use crate::ast::{Expr, Func, Op, Struct, Type, cor_name};

pub fn derive(program: &mut Vec<Struct>) {
    derive_cor_structs(program);
}

fn derive_cor_structs(program: &mut Vec<Struct>) {
    let mut structs = Vec::new();

    for strukt in program.iter_mut() {
        for func in strukt.funcs.values() {
            if func.is_cor {
                let cor_name = cor_name(&strukt.name, &func.name);
                let cor_generic_types: Vec<_> = strukt
                    .generics
                    .iter()
                    .map(|name| Type::generic(name.clone(), func.result.span))
                    .collect();
                let cor_type = Type::named(cor_name.clone(), cor_generic_types, func.result.span);
                let cor_func = Func {
                    name: "poll".into(),
                    args: vec![("cor".into(), cor_type)],
                    result: Type::bool(func.result.span),
                    is_cor: false,
                    body: Expr::Op(Op::Builtin("todo".into()), vec![], None, func.result.span),
                };
                let cor_struct = Struct {
                    name: cor_name,
                    generics: strukt.generics.clone(),
                    fields: HashMap::new(),
                    funcs: HashMap::from([("poll".into(), cor_func)]),
                };
                structs.push(cor_struct);
            }
        }
    }

    program.extend(structs);
}
