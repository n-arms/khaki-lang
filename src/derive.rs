//! Implement derivation of pre-type checking structs / functions:
//! - structs get getters, setters, constructors
//! - cors get auto generated @struct.cor_name.poll structs + functions
//!   - might have todo's as function bodies? :check_mark:

use std::collections::{HashMap, HashSet};

use crate::{
    ast::{Expr, Func, Op, Struct, Type, cor_name},
    ord_map::OrdMap,
};

#[derive(Debug)]
pub struct CorParts {
    pub struct_name: String,
    pub func_name: String,
}

// add Foo.Foo function for each struct Foo
pub fn derive_constructors(program: &mut Vec<Struct>) {
    for strukt in program.iter_mut() {
        let span = strukt.span;
        let (func_args, op_args) = strukt
            .fields
            .iter()
            .map(|(name, typ)| {
                let expr = Expr::Var(name.clone(), None, span);
                ((name.clone(), typ.clone()), expr)
            })
            .unzip();
        let result = Type::named(
            strukt.name.clone(),
            strukt
                .generics
                .iter()
                .map(|name| Type::generic(name, span))
                .collect(),
            span,
        );

        let body = Expr::Op(Op::Constructor(strukt.name.clone()), op_args, None, span);

        let func = Func {
            name: strukt.name.clone(),
            args: func_args,
            result,
            is_cor: false,
            body,
        };

        strukt.funcs.insert(strukt.name.clone(), func);
    }
}

// add the appropriate cor + cor.poll struct + function for each cor, returning the list of generated structs
pub fn derive_cor_structs(program: &mut Vec<Struct>) -> HashMap<String, CorParts> {
    let mut structs = Vec::new();
    let mut struct_names = HashMap::new();

    for strukt in program.iter_mut() {
        for func in strukt.funcs.values() {
            if func.is_cor {
                let cor_name = cor_name(&strukt.name, &func.name);
                struct_names.insert(
                    cor_name.clone(),
                    CorParts {
                        struct_name: strukt.name.clone(),
                        func_name: func.name.clone(),
                    },
                );
                let span = func.result.span;
                let cor_generic_types: Vec<_> = strukt
                    .generics
                    .iter()
                    .map(|name| Type::generic(name.clone(), span))
                    .collect();
                let cor_type = Type::named(cor_name.clone(), cor_generic_types, span);
                let cor_func = Func {
                    name: "poll".into(),
                    args: vec![
                        ("cor".into(), Type::ptr(cor_type, span)),
                        ("result".into(), Type::ptr(func.result.clone(), span)),
                    ],
                    result: Type::bool(span),
                    is_cor: false,
                    body: Expr::Op(Op::Builtin("todo".into()), vec![], None, span),
                };
                let cor_struct = Struct {
                    name: cor_name,
                    generics: strukt.generics.clone(),
                    fields: OrdMap::new(),
                    funcs: HashMap::from([("poll".into(), cor_func)]),
                    span: func.result.span,
                };
                structs.push(cor_struct);
            }
        }
    }

    program.extend(structs);
    struct_names
}
