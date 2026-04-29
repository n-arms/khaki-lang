//! Implement derivation of pre-type checking structs / functions:
//! - structs get getters, setters, constructors
//! - cors get auto generated @struct.cor_name.poll structs + functions
//!   - might have todo's as function bodies? :check_mark:

use std::collections::HashMap;

use crate::{
    ast::{Expr, Func, Module, Op, Path, Struct, Type, cor_name},
    ord_map::OrdMap,
};

#[derive(Debug)]
pub struct CorParts {
    pub func_name: String,
}

// add Foo.Foo function for each struct Foo
pub fn derive_constructors(module_path: Path, module: &mut Module) {
    for strukt in module.structs.values_mut() {
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
            module_path.clone(),
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
            generics: Vec::new(),
            args: func_args,
            result,
            is_cor: false,
            body,
        };

        module.funcs.insert(strukt.name.clone(), func);
    }
}

// add the appropriate cor + cor.poll struct + function for each cor, returning the list of generated structs
pub fn derive_cor_structs(module_path: Path, module: &mut Module) -> HashMap<String, CorParts> {
    let mut structs = Vec::new();
    let mut funcs = Vec::new();
    let mut struct_names = HashMap::new();

    for func in module.funcs.values() {
        if func.is_cor {
            let cor_name = cor_name(&func.name);
            struct_names.insert(
                cor_name.clone(),
                CorParts {
                    func_name: func.name.clone(),
                },
            );
            let span = func.result.span;
            let cor_generic_types: Vec<_> = func
                .generics
                .iter()
                .map(|name| Type::generic(name.clone(), span))
                .collect();
            let cor_generics = func.generics.clone();
            let cor_type = Type::named(
                module_path.clone(),
                cor_name.clone(),
                cor_generic_types.clone(),
                span,
            );
            let cor_func = Func {
                name: "poll".into(),
                generics: cor_generics.clone(),
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
                generics: cor_generics,
                fields: OrdMap::new(),
                span: func.result.span,
            };
            funcs.push((cor_func.name.clone(), cor_func));
            structs.push((cor_struct.name.clone(), cor_struct));
        }
    }

    module.structs.extend(structs);
    module.funcs.extend(funcs);
    struct_names
}
