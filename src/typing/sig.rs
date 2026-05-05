//! Collection of all types and type signatures to export from a module

use std::collections::HashMap;

use crate::{
    ast::{Module, Struct, Type},
    typing::solve::CorResult,
};

#[derive(Debug)]
pub struct FuncDef {
    pub generics: Vec<String>,
    pub args: Vec<(String, Type)>,
    pub result: Type,
    pub is_cor: bool,
}

impl FuncDef {
    pub fn arg_types(&self) -> Vec<Type> {
        self.args.iter().map(|(_, typ)| typ.clone()).collect()
    }
}

#[derive(Debug)]
pub struct ModuleSig {
    pub structs: HashMap<String, Struct>,
    pub func_defs: HashMap<String, FuncDef>,
    pub cor_defs: HashMap<String, CorResult>,
}

pub fn build_sig(module: &Module) -> ModuleSig {
    let structs = module.structs.clone();
    let func_defs = module
        .funcs
        .iter()
        .map(|(name, func)| {
            (
                name.clone(),
                FuncDef {
                    generics: func.generics.clone(),
                    args: func.args.clone(),
                    result: func.result.clone(),
                    is_cor: func.is_cor,
                },
            )
        })
        .collect();
    let cor_defs = module
        .funcs
        .iter()
        .filter_map(|(name, func)| {
            if func.is_cor {
                Some((
                    name.clone(),
                    CorResult {
                        generics: func.generics.clone(),
                        result: func.result.clone(),
                    },
                ))
            } else {
                None
            }
        })
        .collect();
    ModuleSig {
        structs,
        func_defs,
        cor_defs,
    }
}
