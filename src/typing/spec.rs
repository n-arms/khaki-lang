use std::collections::{HashMap, HashSet};

use crate::ast::{Expr, Func, FuncSpec, Stmt, Type, TypeKind};

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct Spec {
    pub struct_name: String,
    pub generics: Vec<Type>,
}

#[derive(Default)]
pub struct Specs {
    specs: HashMap<Spec, HashSet<FuncSpec>>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum AnySpec {
    Struct(Spec),
    Func(Spec, FuncSpec),
}

impl Specs {
    pub fn spec_struct(&mut self, name: String, generics: Vec<Type>) {
        self.specs
            .entry(Spec {
                struct_name: name,
                generics,
            })
            .or_default();
    }

    pub fn spec_func(
        &mut self,
        struct_name: String,
        struct_generics: Vec<Type>,
        func_name: String,
        func_generics: Vec<Type>,
    ) {
        self.specs
            .entry(Spec {
                struct_name,
                generics: struct_generics,
            })
            .or_default()
            .insert(FuncSpec {
                func_name,
                generics: func_generics,
            });
    }

    pub fn all_specs(&self) -> Vec<AnySpec> {
        let mut specs = Vec::new();
        for (strukt, funcs) in &self.specs {
            specs.push(AnySpec::Struct(strukt.clone()));
            for func in funcs {
                specs.push(AnySpec::Func(strukt.clone(), func.clone()));
            }
        }
        specs
    }
}

pub fn spec_func(func: &Func) -> impl IntoIterator<Item = AnySpec> {
    let mut specs = Specs::default();

    for (_, typ) in &func.args {
        spec_type(typ, &mut specs);
    }

    spec_type(&func.result, &mut specs);
    spec_expr(&func.body, &mut specs);

    specs.all_specs()
}

fn spec_expr(expr: &Expr, specs: &mut Specs) {
    match expr {
        Expr::Literal(_, typ) | Expr::Var(_, typ, _) => {
            spec_type(typ.as_ref().unwrap(), specs);
        }
        Expr::Field(_, _, meta, _) => spec_type(&meta.as_ref().unwrap().0, specs),
        Expr::Func(struct_name, func_name, meta, _) => {
            let (typ, struct_generics, func_generics) = meta.as_ref().unwrap();
            spec_type(typ, specs);
            specs.spec_func(
                struct_name.clone(),
                struct_generics.clone(),
                func_name.clone(),
                func_generics.clone(),
            );
        }
        Expr::Op(_, exprs, typ, _) => {
            for expr in exprs {
                spec_expr(expr, specs);
            }
            spec_type(typ.as_ref().unwrap(), specs);
        }
        Expr::Call(func, args, typ, _) => {
            spec_expr(func, specs);
            for arg in args {
                spec_expr(arg, specs);
            }
            spec_type(typ.as_ref().unwrap(), specs);
        }
        Expr::Block(stmts, result, span) => {
            for stmt in stmts {
                match stmt {
                    Stmt::Set(_, val) | Stmt::Expr(val) | Stmt::Let(_, val) => {
                        spec_expr(val, specs)
                    }
                }
            }
            if let Some(result) = result {
                spec_expr(result, specs);
            }
        }
        Expr::MethodCall(..) => unreachable!(),
        Expr::Array(_, elems, elem_type, _) => {
            for elem in elems.iter().flatten() {
                spec_expr(elem, specs);
            }
            let elem_type = elem_type.as_ref().unwrap();
            spec_type(elem_type, specs);
            specs.spec_struct("Slice".into(), vec![elem_type.clone()]);
        }
    }
}

fn spec_type(typ: &Type, specs: &mut Specs) {
    for typ in &typ.children {
        spec_type(typ, specs);
    }
    match &typ.kind {
        TypeKind::Func => {}
        TypeKind::Unif(u) => unreachable!("Found unif {u} at span {:?}", typ.span),
        TypeKind::Generic(n) => unreachable!("Found generic {n} at span {:?}", typ.span),
        TypeKind::Named(name) => {
            specs.spec_struct(name.clone(), typ.children.clone());
        }
        TypeKind::Array(_) => {}
    }
}
