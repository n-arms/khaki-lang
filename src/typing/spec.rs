use std::collections::HashSet;

use crate::ast::{Expr, Func, Stmt, Type, TypeKind};

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct Spec {
    pub struct_name: String,
    pub generics: Vec<Type>,
}

pub fn spec_func(func: &Func) -> impl IntoIterator<Item = Spec> {
    let mut specs = HashSet::new();

    for (_, typ) in &func.args {
        spec_type(typ, &mut specs);
    }

    spec_type(&func.result, &mut specs);
    spec_expr(&func.body, &mut specs);

    specs
}

fn spec_expr(expr: &Expr, specs: &mut HashSet<Spec>) {
    match expr {
        Expr::Literal(_, typ) | Expr::Var(_, typ, _) | Expr::Field(_, _, typ, _) => {
            spec_type(typ.as_ref().unwrap(), specs);
        }
        Expr::Func(struct_name, _, meta, _) => {
            let (typ, generics) = meta.as_ref().unwrap();
            spec_type(typ, specs);
            specs.insert(Spec {
                struct_name: struct_name.clone(),
                generics: generics.clone(),
            });
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
    }
}

fn spec_type(typ: &Type, specs: &mut HashSet<Spec>) {
    for typ in &typ.children {
        spec_type(typ, specs);
    }
    match &typ.kind {
        TypeKind::Func => {}
        TypeKind::Unif(u) => unreachable!("Found unif {u} at span {:?}", typ.span),
        TypeKind::Generic(n) => unreachable!("Found generic {n} at span {:?}", typ.span),
        TypeKind::Named(name) => {
            specs.insert(Spec {
                struct_name: name.clone(),
                generics: typ.children.clone(),
            });
        }
    }
}
