use crate::{
    ast::{Expr, Literal, Op, Span, Stmt, Type, cor_name},
    typing::{
        Error,
        env::{Global, Local, Scope},
        sub::Sub,
    },
};

pub fn infer_expr(
    expr: &mut Expr,
    global: &Global,
    local: &mut Local,
    scope: &Scope,
) -> Result<(), Error> {
    match expr {
        Expr::Var(name, typ, span) => {
            *typ = Some(scope.get_var(name, *span)?.clone());
        }
        Expr::Func(struct_name, func_name, meta, span) => {
            let strukt = global.get_struct(struct_name, *span)?;
            let func = strukt
                .funcs
                .get(func_name)
                .ok_or_else(|| Error::UnknownName(func_name.to_string(), *span))?;
            let (generics, generic_sub) = instantiate(&strukt.generics, local, *span);
            let result_type = if func.is_cor {
                Type::named(cor_name(&strukt.name, &func.name), generics.clone(), *span)
            } else {
                func.result.clone()
            };
            let mut func_type = Type::func(func.arg_types(), result_type, *span);
            generic_sub.typ(&mut func_type);
            *meta = Some((func_type, generics));
        }
        Expr::Literal(literal, typ) => match literal {
            Literal::Bool(_, span) => *typ = Some(Type::bool(*span)),
            Literal::Number(_, span) => *typ = Some(Type::int(*span)),
            Literal::Unit(span) => *typ = Some(Type::unit(*span)),
        },
        Expr::Op(op, args, typ, span) => {
            for arg in args.iter_mut() {
                infer_expr(arg, global, local, scope)?;
            }
            *typ = Some(match op {
                Op::Builtin(..) => local.fresh(*span),
                Op::Await => {
                    if !local.is_cor() {
                        return Err(Error::AwaitOutsideCor(*span));
                    }
                    let awaited = local.fresh(*span);
                    local.unify_await(args[0].get_type(), awaited.clone(), *span);
                    awaited
                }
                Op::Yield => {
                    if !local.is_cor() {
                        return Err(Error::YieldOutsideCor(*span));
                    }
                    Type::unit(*span)
                }
            });
        }
        Expr::Call(func, args, meta, span) => {
            let result_type = local.fresh(*span);
            let arg_types: Vec<_> = args
                .iter_mut()
                .map(|arg| {
                    infer_expr(arg, global, local, scope)?;
                    Ok(arg.get_type())
                })
                .collect::<Result<_, _>>()?;
            infer_expr(func, global, local, scope)?;
            local.unify(
                Type::func(arg_types, result_type.clone(), *span),
                func.get_type(),
                *span,
            );
            *meta = Some(result_type);
        }
        Expr::Block(stmts, result) => {
            let mut inner = scope.clone();
            for stmt in stmts {
                match stmt {
                    Stmt::Let(var, expr) => {
                        infer_expr(expr, global, local, &inner)?;
                        inner.set_var(var.clone(), expr.get_type());
                    }
                }
            }
            infer_expr(result, global, local, &inner)?;
        }
    }

    Ok(())
}

fn instantiate(generics: &[String], local: &mut Local, span: Span) -> (Vec<Type>, Sub) {
    let mut generic_types = Vec::new();
    let mut sub = Sub::default();

    for name in generics {
        let typ = local.fresh(span);
        generic_types.push(typ.clone());
        sub.set_generic(name.to_string(), typ);
    }

    (generic_types, sub)
}
