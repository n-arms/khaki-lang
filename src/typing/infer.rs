use crate::{
    ast::{Expr, IntType, Literal, Op, Span, Stmt, Type, TypeKind, cor_name},
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
    println!("infer {expr:?}");
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
            Literal::Number(_, span) => {
                let unif = local.fresh(*span);
                local.unify_int(unif.clone(), *span);
                *typ = Some(unif)
            }
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
                Op::Ref => {
                    ensure_lvalue(&args[0], *span)?;
                    Type::named("Ptr".into(), vec![args[0].get_type()], *span)
                }
                Op::Deref => {
                    let result = local.fresh(*span);
                    let arg_type = args[0].get_type();
                    if let TypeKind::Named(name) = &arg_type.kind {
                        if name == "Ptr" {
                            *typ = Some(arg_type.children[0].clone());
                            return Ok(());
                        }
                    }
                    local.unify(arg_type, Type::ptr(result.clone(), *span), *span);
                    result
                }
                Op::If => {
                    local.unify(Type::bool(*span), args[0].get_type(), *span);
                    local.unify(args[1].get_type(), args[2].get_type(), *span);
                    args[1].get_type()
                }
                Op::While => {
                    local.unify(Type::bool(*span), args[0].get_type(), *span);
                    Type::unit(*span)
                }
                Op::Constructor(..) => local.fresh(*span),
                Op::SliceIndex => {
                    let result = local.fresh(*span);
                    let arg_type = args[0].get_type();
                    local.unify(args[1].get_type(), IntType::usize().to_type(*span), *span);
                    if let TypeKind::Named(name) = &arg_type.kind {
                        if name == "Slice" {
                            *typ = Some(arg_type.children[0].clone());
                            return Ok(());
                        }
                    }
                    local.unify(arg_type, Type::slice(result.clone(), *span), *span);
                    result
                }
                Op::Arith(_) => {
                    local.unify(args[0].get_type(), args[1].get_type(), *span);
                    local.unify_int(args[0].get_type(), *span);
                    args[0].get_type()
                }
                Op::Cmp(_) => {
                    local.unify(args[0].get_type(), args[1].get_type(), *span);
                    local.unify_int(args[0].get_type(), *span);
                    // TODO: support comparisons for things that aren't integers
                    Type::bool(*span)
                }
                Op::Logic(_) => {
                    local.unify(args[0].get_type(), Type::bool(*span), *span);
                    local.unify(args[1].get_type(), Type::bool(*span), *span);
                    Type::bool(*span)
                }
            });
        }
        Expr::Call(func, args, meta, span) => {
            let arg_types: Vec<_> = args
                .iter_mut()
                .map(|arg| {
                    infer_expr(arg, global, local, scope)?;
                    Ok(arg.get_type())
                })
                .collect::<Result<_, _>>()?;
            infer_expr(func, global, local, scope)?;

            let func_type = func.get_type();

            let result_type = if let TypeKind::Func = func_type.kind {
                for (expected, arg_type) in func_type.children.iter().zip(arg_types) {
                    local.unify(arg_type, expected.clone(), *span);
                }
                func_type.children.last().unwrap().clone()
            } else {
                let result_type = local.fresh(*span);
                local.unify(
                    Type::func(arg_types, result_type.clone(), *span),
                    func.get_type(),
                    *span,
                );
                result_type
            };
            *meta = Some(result_type);
        }
        Expr::Block(stmts, result, span) => {
            let mut inner = scope.clone();
            for stmt in stmts {
                match stmt {
                    Stmt::Let(var, expr) => {
                        infer_expr(expr, global, local, &inner)?;
                        inner.set_var(var.clone(), expr.get_type());
                    }
                    Stmt::Set(lval, expr) => {
                        infer_expr(lval, global, local, &inner)?;
                        ensure_lvalue(lval, *span)?;
                        infer_expr(expr, global, local, &inner)?;
                        let expr_type = expr.get_type();
                        let span = expr_type.span;
                        local.unify(lval.get_type(), expr_type, span);
                    }
                    Stmt::Expr(expr) => {
                        infer_expr(expr, global, local, &inner)?;
                    }
                }
            }
            if let Some(result) = result {
                infer_expr(result, global, local, &inner)?;
            }
        }
        Expr::Field(struct_expr, field_name, typ, span) => {
            infer_expr(struct_expr, global, local, scope)?;
            let struct_type = struct_expr.get_type();
            let TypeKind::Named(struct_name) = &struct_type.kind else {
                return Err(Error::NeedsTypeAnnotation(struct_expr.clone(), *span));
            };
            let strukt = global.get_struct(struct_name, *span)?;
            let mut generic_sub = Sub::default();
            for (name, typ) in strukt.generics.iter().zip(&struct_type.children) {
                generic_sub.set_generic(name.clone(), typ.clone());
            }
            if let Some(mut field_type) = strukt.fields.get(&*field_name).cloned() {
                generic_sub.typ(&mut field_type);
                let index = strukt.fields.find_index(&*field_name).unwrap();
                *typ = Some((field_type, index));
            } else {
                return Err(Error::UnknownName(field_name.clone(), *span));
            }
        }
        Expr::Array(size, elems, elem_type, span) => {
            if let Some(elems) = elems {
                if *size != elems.len() {
                    return Err(Error::BadArraySize(*size, *span));
                }
                for elem in elems.iter_mut() {
                    infer_expr(elem, global, local, scope)?;
                }
                for pair in elems.windows(2) {
                    let [a, b]: &[Expr; _] = pair.try_into().unwrap();
                    local.unify(a.get_type(), b.get_type(), *span);
                }
                if let Some(elem_type) = elem_type {
                    if let Some(first) = elems.first() {
                        local.unify(elem_type.clone(), first.get_type(), *span);
                    }
                } else {
                    if let Some(first) = elems.first() {
                        *elem_type = Some(first.get_type());
                    } else {
                        *elem_type = Some(local.fresh(*span));
                    }
                }
            } else {
                if elem_type.is_none() {
                    *elem_type = Some(local.fresh(*span));
                }
            }
        }
        Expr::MethodCall(struct_expr, method_name, args, typ, span) => {
            for arg in args.iter_mut() {
                infer_expr(arg, global, local, scope)?;
            }
            infer_expr(struct_expr, global, local, scope)?;
            println!("infered elems to struct expr {struct_expr:?}, args {args:?}");
            let struct_type = struct_expr.get_type();
            let TypeKind::Named(struct_name) = &struct_type.kind else {
                return Err(Error::NeedsTypeAnnotation(struct_expr.clone(), *span));
            };
            let strukt = global.get_struct(struct_name, *span)?;
            let Some(func) = strukt.funcs.get(method_name) else {
                return Err(Error::UnknownName(method_name.clone(), *span));
            };
            let mut generic_sub = Sub::default();
            let generics = struct_type.children.clone();
            for (name, typ) in strukt.generics.iter().zip(&generics) {
                generic_sub.set_generic(name.clone(), typ.clone());
            }

            let result_type = if func.is_cor {
                Type::named(cor_name(&strukt.name, &func.name), generics.clone(), *span)
            } else {
                func.result.clone()
            };
            let mut func_type = Type::func(func.arg_types(), result_type.clone(), *span);
            generic_sub.typ(&mut func_type);

            let func_expr = Expr::Func(
                struct_name.clone(),
                method_name.clone(),
                Some((func_type, generics)),
                *span,
            );

            let mut args = args.clone();
            args.insert(0, struct_expr.as_ref().clone());

            *expr = Expr::Call(Box::new(func_expr), args, Some(result_type.clone()), *span);
        }
    }

    Ok(())
}

fn ensure_lvalue(lvalue: &Expr, set_span: Span) -> Result<(), Error> {
    match lvalue {
        Expr::Field(expr, ..) => ensure_lvalue(expr, set_span)?,
        Expr::Var(..) | Expr::Op(Op::Deref | Op::SliceIndex, ..) => {}
        _ => return Err(Error::BadLValue(lvalue.clone(), set_span)),
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
