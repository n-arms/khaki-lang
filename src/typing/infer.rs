use crate::{
    ast::{AnyMeta, Expr, IntType, Literal, Op, Path, Prim, Span, Stmt, Type, TypeKind},
    typing::{
        Error,
        env::{Global, Local, Scope},
        sub::Sub,
    },
};

pub fn check_expr(
    expr: &mut Expr,
    typ: &Type,
    global: &Global,
    local: &mut Local,
    scope: &Scope,
) -> Result<(), Error> {
    match (expr, &typ.kind) {
        (expr, TypeKind::Any(existential)) => {
            let span = expr.span();
            let existential_unif = local.fresh(span);
            let mut sub = Sub::default();
            sub.set_generic(existential.clone(), existential_unif.clone());
            let mut inner_type = typ.children[0].clone();
            sub.typ(&mut inner_type);
            check_expr(expr, &inner_type, global, local, scope)?;
            let meta = Some(AnyMeta {
                result: typ.clone(),
                existential: existential_unif.clone(),
            });
            *expr = Expr::Any(Box::new(expr.clone()), meta, span);
        }
        (expr, _) => {
            infer_expr(expr, global, local, scope)?;
            local.unify(typ.clone(), expr.get_type(), typ.span)?;
        }
    }
    Ok(())
}

pub fn infer_expr(
    expr: &mut Expr,
    global: &Global,
    local: &mut Local,
    scope: &Scope,
) -> Result<(), Error> {
    println!("infer {expr:?}");
    match expr {
        Expr::Var(name, typ, span) => {
            if let Ok(func) = global.get_func(&Path::new(vec![], *span), name) {
                let result_type = if func.is_cor {
                    let generics = func
                        .generics
                        .iter()
                        .map(|generic| Type::generic(generic, *span))
                        .collect();
                    Type::named(
                        Path::new(vec![name.clone()], *span),
                        name.clone(),
                        generics,
                        *span,
                    )
                } else {
                    func.result.clone()
                };
                let func_type =
                    Type::func(func.generics.clone(), func.arg_types(), result_type, *span);
                *expr = Expr::Func(
                    Path::new(vec![], *span),
                    name.clone(),
                    Some(func_type),
                    *span,
                );
            } else {
                *typ = Some(scope.get_var(name, *span)?.clone());
            }
        }
        Expr::Func(path, func_name, meta, span) => {
            let func = global.get_func(path, func_name)?;
            let result_type = if func.is_cor {
                let generics = func
                    .generics
                    .iter()
                    .map(|generic| Type::generic(generic, *span))
                    .collect();
                Type::named(path.clone(), func_name.clone(), generics, *span)
            } else {
                func.result.clone()
            };
            let func_type = Type::func(func.generics.clone(), func.arg_types(), result_type, *span);
            *meta = Some(func_type);
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
                    let TypeKind::Named(path, name) = args[0].get_type().kind else {
                        let span = *span;
                        return Err(Error::NeedsTypeAnnotation(expr.clone(), span));
                    };
                    let cor_result = global.get_cor(&path.popped(), &name)?;
                    let mut sub = Sub::default();
                    for (name, typ) in cor_result.generics.iter().zip(args[0].get_type().children) {
                        sub.set_generic(name.clone(), typ.clone());
                    }
                    let mut cor_type = cor_result.result.clone();
                    sub.typ(&mut cor_type);
                    cor_type
                }
                Op::Yield => {
                    if !local.is_cor() {
                        return Err(Error::YieldOutsideCor(*span));
                    }
                    Type::unit(*span)
                }
                Op::Ref => {
                    ensure_lvalue(&args[0], *span)?;
                    Type::ptr(args[0].get_type(), *span)
                }
                Op::Deref => {
                    let arg_type = args[0].get_type();
                    let TypeKind::Primitive(Prim::Ptr) = &arg_type.kind else {
                        let span = *span;
                        return Err(Error::NeedsTypeAnnotation(expr.clone(), span));
                    };

                    arg_type.children[0].clone()
                }
                Op::If => {
                    local.unify(Type::bool(*span), args[0].get_type(), *span)?;
                    local.unify(args[1].get_type(), args[2].get_type(), *span)?;
                    args[1].get_type()
                }
                Op::While => {
                    local.unify(Type::bool(*span), args[0].get_type(), *span)?;
                    Type::unit(*span)
                }
                Op::Constructor(..) => local.fresh(*span),
                Op::SliceIndex => {
                    let result = local.fresh(*span);
                    let arg_type = args[0].get_type();
                    local.unify(
                        args[1].get_type(),
                        Type::int(IntType::usize(), *span),
                        *span,
                    )?;
                    if let TypeKind::Named(path, name) = &arg_type.kind {
                        if path.path.is_empty() && name == "Slice" {
                            *typ = Some(arg_type.children[0].clone());
                            return Ok(());
                        }
                    }
                    local.unify(arg_type, Type::slice(result.clone(), *span), *span)?;
                    result
                }
                Op::Arith(_) => {
                    local.unify(args[0].get_type(), args[1].get_type(), *span)?;
                    local.unify_int(args[0].get_type(), *span);
                    args[0].get_type()
                }
                Op::Cmp(_) => {
                    local.unify(args[0].get_type(), args[1].get_type(), *span)?;
                    local.unify_int(args[0].get_type(), *span);
                    // TODO: support comparisons for things that aren't integers
                    Type::bool(*span)
                }
                Op::Logic(_) => {
                    local.unify(args[0].get_type(), Type::bool(*span), *span)?;
                    local.unify(args[1].get_type(), Type::bool(*span), *span)?;
                    Type::bool(*span)
                }
                Op::Open(meta) => {
                    let mut inner_type = args[0].get_type();
                    let TypeKind::Any(name) = inner_type.kind.clone() else {
                        let span = *span;
                        return Err(Error::BadOpen(expr.clone(), span));
                    };
                    let skolemized = local.skolem(name.clone(), *span);
                    let TypeKind::Generic(_, id) = &skolemized.kind else {
                        unreachable!()
                    };
                    let id = *id;
                    let mut sub = Sub::default();
                    sub.set_generic(name.clone(), skolemized);
                    sub.typ(&mut inner_type);
                    *meta = Some((name, id));
                    inner_type
                }
            });
        }
        Expr::Call(func, args, meta, span) => {
            infer_expr(func, global, local, scope)?;

            let func_type = func.get_type();

            let TypeKind::Func(generic_names) = &func_type.kind else {
                let span = *span;
                return Err(Error::NeedsTypeAnnotation(expr.clone(), span));
            };

            let mut sub = Sub::default();
            for name in generic_names {
                sub.set_generic(name.clone(), local.fresh(*span));
            }

            for (mut expected, arg) in func_type.children.iter().cloned().zip(args.iter_mut()) {
                sub.typ(&mut expected);
                check_expr(arg, &expected, global, local, scope)?;
            }
            let mut result_type = func_type.children.last().unwrap().clone();
            sub.typ(&mut result_type);

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
                        local.unify(lval.get_type(), expr_type, span)?;
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
            let TypeKind::Named(struct_path, struct_name) = &struct_type.kind else {
                return Err(Error::NeedsTypeAnnotation(
                    struct_expr.as_ref().clone(),
                    *span,
                ));
            };
            let strukt = global.get_struct(struct_path, struct_name)?;
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
                    local.unify(a.get_type(), b.get_type(), *span)?;
                }
                if let Some(elem_type) = elem_type {
                    if let Some(first) = elems.first() {
                        local.unify(elem_type.clone(), first.get_type(), *span)?;
                    }
                } else {
                    if let Some(first) = elems.first() {
                        *elem_type = Some(first.get_type());
                    } else {
                        let span = *span;
                        return Err(Error::NeedsTypeAnnotation(expr.clone(), span));
                    }
                }
            } else {
                if elem_type.is_none() {
                    *elem_type = Some(local.fresh(*span));
                }
            }
        }
        Expr::Any(..) => {
            unreachable!()
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
