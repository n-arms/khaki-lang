use std::collections::HashMap;

use crate::{
    ast::{self, IntType, Path, Span, Type, TypeKind},
    ir,
    lower::builder::FuncBuilder,
    typing::{env::Global, sub::Sub},
};

mod builder;

#[derive(Clone, Default)]
struct Env {
    vars: HashMap<String, ir::Slot>,
}

impl Env {
    fn get_var(&self, var: &str) -> &ir::Slot {
        &self.vars[var]
    }

    fn set_var(&mut self, var: String, slot: ir::Slot) {
        self.vars.insert(var, slot);
    }
}

pub fn lower_module(module: &ast::Module, global: &Global) -> ir::Module {
    ir::Module {
        structs: module.structs.clone(),
        funcs: module
            .funcs
            .values()
            .map(|func| (func.name.clone(), lower_func(func, global)))
            .collect(),
    }
}

fn lower_func(func: &ast::Func, global: &Global) -> ir::Func {
    let mut fb = FuncBuilder::new();
    let mut env = Env::default();
    let mut args = Vec::new();
    for (name, typ) in &func.args {
        let witness = lower_witness(typ, &mut fb, &env, &global);
        let slot = fb.slot(typ.clone(), witness);
        env.set_var(name.clone(), slot.clone());
        args.push(slot);
    }
    let result_slot = lower_expr(&func.body, &mut fb, &env, global);
    fb.end_block(ir::End::Return(result_slot, func.result.span));
    fb.finish(func.name.clone(), func.is_cor, args, func.result.clone())
}

fn lower_expr(expr: &ast::Expr, fb: &mut FuncBuilder, env: &Env, global: &Global) -> ir::Slot {
    let result = expr.get_type();
    let result_witness = lower_witness(&result, fb, env, global);
    match expr {
        ast::Expr::Var(name, _, span) => fb.instr(
            result.clone(),
            result_witness,
            ir::Value::Slot,
            vec![env.get_var(name).clone()],
            *span,
        ),
        ast::Expr::Func(path, func_name, _, span) => fb.instr(
            result,
            result_witness,
            ir::Value::Func(path.clone(), func_name.clone()),
            vec![],
            *span,
        ),
        ast::Expr::Literal(literal, _) => fb.instr(
            result,
            result_witness,
            ir::Value::Literal(literal.clone()),
            vec![],
            literal.span(),
        ),
        ast::Expr::Op(op, args, _, span) => {
            if op == &ast::Op::Ref {
                return lower_lvalue_ref(&args[0], fb, env, global);
            }
            if op == &ast::Op::If {
                let cond_val = lower_expr(&args[0], fb, env, global);
                let then_branch = fb.create_block();
                let else_branch = fb.create_block();
                let finally_branch = fb.create_block();
                fb.end_block(ir::End::JumpIf {
                    slot: cond_val,
                    then_branch,
                    else_branch,
                    span: *span,
                });
                fb.start_block(then_branch);
                let result_slot = lower_expr(&args[1], fb, env, global);
                fb.end_block(ir::End::Jump(finally_branch, *span));
                fb.start_block(else_branch);
                let temp = lower_expr(&args[2], fb, env, global);
                fb.push(ir::Instr {
                    result: result_slot.clone(),
                    value: ir::Value::Slot,
                    args: vec![temp],
                    span: *span,
                });
                fb.end_block(ir::End::Jump(finally_branch, *span));
                fb.start_block(finally_branch);

                return result_slot;
            }
            if op == &ast::Op::While {
                let cond_branch = fb.create_block();
                let body_branch = fb.create_block();
                let done_branch = fb.create_block();
                fb.end_block(ir::End::Jump(cond_branch, *span));
                fb.start_block(cond_branch);
                let cond_val = lower_expr(&args[0], fb, env, global);
                fb.end_block(ir::End::JumpIf {
                    slot: cond_val,
                    then_branch: body_branch,
                    else_branch: done_branch,
                    span: *span,
                });
                fb.start_block(body_branch);
                lower_expr(&args[1], fb, env, global);
                fb.end_block(ir::End::Jump(cond_branch, *span));
                fb.start_block(done_branch);

                return fb.instr(
                    result,
                    result_witness,
                    ir::Value::Literal(ast::Literal::Unit(*span)),
                    vec![],
                    *span,
                );
            }
            let arg_vals: Vec<_> = args
                .iter()
                .map(|arg| lower_expr(arg, fb, env, global))
                .collect();
            match op {
                ast::Op::Builtin(builtin) => fb.instr(
                    result,
                    result_witness,
                    ir::Value::Op(ir::Op::Builtin(builtin.clone())),
                    arg_vals,
                    *span,
                ),
                ast::Op::Await => {
                    let result = fb.slot(result, result_witness);
                    let await_branch = fb.create_block();
                    let then_branch = fb.create_block();
                    fb.end_block(ir::End::Jump(await_branch, *span));
                    fb.start_block(await_branch);
                    fb.end_block(ir::End::Await {
                        cor_struct: arg_vals[0].clone(),
                        result: result.clone(),
                        then_branch,
                        span: *span,
                    });
                    fb.start_block(then_branch);
                    result
                }
                ast::Op::Yield => {
                    let then_branch = fb.create_block();
                    fb.end_block(ir::End::Yield(then_branch, *span));
                    fb.start_block(then_branch);
                    fb.instr(
                        result,
                        result_witness,
                        ir::Value::Literal(ast::Literal::Unit(*span)),
                        vec![],
                        *span,
                    )
                }
                ast::Op::Constructor(..) => {
                    let TypeKind::Named(path, struct_name) = result.kind.clone() else {
                        unreachable!();
                    };
                    fb.instr(
                        result,
                        result_witness,
                        ir::Value::PackStruct(path, struct_name),
                        arg_vals,
                        *span,
                    )
                }
                ast::Op::Deref => fb.instr(
                    result,
                    result_witness,
                    ir::Value::Op(ir::Op::Builtin("ptr_get".into())),
                    arg_vals,
                    *span,
                ),
                ast::Op::SliceIndex => {
                    let ptr_witness =
                        lower_witness(&Type::ptr(result.clone(), *span), fb, env, global);
                    let size_witness =
                        lower_witness(&Type::int(IntType::usize(), *span), fb, env, global);
                    let backing_ptr = fb.instr(
                        Type::ptr(Type::ptr(result.clone(), *span), *span),
                        ptr_witness.clone(),
                        ir::Value::FieldGet(0, vec![ptr_witness.clone(), size_witness]),
                        vec![arg_vals[0].clone()],
                        *span,
                    );
                    let elem_witness = lower_witness(&result, fb, env, global);
                    let elem_ptr = fb.instr(
                        Type::ptr(result.clone(), *span),
                        ptr_witness.clone(),
                        ir::Value::IndexRef(elem_witness),
                        vec![backing_ptr, arg_vals[1].clone()],
                        *span,
                    );
                    fb.instr(
                        result,
                        result_witness,
                        ir::Value::Op(ir::Op::Builtin("ptr_get".into())),
                        vec![elem_ptr],
                        *span,
                    )
                }
                ast::Op::If | ast::Op::While | ast::Op::Ref => unreachable!(),
                ast::Op::Arith(arith) => fb.instr(
                    result,
                    result_witness,
                    ir::Value::Op(ir::Op::Arith(arith.clone())),
                    arg_vals,
                    *span,
                ),
                ast::Op::Cmp(cmp) => fb.instr(
                    result,
                    result_witness,
                    ir::Value::Op(ir::Op::Cmp(cmp.clone())),
                    arg_vals,
                    *span,
                ),
                ast::Op::Logic(logic) => fb.instr(
                    result,
                    result_witness,
                    ir::Value::Op(ir::Op::Logic(logic.clone())),
                    arg_vals,
                    *span,
                ),
                ast::Op::Open(_) => {
                    todo!()
                }
            }
        }
        ast::Expr::Call(func, args, _, span) => {
            let func_val = lower_expr(func, fb, env, global);
            let arg_vals: Vec<_> = args
                .iter()
                .map(|arg| lower_expr(arg, fb, env, global))
                .collect();

            let mut instr_args = vec![func_val];
            instr_args.extend(arg_vals);

            fb.instr(result, result_witness, ir::Value::Call, instr_args, *span)
        }
        ast::Expr::Block(stmts, expr, span) => {
            let mut inner = env.clone();
            for stmt in stmts {
                match stmt {
                    ast::Stmt::Let(var, value) => {
                        let slot = lower_expr(value, fb, &inner, global);
                        inner.set_var(var.clone(), slot);
                    }
                    ast::Stmt::Set(lval, value) => {
                        let val_slot = lower_expr(value, fb, &inner, global);
                        let lvalue_ptr_slot = lower_lvalue_ref(lval, fb, &inner, global);
                        fb.instr(
                            Type::unit(*span),
                            unit_witness(),
                            ir::Value::Op(ir::Op::Builtin("ptr_set".into())),
                            vec![lvalue_ptr_slot, val_slot],
                            *span,
                        );
                    }
                    ast::Stmt::Expr(value) => {
                        lower_expr(value, fb, &inner, global);
                    }
                }
            }
            if let Some(expr) = expr {
                lower_expr(expr, fb, &inner, global)
            } else {
                fb.instr(
                    result,
                    result_witness,
                    ir::Value::Literal(ast::Literal::Unit(*span)),
                    vec![],
                    *span,
                )
            }
        }
        ast::Expr::Field(expr, _, meta, span) => {
            let (_, field_index) = meta.clone().unwrap();

            let expr_val = lower_expr(expr, fb, env, global);

            let struct_type = expr_val.1.clone();
            let TypeKind::Named(struct_path, struct_name) = &struct_type.kind else {
                unreachable!();
            };
            let field_witnesses = field_witnesses(
                &struct_type.children,
                struct_path,
                struct_name,
                fb,
                env,
                global,
                *span,
            );
            fb.instr(
                result.clone(),
                result_witness,
                ir::Value::FieldGet(field_index, field_witnesses),
                vec![expr_val],
                *span,
            )
        }
        ast::Expr::Array(size, elems, elem_type, span) => {
            let elem_slots = elems
                .iter()
                .flatten()
                .map(|elem| lower_expr(elem, fb, env, global))
                .collect();
            let elem_type = elem_type.clone().unwrap();
            let array_type = Type {
                kind: TypeKind::Array(*size),
                span: *span,
                children: vec![elem_type.clone()],
            };
            let array_witness = lower_witness(&array_type, fb, env, global);
            let elem_witness = lower_witness(&elem_type, fb, env, global);
            let array = fb.instr(
                array_type.clone(),
                array_witness,
                ir::Value::Array(*size, elem_witness),
                elem_slots,
                *span,
            );
            let array_ptr = fb.instr(
                Type::ptr(elem_type.clone(), *span),
                pointer_witness(),
                ir::Value::RefArray,
                vec![array],
                *span,
            );
            let array_size = fb.instr(
                Type::int(IntType::usize(), *span),
                integer_witness(&IntType::usize()),
                ir::Value::Literal(ast::Literal::Number(size.to_string(), *span)),
                vec![],
                *span,
            );
            let slice = fb.instr(
                result,
                result_witness,
                ir::Value::PackStruct(Path::new(vec![], *span), "Slice".into()),
                vec![array_ptr, array_size],
                *span,
            );
            slice
        }
        ast::Expr::Any(expr, any_meta, span) => {
            let expr_val = lower_expr(expr, fb, env, global);
            let any_meta = any_meta.as_ref().unwrap();
            let t_witness = match lower_witness(&any_meta.existential, fb, env, global) {
                ir::Witness::Dynamic(slot) => *slot,
                ir::Witness::Static { size, align } => {
                    let size_val = fb.instr(
                        Type::int(IntType::usize(), *span),
                        integer_witness(&IntType::usize()),
                        ir::Value::Literal(ast::Literal::Number(size.to_string(), *span)),
                        vec![],
                        *span,
                    );
                    let align_val = fb.instr(
                        Type::int(IntType::usize(), *span),
                        integer_witness(&IntType::usize()),
                        ir::Value::Literal(ast::Literal::Number(align.to_string(), *span)),
                        vec![],
                        *span,
                    );
                    fb.instr(
                        witness_type(*span),
                        witness_witness(),
                        ir::Value::PackStruct(Path::new(vec![], *span), "Witness".into()),
                        vec![size_val, align_val],
                        *span,
                    )
                }
            };
            fb.instr(
                result.clone(),
                result_witness,
                ir::Value::PackStruct(Path::new(vec![], *span), "Any".into()),
                vec![t_witness, expr_val],
                *span,
            )
        }
    }
}

fn witness_type(span: Span) -> Type {
    Type::named(Path::new(vec![], span), "Witness".into(), vec![], span)
}

fn unit_witness() -> ir::Witness {
    ir::Witness::Static { size: 0, align: 0 }
}

fn pointer_witness() -> ir::Witness {
    ir::Witness::Static { size: 8, align: 8 }
}

fn integer_witness(int_type: &IntType) -> ir::Witness {
    ir::Witness::Static {
        size: int_type.width(),
        align: int_type.width(),
    }
}

fn witness_witness() -> ir::Witness {
    ir::Witness::Static { size: 16, align: 8 }
}

pub fn generic_name(name: &str, id: usize) -> String {
    format!("{name}_{id}")
}

fn lower_witness(typ: &Type, fb: &mut FuncBuilder, env: &Env, global: &Global) -> ir::Witness {
    let span = typ.span;
    match &typ.kind {
        TypeKind::Func(..) => pointer_witness(),
        TypeKind::Any(..) => lower_witness(&typ.children[0], fb, env, global),
        TypeKind::Named(path, name) => {
            let field_witnesses =
                field_witnesses(&typ.children, path, name, fb, env, global, typ.span);
            struct_witness(field_witnesses, fb, env, global, span)
        }
        TypeKind::Primitive(prim) => match prim {
            ast::Prim::Int(int_type) => ir::Witness::Static {
                size: int_type.width(),
                align: int_type.width(),
            },
            ast::Prim::Bool => ir::Witness::Static { size: 1, align: 1 },
            ast::Prim::Unit => unit_witness(),
            ast::Prim::Ptr => pointer_witness(),
        },
        TypeKind::Unif(_) => unreachable!(),
        TypeKind::Generic(name, id) => {
            ir::Witness::Dynamic(Box::new(env.get_var(&generic_name(name, *id)).clone()))
        }
        TypeKind::Array(count) => {
            let elem_witness = lower_witness(&typ.children[0], fb, env, global);
            match elem_witness {
                ir::Witness::Static { size, align } => ir::Witness::Static {
                    size: size * count,
                    align,
                },
                ir::Witness::Dynamic(slot) => {
                    let usize_type = Type::int(IntType::usize(), span);
                    let usize_witness = integer_witness(&IntType::usize());
                    let witnesses = vec![usize_witness.clone(), usize_witness.clone()];

                    let size = fb.instr(
                        usize_type.clone(),
                        usize_witness.clone(),
                        ir::Value::FieldGet(0, witnesses.clone()),
                        vec![slot.as_ref().clone()],
                        span,
                    );
                    let align = fb.instr(
                        usize_type.clone(),
                        usize_witness.clone(),
                        ir::Value::FieldGet(1, witnesses),
                        vec![slot.as_ref().clone()],
                        span,
                    );
                    let count = fb.instr(
                        usize_type.clone(),
                        usize_witness.clone(),
                        ir::Value::Literal(ast::Literal::Number(count.to_string(), span)),
                        vec![],
                        span,
                    );
                    let total_size = fb.instr(
                        usize_type.clone(),
                        usize_witness.clone(),
                        ir::Value::Op(ir::Op::Arith(ast::Arith::Mul)),
                        vec![size, count],
                        span,
                    );
                    let witness_slot = fb.instr(
                        witness_type(span),
                        witness_witness(),
                        ir::Value::PackStruct(Path::new(vec![], span), "".into()),
                        vec![total_size, align],
                        span,
                    );
                    ir::Witness::Dynamic(Box::new(witness_slot))
                }
            }
        }
    }
}

fn struct_witness(
    mut fields: Vec<ir::Witness>,
    fb: &mut FuncBuilder,
    _env: &Env,
    _global: &Global,
    span: Span,
) -> ir::Witness {
    let mut total_size = 0;
    let mut max_align = 0;
    fields.retain(|witness| {
        if let ir::Witness::Static { size, align } = witness {
            total_size += size;
            max_align = max_align.max(*align);
            false
        } else {
            true
        }
    });
    if fields.is_empty() {
        ir::Witness::Static {
            size: total_size,
            align: max_align,
        }
    } else {
        let usize_witness = integer_witness(&IntType::usize());
        let _total_size = fb.instr(
            Type::int(IntType::usize(), span),
            usize_witness.clone(),
            ir::Value::Literal(ast::Literal::Number(total_size.to_string(), span)),
            vec![],
            span,
        );
        let _total_align = fb.instr(
            Type::int(IntType::usize(), span),
            usize_witness,
            ir::Value::Literal(ast::Literal::Number(max_align.to_string(), span)),
            vec![],
            span,
        );
        todo!();
    }
}

fn field_witnesses(
    children: &[Type],
    path: &ast::Path,
    name: &str,
    fb: &mut FuncBuilder,
    env: &Env,
    global: &Global,
    _span: Span,
) -> Vec<ir::Witness> {
    let strukt = global.get_struct(path, name).unwrap();
    let mut sub = Sub::default();
    for (name, typ) in strukt.generics.iter().zip(children) {
        sub.set_generic(name.clone(), typ.clone());
    }

    strukt
        .fields
        .values()
        .map(|field| {
            let mut field = field.clone();
            sub.typ(&mut field);

            lower_witness(&field, fb, env, global)
        })
        .collect()
}

// the resulting slot contains the address of the lvalue, enabling you to write to the lvalue with ptr_set
fn lower_lvalue_ref(
    expr: &ast::Expr,
    fb: &mut FuncBuilder,
    env: &Env,
    global: &Global,
) -> ir::Slot {
    let lvalue_type = expr.get_type();
    let span = lvalue_type.span;
    let result = Type::ptr(lvalue_type.clone(), span);
    let result_witness = lower_witness(&result, fb, env, global);
    match expr {
        ast::Expr::Var(name, _, span) => fb.instr(
            result,
            result_witness,
            ir::Value::Ref,
            vec![env.get_var(name).clone()],
            *span,
        ),
        ast::Expr::Op(ast::Op::Deref, args, ..) => lower_expr(&args[0], fb, env, global),
        ast::Expr::Field(expr, _, meta, span) => {
            let struct_type = expr.get_type();
            let TypeKind::Named(struct_path, struct_name) = &struct_type.kind else {
                unreachable!();
            };
            let field_witnesses = field_witnesses(
                &struct_type.children,
                struct_path,
                struct_name,
                fb,
                env,
                global,
                *span,
            );
            let (_, field_index) = meta.as_ref().unwrap();
            let container_ptr = lower_lvalue_ref(&expr, fb, env, global);
            fb.instr(
                result,
                result_witness,
                ir::Value::FieldRef(*field_index, field_witnesses),
                vec![container_ptr],
                *span,
            )
        }
        ast::Expr::Op(ast::Op::SliceIndex, args, meta, span) => {
            let slice = lower_expr(&args[0], fb, env, global);
            let index = lower_expr(&args[1], fb, env, global);
            let span = *span;
            let backing_ptr = fb.instr(
                Type::ptr(Type::ptr(lvalue_type.clone(), span), span),
                pointer_witness(),
                ir::Value::FieldGet(
                    0,
                    vec![pointer_witness(), integer_witness(&IntType::usize())],
                ),
                vec![slice],
                span,
            );
            let elem_type = meta.as_ref().unwrap();
            let elem_witness = lower_witness(elem_type, fb, env, global);
            fb.instr(
                result,
                result_witness,
                ir::Value::IndexRef(elem_witness),
                vec![backing_ptr, index],
                span,
            )
        }
        ast::Expr::Func(..)
        | ast::Expr::Op(..)
        | ast::Expr::Literal(..)
        | ast::Expr::Call(..)
        | ast::Expr::Array(..)
        | ast::Expr::Any(..)
        | ast::Expr::Block(..) => unreachable!(),
    }
}
