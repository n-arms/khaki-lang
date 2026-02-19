use std::collections::HashMap;

use crate::{
    ast::{self, Type},
    ir,
    lower::builder::FuncBuilder,
    typing::Spec,
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

pub fn lower_program(prog: &HashMap<Spec, ast::Struct>) -> HashMap<Spec, ir::Struct> {
    prog.iter()
        .map(|(spec, strukt)| (spec.clone(), lower_struct(strukt, spec.generics.clone())))
        .collect()
}

fn lower_struct(strukt: &ast::Struct, generics: Vec<Type>) -> ir::Struct {
    let mut funcs = HashMap::new();

    for func in strukt.funcs.values() {
        funcs.insert(func.name.clone(), lower_func(func));
    }

    let spec = Spec {
        struct_name: strukt.name.clone(),
        generics,
    };

    ir::Struct {
        name: spec,
        fields: strukt.fields.clone(),
        funcs,
    }
}

fn lower_func(func: &ast::Func) -> ir::Func {
    let mut fb = FuncBuilder::new();
    let mut env = Env::default();
    let mut args = Vec::new();
    for (name, typ) in &func.args {
        let slot = fb.slot(typ.clone());
        env.set_var(name.clone(), slot.clone());
        args.push(slot);
    }
    let result_slot = lower_expr(&func.body, &mut fb, &env);
    fb.end_block(ir::End::Return(result_slot, func.result.span));
    fb.finish(func.name.clone(), args, func.result.clone())
}

fn lower_expr(expr: &ast::Expr, fb: &mut FuncBuilder, env: &Env) -> ir::Slot {
    let result = expr.get_type();
    match expr {
        ast::Expr::Var(name, _, span) => fb.instr(
            result.clone(),
            ir::Value::Slot,
            vec![env.get_var(name).clone()],
            *span,
        ),
        ast::Expr::Func(struct_name, func_name, meta, span) => {
            let spec = Spec {
                struct_name: struct_name.clone(),
                generics: meta.clone().unwrap().1,
            };
            fb.instr(
                result,
                ir::Value::Func(spec, func_name.clone()),
                vec![],
                *span,
            )
        }
        ast::Expr::Literal(literal, _) => fb.instr(
            result,
            ir::Value::Literal(literal.clone()),
            vec![],
            literal.span(),
        ),
        ast::Expr::Op(op, args, _, span) => {
            let arg_vals: Vec<_> = args.iter().map(|arg| lower_expr(arg, fb, env)).collect();
            match op {
                ast::Op::Builtin(builtin) => fb.instr(
                    result,
                    ir::Value::Op(ir::Op::Builtin(builtin.clone())),
                    arg_vals,
                    *span,
                ),
                ast::Op::Await => {
                    let result = fb.slot(result);
                    let then_branch = fb.create_block();
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
                        ir::Value::Literal(ast::Literal::Unit(*span)),
                        vec![],
                        *span,
                    )
                }
            }
        }
        ast::Expr::Call(func, args, _, span) => {
            let func_val = lower_expr(func, fb, env);
            let arg_vals: Vec<_> = args.iter().map(|arg| lower_expr(arg, fb, env)).collect();

            let mut instr_args = vec![func_val];
            instr_args.extend(arg_vals);

            fb.instr(result, ir::Value::Call, instr_args, *span)
        }
        ast::Expr::Block(stmts, expr) => {
            let mut inner = env.clone();
            for stmt in stmts {
                match stmt {
                    ast::Stmt::Let(var, value) => {
                        let slot = lower_expr(value, fb, &inner);
                        inner.set_var(var.clone(), slot);
                    }
                }
            }
            lower_expr(expr, fb, &inner)
        }
    }
}
