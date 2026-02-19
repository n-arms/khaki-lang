use std::collections::HashMap;

use crate::{
    ast::{Expr, Literal, Op, Stmt, Struct},
    typing::Spec,
};

#[derive(Clone, Debug)]
pub enum Value {
    Int(i64),
    Bool(bool),
    Unit,
    Struct(HashMap<String, Value>),
    Func(Spec, String),
}

pub struct Prog {
    pub structs: HashMap<Spec, Struct>,
    pub builtins: HashMap<String, fn(Vec<Value>) -> Value>,
}

pub type Env = HashMap<String, Value>;

pub fn eval(expr: &Expr, env: &Env, prog: &Prog) -> Value {
    match expr {
        Expr::Var(name, ..) => env[name].clone(),
        Expr::Func(struct_name, func_name, meta, ..) => {
            let (_, generics) = meta.clone().unwrap();
            let struct_spec = Spec {
                struct_name: struct_name.clone(),
                generics,
            };
            Value::Func(struct_spec, func_name.clone())
        }
        Expr::Literal(literal, _) => match literal {
            Literal::Bool(bool, _) => Value::Bool(*bool),
            Literal::Number(num, _) => Value::Int(num.parse().unwrap()),
            Literal::Unit(span) => todo!(),
        },
        Expr::Op(op, args, ..) => {
            let arg_vals: Vec<_> = args.iter().map(|arg| eval(arg, env, prog)).collect();
            match op {
                Op::Builtin(builtin) => {
                    let builtin = &prog.builtins[builtin];
                    builtin(arg_vals)
                }
                Op::Yield => todo!(),
                Op::Await => todo!(),
            }
        }
        Expr::Call(func, args, ..) => {
            let func = eval(func, env, prog);
            let arg_vals: Vec<_> = args.iter().map(|arg| eval(arg, env, prog)).collect();

            let Value::Func(spec, func_name) = func else {
                panic!("Attempt to call {func:?}");
            };

            let strukt = &prog.structs[&spec];
            let func = &strukt.funcs[&func_name];

            let mut inner = Env::default();
            for ((name, _), value) in func.args.iter().zip(arg_vals) {
                inner.insert(name.clone(), value);
            }
            eval(&func.body, &inner, prog)
        }
        Expr::Block(stmts, result) => {
            let mut env = env.clone();

            for stmt in stmts {
                match stmt {
                    Stmt::Let(var, expr) => {
                        env.insert(var.clone(), eval(expr, &env, prog));
                    }
                }
            }

            eval(result, &env, prog)
        }
    }
}
