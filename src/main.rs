use std::{collections::HashMap, fs};

use crate::{
    ast::{Expr, Func, Op, Span, Struct, Type},
    derive::derive,
    emit::emit_program,
    lower::lower_program,
    parser::{parse_program, scan_program},
    typing::type_program,
};

mod ast;
mod derive;
mod emit;
mod eval;
mod ir;
mod lower;
mod parser;
mod typing;

fn main() {
    // let source = r#"
    //     struct Main {
    //         cor foo(): Int = {
    //             let x = 5;
    //             let _ = yield;
    //             x
    //         }
    //         cor bar(): Int = {
    //             let a = Main.foo()!;
    //             let _ = yield;
    //             a
    //         }
    //     }
    // "#;

    let source = r#"
        struct Main {
            func double(x: Int): Int = {
                Int.add(x, x)
            }
        }
    "#;

    let tokens = scan_program(source).unwrap();
    let mut ast = parse_program(source, &tokens).unwrap();

    let span: Span = (0..0).into();
    let int_type = Type::int(span);
    ast.push(Struct {
        name: "Int".into(),
        generics: Vec::new(),
        fields: HashMap::new(),
        funcs: HashMap::from([((
            String::from("add"),
            Func {
                name: "add".into(),
                is_cor: false,
                args: vec![
                    ("a".into(), int_type.clone()),
                    ("b".into(), int_type.clone()),
                ],
                result: int_type.clone(),
                body: Expr::Op(
                    Op::Builtin("int_add".into()),
                    vec![
                        Expr::Var("a".into(), Some(int_type.clone()), span),
                        Expr::Var("b".into(), Some(int_type.clone()), span),
                    ],
                    Some(int_type),
                    span,
                ),
            },
        ))]),
    });
    ast.push(Struct {
        name: "Unit".into(),
        generics: Vec::new(),
        fields: HashMap::new(),
        funcs: HashMap::new(),
    });
    ast.push(Struct {
        name: "Bool".into(),
        generics: Vec::new(),
        fields: HashMap::new(),
        funcs: HashMap::new(),
    });
    let ptr = Type::named("Ptr".into(), vec![Type::generic("t", span)], span);
    ast.push(Struct {
        name: "Ptr".into(),
        generics: vec!["t".into()],
        fields: HashMap::new(),
        funcs: HashMap::from([
            (
                String::from("get"),
                Func {
                    name: "get".into(),
                    is_cor: false,
                    args: vec![("p".into(), ptr.clone())],
                    result: Type::generic("t", span),
                    body: Expr::Op(
                        Op::Builtin("ptr_get".into()),
                        vec![Expr::Var("p".into(), Some(ptr.clone()), span)],
                        Some(Type::generic("t", span)),
                        span,
                    ),
                },
            ),
            (
                String::from("set"),
                Func {
                    name: "set".into(),
                    is_cor: false,
                    args: vec![
                        ("p".into(), ptr.clone()),
                        ("x".into(), Type::generic("t", span)),
                    ],
                    result: Type::unit(span),
                    body: Expr::Op(
                        Op::Builtin("ptr_set".into()),
                        vec![
                            Expr::Var("p".into(), Some(ptr.clone()), span),
                            Expr::Var("x".into(), Some(Type::generic("t", span)), span),
                        ],
                        Some(Type::generic("t", span)),
                        span,
                    ),
                },
            ),
        ]),
    });

    derive(&mut ast);
    dbg!(&ast);
    let typed = type_program(&ast).unwrap();
    dbg!(&typed);
    let lowered = lower_program(&typed);
    dbg!(&lowered);
    let llvm = emit_program(&lowered);
    fs::write("out.ll", llvm).unwrap();

    // let prog = Prog {
    //     structs: typed,
    //     builtins: HashMap::from([(
    //         String::from("int_add"),
    //         (|elems: Vec<Value>| {
    //             let [Value::Int(a), Value::Int(b)] = elems.as_slice() else {
    //                 panic!("Bad args: {elems:?}");
    //             };
    //             Value::Int(a + b)
    //         }) as fn(Vec<Value>) -> Value,
    //     )]),
    // };

    // let main = &prog.structs[&Spec {
    //     struct_name: "Main".into(),
    //     generics: Vec::new(),
    // }]
    //     .funcs["main"];

    // dbg!(eval(&main.body, &Env::default(), &prog));
}
