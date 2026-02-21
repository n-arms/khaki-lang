use std::{collections::HashMap, fs};

use crate::{
    ast::{Expr, Func, Op, Span, Struct, Type},
    derive::{derive_constructors, derive_cor_structs},
    emit::emit_program,
    lower::lower_program,
    ord_map::OrdMap,
    parser::{parse_program, scan_program},
    typing::type_program,
};

mod ast;
mod derive;
mod emit;
mod ir;
mod lower;
mod ord_map;
mod parser;
mod typing;

fn main() {
    let source = r#"
        struct SpiConfig {
            baud_rate: Int
            clock_speed: Int

            func prj_baud(cfg: Ptr[SpiConfig]): Ptr[Int] = &cfg*.baud_rate

            func my_config(): SpiConfig = {
                let config = SpiConfig(0, 0);
                set config.baud_rate = 10;
                set config.clock_speed = 50;
                config
            }
        }
    "#;
    // func prj_baud_rate(cfg: Ptr[SpiConfig]): Ptr[Int] = &cfg*.baud_rate

    let tokens = scan_program(source).unwrap();
    let mut ast = parse_program(source, &tokens).unwrap();

    let span: Span = (0..0).into();
    let int_type = Type::int(span);
    ast.push(Struct {
        name: "Int".into(),
        span,
        generics: Vec::new(),
        fields: OrdMap::new(),
        funcs: HashMap::from([
            (
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
                        Some(int_type.clone()),
                        span,
                    ),
                },
            ),
            (
                String::from("less_than"),
                Func {
                    name: "less_than".into(),
                    is_cor: false,
                    args: vec![
                        ("a".into(), int_type.clone()),
                        ("b".into(), int_type.clone()),
                    ],
                    result: Type::bool(span),
                    body: Expr::Op(
                        Op::Builtin("int_less_than".into()),
                        vec![
                            Expr::Var("a".into(), Some(int_type.clone()), span),
                            Expr::Var("b".into(), Some(int_type.clone()), span),
                        ],
                        Some(Type::bool(span)),
                        span,
                    ),
                },
            ),
            (
                String::from("print"),
                Func {
                    name: "print".into(),
                    is_cor: false,
                    args: vec![("a".into(), int_type.clone())],
                    result: Type::unit(span),
                    body: Expr::Op(
                        Op::Builtin("int_print".into()),
                        vec![Expr::Var("a".into(), Some(int_type.clone()), span)],
                        Some(Type::unit(span)),
                        span,
                    ),
                },
            ),
        ]),
    });
    ast.push(Struct {
        name: "Unit".into(),
        span,
        generics: Vec::new(),
        fields: OrdMap::new(),
        funcs: HashMap::new(),
    });
    let bool_type = Type::bool(span);
    ast.push(Struct {
        name: "Bool".into(),
        span,
        generics: Vec::new(),
        fields: OrdMap::new(),
        funcs: HashMap::from([(
            "not".into(),
            Func {
                name: "not".into(),
                args: vec![("x".into(), bool_type.clone())],
                result: bool_type.clone(),
                is_cor: false,
                body: Expr::Op(
                    Op::Builtin("bool_not".into()),
                    vec![Expr::Var("x".into(), Some(bool_type.clone()), span)],
                    Some(bool_type),
                    span,
                ),
            },
        )]),
    });
    let ptr = Type::named("Ptr".into(), vec![Type::generic("t", span)], span);
    ast.push(Struct {
        name: "Ptr".into(),
        span,
        generics: vec!["t".into()],
        fields: OrdMap::new(),
        funcs: HashMap::from([
            (
                String::from("load"),
                Func {
                    name: "load".into(),
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
                String::from("store"),
                Func {
                    name: "store".into(),
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

    derive_constructors(&mut ast);
    let cor_structs = derive_cor_structs(&mut ast);
    dbg!(&ast);
    let typed = type_program(&ast).unwrap();
    dbg!(&typed);
    let lowered = lower_program(&typed);
    dbg!(&lowered);
    let llvm = emit_program(&lowered, &cor_structs);
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
