use std::{collections::HashMap, fs};

use crate::{
    ast::{Expr, Func, IntType, Op, Span, Struct, Type},
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
    struct Vec[t] {
        elems: []t
        length: U32

        func new(buf: []t): Vec[t] = Vec(buf, 0)
        func get(vec: Vec[t], index: U32): t = vec.elems[index]
        func push(vec: Ptr[Vec[t]], elem: t): Unit = {
            set vec*.elems[vec*.length] = elem;
            set vec*.length = vec*.length + 1;
        }
    }

    struct Main {
        func main(): U32 = {
            let buf = [10]U32 {};
            let vec = Vec.new(buf);
            5
        }
    }
    "#;

    // let source = r#"
    //     struct Main {
    //         func main(): I32 = if 1==2 {
    //             7 | 5
    //         } else {
    //             7 | 8
    //         }
    //     }
    // "#;

    let tokens = scan_program(source).unwrap();
    let mut ast = parse_program(source, &tokens).unwrap();

    let span: Span = (0..0).into();
    ast.push(Struct {
        name: "I32".into(),
        span,
        generics: Vec::new(),
        fields: OrdMap::new(),
        funcs: HashMap::new(),
    });
    ast.push(Struct {
        name: "U32".into(),
        span,
        generics: Vec::new(),
        fields: OrdMap::new(),
        funcs: HashMap::new(),
    });
    ast.push(Struct {
        name: "U8".into(),
        span,
        generics: Vec::new(),
        fields: OrdMap::new(),
        funcs: HashMap::new(),
    });
    ast.push(Struct {
        name: "Unit".into(),
        span,
        generics: Vec::new(),
        fields: OrdMap::new(),
        funcs: HashMap::new(),
    });
    ast.push(Struct {
        name: "Slice".into(),
        span,
        generics: vec!["t".into()],
        fields: [
            ("ptr".into(), Type::ptr(Type::generic("t", span), span)),
            ("length".into(), IntType::usize().to_type(span)),
        ]
        .into_iter()
        .collect(),
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
