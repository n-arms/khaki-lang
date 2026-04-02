use std::{collections::HashMap, fs};

use crate::{
    ast::{Expr, Func, FuncSpec, IntType, Op, Span, Struct, Type},
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
    // let source = r#"
    // struct Vec[t] {
    //     elems: []t
    //     length: U32

    //     func new(buf: []t): Vec[t] = Vec(buf, 0)
    //     func get(vec: Vec[t], index: U32): Ptr[t] = vec.elems[index]&
    //     func push(vec: Ptr[Vec[t]], elem: t): Unit = {
    //         set vec*.elems[vec*.length] = elem;
    //         set vec*.length = vec*.length + 1;
    //     }
    // }

    // struct Main {
    //     func main(): U32 = {
    //         let buf = [10]U32 {};
    //         let vec = Vec.new(buf);
    //         vec.push(5);
    //         vec.get(0)*
    //     }
    // }
    // "#;

    let source = r#"
    struct Foo {
        func bar[t](x: t): t = x
    }

    struct Main {
        func main(): U32 = {
            Foo.bar(3)
        }
    }
    "#;

    let file_id = 0;
    let tokens = scan_program(source).unwrap();
    let mut ast = parse_program(source, &tokens, file_id).unwrap();

    let span: Span = Span::new(file_id, 0, 0);
    let i32_type = IntType::signed(32).to_type(span);
    ast.push(Struct {
        name: "I32".into(),
        span,
        generics: Vec::new(),
        fields: OrdMap::new(),
        funcs: HashMap::from([(
            FuncSpec::named("print".into()),
            Func {
                name: "print".into(),
                generics: Vec::new(),
                args: vec![("self".into(), i32_type)],
                result: Type::unit(span),
                is_cor: false,
                body: Expr::Op(
                    Op::Builtin("int_print".into()),
                    vec![Expr::Var("self".into(), None, span)],
                    None,
                    span,
                ),
            },
        )]),
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
            FuncSpec::named("not".into()),
            Func {
                name: "not".into(),
                generics: Vec::new(),
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
                FuncSpec::named("load".into()),
                Func {
                    name: "load".into(),
                    generics: Vec::new(),
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
                FuncSpec::named("store".into()),
                Func {
                    name: "store".into(),
                    generics: Vec::new(),
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
    //let lowered = lower_program(&typed);
    //dbg!(&lowered);
    //let llvm = emit_program(&lowered, &cor_structs);
    //fs::write("out.ll", llvm).unwrap();
}
