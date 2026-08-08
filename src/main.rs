use std::{collections::HashMap, fs, process::Command};

use crate::{
    ast::{Expr, Func, IntType, Op, Path, Span, Struct, Type},
    derive::{derive_constructors, derive_cor_structs},
    emit::{emit_module, emit_prelude},
    lower::{decor::decor, lower_module},
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

struct Config {
    /// Parse the input and display the parsed IR
    parse: bool,
    /// Type check the input and display the typed IR
    type_check: bool,
    /// Lower the input and display the lowered IR
    lower: bool,
    /// Emit the input as LLVM IR
    emit: bool,
}

impl Default for Config {
    fn default() -> Self {
        Self {
            parse: false,
            type_check: false,
            lower: false,
            emit: false,
        }
    }
}

fn compile_and_run(name: &str, source: &str, config: Config) -> i32 {
    let file_id = 0;
    let tokens = scan_program(source).unwrap();
    let mut module = parse_program(source, &tokens, file_id).unwrap();

    if config.parse {
        dbg!(module);
        return 0;
    }

    let span = Span::new(file_id, 0, 0);

    module.structs.insert(
        "Slice".into(),
        Struct {
            name: "Slice".into(),
            span,
            generics: vec!["t".into()],
            fields: [
                ("ptr".into(), Type::ptr(Type::generic("t", span), span)),
                ("length".into(), Type::int(IntType::usize(), span)),
            ]
            .into_iter()
            .collect(),
        },
    );

    module.structs.insert(
        "Witness".into(),
        Struct {
            name: "Witness".into(),
            span,
            generics: vec![],
            fields: [
                ("size".into(), Type::int(IntType::usize(), span)),
                ("align".into(), Type::int(IntType::usize(), span)),
            ]
            .into_iter()
            .collect(),
        },
    );

    module.funcs.insert(
        "ptr_cast".into(),
        Func {
            name: "ptr_cast".into(),
            generics: vec!["t".into(), "u".into()],
            args: vec![("x".into(), Type::ptr(Type::generic("t", span), span))],
            result: Type::ptr(Type::generic("u", span), span),
            is_cor: false,
            body: Expr::Op(
                Op::Builtin("transmute".into()),
                vec![Expr::Var("x".into(), None, span)],
                None,
                span,
            ),
        },
    );

    module.funcs.insert(
        "size_of".into(),
        Func {
            name: "size_of".into(),
            generics: vec!["t".into()],
            args: vec![("x".into(), Type::generic("t", span))],
            result: Type::int(IntType::usize(), span),
            is_cor: false,
            body: Expr::Field(
                Box::new(Expr::Op(
                    Op::WitnessOf,
                    vec![Expr::Var("x".into(), None, span)],
                    None,
                    span,
                )),
                "size".into(),
                None,
                span,
            ),
        },
    );

    let module_path = Path::new(vec![], span);
    derive_constructors(module_path.clone(), &mut module);
    let mut module_map = HashMap::from([(vec![], module)]);
    derive_cor_structs(module_path.clone(), &mut module_map);
    let global = type_program(&mut module_map).unwrap();
    if config.type_check {
        dbg!(&module_map);
        return 0;
    }
    let mut ir_map: HashMap<_, _> = module_map
        .iter()
        .map(|(path, module)| (path.clone(), lower_module(module, &global)))
        .collect();
    println!("Before decor: {:?}", &ir_map);
    decor(&mut ir_map, &global);
    println!("After decor: {:?}", ir_map);
    if config.lower {
        dbg!(ir_map);
        return 0;
    }
    let llvm_modules: Vec<_> = ir_map
        .iter()
        .map(|(path, module)| emit_module(path, module))
        .collect();
    let llvm = llvm_modules.join("\n") + "\n" + &emit_prelude();
    if config.emit {
        dbg!(llvm);
        return 0;
    }
    let exe_path = format!("./target/{name}");
    let file_path = format!("./target/{name}.ll");
    fs::write(&file_path, llvm).unwrap();
    let clang_output = Command::new("clang")
        .args(["-mllvm", "-opaque-pointers", &file_path, "-o", &exe_path])
        .output()
        .expect("Failed to run clang");
    assert!(clang_output.status.success());
    let output = Command::new(&exe_path)
        .output()
        .expect("Failed to run compiler exe");
    output
        .status
        .code()
        .expect("Compiled exe likely segfaulted")
}

fn main() {
    let source = r#"
    struct Pair[a, b] {
        a: a
        b: b
    }
    func first(p: Pair[x, y]): x = p.a
    func main(): I32 = 3
    "#;

    println!(
        "Exe result: {}",
        compile_and_run(
            "main",
            source,
            Config {
                ..Default::default()
            }
        )
    );
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn int_literal() {
        assert_eq!(
            compile_and_run("int_literal", "func main(): I32 = 3", Config::default()),
            3
        )
    }

    #[test]
    fn set_get_variable() {
        assert_eq!(
            compile_and_run(
                "set_get",
                r#"func main(): I32 = {
                let x = 5;
                set x = 6;
                x
            }"#,
                Config::default()
            ),
            6
        )
    }

    #[test]
    fn call_add1() {
        assert_eq!(
            compile_and_run(
                "add1",
                r#"
                func add1(x: I32): I32 = x + 1
                func main(): I32 = add1(5)
            "#,
                Config::default()
            ),
            6
        )
    }

    #[test]
    fn pack_unpack() {
        assert_eq!(
            compile_and_run(
                "pack_unpack",
                r#"
                struct Foo {
                    x: I32
                    y: Bool
                }
                func foo(): Foo = Foo(3, 3==4)
                func main(): I32 = foo().x
            "#,
                Config::default()
            ),
            3
        )
    }

    #[test]
    fn array_get() {
        assert_eq!(
            compile_and_run(
                "array_get",
                r#"
                func main(): I32 = {
                    let x = [] { 12, 13, 14 };
                    x[0]
                }
            "#,
                Config::default()
            ),
            12
        );
    }

    #[test]
    fn use_poll() {
        assert_eq!(
            compile_and_run(
                "use_poll",
                r#"
                cor foo(): I32 = 3
                func main(): I32 = {
                    let f = foo();
                    let x = 4;
                    foo::poll(f&, x&);
                    x
                }
                "#,
                Config::default()
            ),
            3
        );
    }

    #[test]
    fn poll_loop() {
        assert_eq!(
            compile_and_run(
                "poll_loop",
                r#"
                cor foo(): I32 = {
                    yield;
                    yield;
                    yield;
                    4
                }
                func main(): I32 = {
                    let i = 0;
                    let result = 0;
                    let f = foo();
                    while !foo::poll(f&, result&) {
                        set i = i + 1;
                    };
                    i + result
                }
                "#,
                Config::default()
            ),
            7
        )
    }

    #[test]
    fn simple_yield_await() {
        assert_eq!(
            compile_and_run(
                "simple_yield_await",
                r#"
                cor foo(): I32 = {
                    yield;
                    4
                }
                cor bar(): I32 = {
                    yield;
                    let f = foo()!;
                    yield;
                    f
                }
                func main(): I32 = {
                    let i = 0;
                    let result = 0;
                    let f = bar();
                    while !bar::poll(f&, result&) {
                        set i = i + 1;
                    };
                    i + result
                }
                "#,
                Config::default()
            ),
            7
        )
    }

    #[test]
    fn generic_pair_fields() {
        assert_eq!(
            compile_and_run(
                "generic_pair_fields",
                r#"
                struct Pair[a, b] {
                    a: a
                    b: b
                }
                func main(): I32 = {
                    let p = Pair(3, 4);
                    set p.a = p.b;
                    set p.b = p.a;
                    p.a + p.b
                }
                "#,
                Config::default()
            ),
            8
        )
    }

    #[test]
    fn i64() {
        assert_eq!(
            compile_and_run(
                "i64",
                r#"
                func main(): I64 = 7
                "#,
                Config::default()
            ),
            7
        )
    }

    #[test]
    fn push_vector() {
        assert_eq!(
            compile_and_run(
                "vector_push",
                r#"
                struct Vector[t] {
                    slice: []t
                    length: U64
                }

                func push[t](v: Vector[t]*, elem: t): U64 = {
                    let index = v*.length;
                    set v*.slice[index] = elem;
                    set v*.length = index + 1;
                    0
                }

                func main(): U64 = {
                    let v = Vector([5]U64 {}, 0);
                    push(v&, 3);
                    push(v&, 1);
                    push(v&, 4);
                    v.slice[1]
                }
                "#,
                Config::default()
            ),
            1
        )
    }

    #[test]
    fn transmute() {
        assert_eq!(
            compile_and_run(
                "transmute",
                r#"
                func main(): I8 = {
                    let x: U64 = 16909060;
                    let slice = Slice(ptr_cast(x&), 4);
                    slice[1]
                }
                "#,
                Config::default()
            ),
            3
        )
    }

    #[test]
    fn dynamic_rtti() {
        assert_eq!(
            compile_and_run(
                "dynamic_rtti",
                r#"
                struct Box[t] { inner: t }
                func main(): U64 = {
                    let b = Box(5);
                    size_of(b)
                }
                "#,
                Config::default()
            ),
            8
        )
    }
}
