use std::{collections::HashMap, fs, process::Command};

use crate::{
    ast::{Expr, Func, IntType, Op, Path, Span, Struct, Type},
    derive::{derive_constructors, derive_cor_structs},
    emit::emit_program,
    lower::lower_module,
    ord_map::OrdMap,
    parser::{parse_program, scan_program},
    typing::type_program,
};

use rand::{distr::{SampleString, slice::Choose}, prelude::*};

mod ast;
mod derive;
mod emit;
mod ir;
mod lower;
mod ord_map;
mod parser;
mod typing;

fn compile_and_run(
    source: &str
) -> i32 {
    let file_id = 0;
    let tokens = scan_program(source).unwrap();
    let mut module = parse_program(source, &tokens, file_id).unwrap();

    let span = Span::new(file_id, 0, 0);


    // module.structs.insert(
    //     "Slice".into(),
    //     Struct {
    //         name: "Slice".into(),
    //         span,
    //         generics: vec!["t".into()],
    //         fields: [
    //             ("ptr".into(), Type::ptr(Type::generic("t", span), span)),
    //             ("length".into(), Type::int(IntType::usize(), span)),
    //         ]
    //         .into_iter()
    //         .collect(),
    //     },
    // );

    let module_path = Path::new(vec![], span);
    derive_constructors(module_path.clone(), &mut module);
    let cor_structs = derive_cor_structs(module_path.clone(), &mut module);
    dbg!(&module);
    let mut module_map = HashMap::from([(vec![], module)]);
    let global = type_program(&mut module_map).unwrap();
    dbg!(&module_map);
    let lowered = lower_module(&module_map[&vec![]], &global);
    dbg!(&lowered);
    let llvm = emit_program(&lowered, &cor_structs);
    let mut exe_path = Choose::new(&['a', 'b', 'c']).unwrap().sample_string(&mut rand::rng(), 10);
    exe_path.insert_str(0, "./target/");
    let mut file_path = exe_path.clone();
    file_path.push_str(".ll");
    fs::write(&file_path, llvm).unwrap();
    let clang_output = Command::new("clang")
        .args(["-mllvm", "-opaque-pointers", &file_path, "-o", &exe_path])
        .output()
        .expect("Failed to run clang");
    assert!(clang_output.status.success());
    let output = Command::new(&exe_path)
        .output()
        .expect("Failed to run compiler exe");
    output.status.code().unwrap()
}

fn main() {
    let source = r#"
    func main(): I32 = 3
    "#;

    println!("Exe result: {}", compile_and_run(source));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn int_literal() {
        assert_eq!(
            compile_and_run("func main(): I32 = 3"), 3
        )
    }

    #[test]
    fn set_get_variable() {
        assert_eq!(
            compile_and_run(r#"func main(): I32 = {
                let x = 5;
                set x = 6;
                x
            }"#), 6
        )
    }

    #[test]
    fn call_add1() {
        assert_eq!(
            compile_and_run(r#"
                func add1(x: I32): I32 = x + 1
                func main(): I32 = add1(5)
            "#),
            6
        )
    }
}
