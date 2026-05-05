use std::{collections::HashMap, fs, process::Command};

use crate::{
    ast::{Path, Span},
    derive::{derive_constructors, derive_cor_structs},
    emit::emit_program,
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
    if config.lower {
        dbg!(ir_map);
        return 0;
    }
    /*
    let llvm = emit_program(&lowered);
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
    output.status.code().unwrap()
    */
    0
}

fn main() {
    let source = r#"
    cor foo(): I32 = {
        yield;
        4
    }
    cor main(): I32 = foo()! + 1
    "#;

    println!(
        "Exe result: {}",
        compile_and_run(
            "main",
            source,
            Config {
                lower: true,
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
}
