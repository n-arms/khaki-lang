use std::{collections::HashMap, fs};

use crate::{
    ast::{Expr, Func, IntType, Op, Path, Span, Struct, Type},
    derive::{derive_constructors, derive_cor_structs},
    emit::emit_program,
    lower::lower_module,
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
    func main(): I32 = 3
    "#;

    let file_id = 0;
    let tokens = scan_program(source).unwrap();
    let mut module = parse_program(source, &tokens, file_id).unwrap();

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
    fs::write("out.ll", llvm).unwrap();
}
