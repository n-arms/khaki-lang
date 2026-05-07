use std::{
    collections::{HashSet, VecDeque},
    iter,
};

use crate::{
    ast::{Arith, Cmp, IntType, Literal, Logic, Prim, Type, TypeKind},
    emit::text::{LlvmVals, Text},
    ir::{BlockId, End, Func, Instr, Module, Op, Slot, Value, Witness},
};

mod cor;
mod text;

fn str_list<I: AsRef<str>>(elems: impl IntoIterator<Item = I>) -> String {
    let mut text = String::new();
    for (i, elem) in elems.into_iter().enumerate() {
        if i != 0 {
            text += ", ";
        }
        text += elem.as_ref();
    }
    text
}

fn emit_type(typ: &Type) -> String {
    use TypeKind::*;
    match &typ.kind {
        Func(..) => "ptr".into(),
        Any(..) => emit_type(&typ.children[0]),
        Named(..) => unreachable!(),
        Primitive(prim) => match prim {
            Prim::Int(int_type) => format!("i{}", int_type.width() * 8),
            Prim::Bool => "i8".into(),
            Prim::Unit => "{}".into(),
            Prim::Ptr => "ptr".into(),
        },
        Unif(..) => unreachable!(),
        Generic(_name, _id) => unreachable!(),
        Array(..) => unreachable!(),
    }
}

pub fn resolve_func(func_name: &str) -> String {
    format!("@\"{}\"", func_name)
}

fn slot_name(slot: &Slot) -> String {
    format!("%{}", slot.0)
}

pub fn emit_module(module: &Module) -> String {
    let mut text = Text::default();

    for func in module.funcs.values() {
        let name = if func.name == "main" {
            emit_entry_point(func, &mut text);
            "_main"
        } else {
            &func.name
        };
        emit_func(func, name, &mut text);
    }

    text.pushln("declare void @llvm.memcpy.p0.p0.i64(ptr nocapture writeonly, ptr nocapture readonly, i64, i1 immarg)");
    text.pushln("attributes #0 = { noinline }");
    text.pushln("@.str = private unnamed_addr constant [4 x i8] c\"%d\\0A\\00\"");
    text.pushln("declare i32 @printf(i8*, ...)");

    text.finish()
}

fn emit_entry_point(func: &Func, text: &mut Text) {
    text.pushln("define i32 @main() {");
    text.pushln("entry:");
    let TypeKind::Primitive(Prim::Int(int)) = &func.result.kind else {
        panic!("Main doesn't return an int");
    };
    let width = int.width();
    text.inc();
    text.pushln(format!("%result = alloca [{width} x i8], align {width}"));
    text.pushln("call void @_main(ptr %result)");
    text.pushln("%status_code = load i32, ptr %result");
    text.pushln("ret i32 %status_code");
    text.dec();
    text.pushln("}");
}

fn emit_func(func: &Func, func_name: &str, text: &mut Text) {
    emit_func_prefix(func, func_name, text);
    let mut vals = LlvmVals::default();
    text.pushln(" {");
    text.pushln("entry:");
    text.inc();
    let mut declared = emit_slot_setup(func, text);
    text.pushln(format!("br label %{}", block_name(func.main)));
    text.dec();

    // Perform a BFS on the basic blocks so that dominators are always visited before dominees
    let mut visited = HashSet::new();
    let mut to_visit = VecDeque::from([func.main]);

    while let Some(id) = to_visit.pop_back() {
        if visited.contains(&id) {
            continue;
        }
        visited.insert(id);
        let block = &func.blocks[&id];

        text.pushln(format!("{}:", block_name(id)));
        text.inc();
        for instr in &block.instrs {
            emit_instr(instr, text, &mut vals, &mut declared);
        }
        for id in emit_end(&block.end, text, &mut vals, &mut declared) {
            to_visit.push_front(id);
        }
        text.dec();
    }
    text.pushln("}");
}

fn block_name(block_id: BlockId) -> String {
    format!("block_{}", block_id.0)
}

// generate a fresh llvm val, load the value from the given slot into the val, return the val name
fn load_slot(slot: &Slot, text: &mut Text, vals: &mut LlvmVals) -> String {
    let slot_name = slot_name(slot);
    let result_type = emit_type(&slot.1);
    let temp = vals.fresh();
    text.pushln(format!("{temp} = load {result_type}, ptr {slot_name}",));
    temp
}

// store an llvm val (or literal) into a slot
fn store_slot(slot: &Slot, val: impl AsRef<str>, text: &mut Text) {
    let slot_name = slot_name(slot);
    let result_type = emit_type(&slot.1);
    text.pushln(format!(
        "store {result_type} {}, ptr {slot_name}",
        val.as_ref()
    ));
}

fn arg_name(slot: &Slot) -> String {
    format!("%arg_{}", slot.0)
}

fn emit_func_prefix(func: &Func, func_name: &str, text: &mut Text) {
    let func_name = resolve_func(func_name);
    text.push(format!(
        "define void {func_name}({}) #0",
        str_list(
            func.args
                .iter()
                .map(|arg| format!("ptr {}", arg_name(arg)))
                .chain(iter::once("ptr %result".into()))
        )
    ));
}

// emit top-level alloca's for each slot, and store the function arguments into the appropriate slots
pub fn emit_slot_setup(func: &Func, text: &mut Text) -> HashSet<Slot> {
    let slots: HashSet<_> = func
        .blocks
        .values()
        .flat_map(|block| {
            let mut block_slots = block
                .instrs
                .iter()
                .map(|instr| &instr.result)
                .collect::<Vec<_>>();
            block_slots.extend(block.end.result_slots());
            block_slots
        })
        .chain(&func.args)
        .filter_map(|slot| {
            if let Witness::Static { size, align } = &slot.2 {
                Some((slot.clone(), size, align))
            } else {
                None
            }
        })
        .collect();

    let mut declared = HashSet::new();

    for (slot, size, align) in slots {
        text.pushln(format!(
            "{} = alloca [{size} x i8], align {align}",
            slot_name(&slot),
        ));
        declared.insert(slot);
    }
    declared
}

fn emit_end(
    end: &End,
    text: &mut Text,
    vals: &mut LlvmVals,
    declared: &mut HashSet<Slot>,
) -> Vec<BlockId> {
    match end {
        End::Jump(block_id, _) => {
            text.pushln(format!("br label %{}", block_name(*block_id)));
            vec![*block_id]
        }
        End::JumpIf {
            slot,
            then_branch,
            else_branch,
            ..
        } => {
            let cond = load_slot(slot, text, vals);
            let temp = vals.fresh();
            text.pushln(format!("{temp} = icmp sgt i8 {cond}, 0"));

            let then_label = block_name(*then_branch);
            let else_label = block_name(*else_branch);

            text.pushln(format!(
                "br i1 {temp}, label %{then_label}, label %{else_label}"
            ));
            vec![*then_branch, *else_branch]
        }
        End::Await { .. } => unreachable!(),
        End::Yield(..) => unreachable!(),
        End::Return(slot, ..) => {
            let name = slot_name(slot);
            let size = witness_size(&slot.2, text, vals);
            text.pushln(format!(
                "call void @llvm.memcpy.p0.p0.i64(ptr %result, ptr {name}, i64 {size}, i1 false)"
            ));
            text.pushln(format!("ret void"));
            vec![]
        }
        End::Switch {
            slot,
            branches,
            default,
            span,
        } => todo!(),
    }
}

fn witness_size(witness: &Witness, text: &mut Text, vals: &mut LlvmVals) -> String {
    match witness {
        Witness::Static { size, .. } => size.to_string(),
        Witness::Dynamic(slot) => {
            let name = slot_name(slot);
            let size = vals.fresh();
            text.pushln(format!("{size} = load i64, ptr {name}"));
            size
        }
    }
}

/*
emission strategy:
- all save/restore logic is done before jumping to a particular block that we emit
- every slot corresponds with a stack allocated slot
- since llvm arguments aren't stack allocated, the prefix to the function needs to load them into an alloca
*/
/// - instr: The instruction to lower.
/// - text: The text to push the resulting SSA instructions in to
/// - vars: A source of SSA variables
fn emit_instr(instr: &Instr, text: &mut Text, vals: &mut LlvmVals, declared: &mut HashSet<Slot>) {
    if !declared.contains(&instr.result) {
        let Witness::Dynamic(witness) = &instr.result.2 else {
            unreachable!("Static witnesses should be emitted in prefix");
        };
        let size = witness_size(&witness.2, text, vals);
        let name = slot_name(&instr.result);
        text.pushln(format!("{name} = alloca i8, i64 {size}"));
    }
    let result_type = emit_type(&instr.result.1);
    let store_result = |value: String, text: &mut Text| store_slot(&instr.result, value, text);
    match &instr.value {
        Value::Slot => {
            let temp = load_slot(&instr.args[0], text, vals);
            store_result(temp, text);
        }
        Value::Func(_path, func_name) => {
            let name = resolve_func(func_name);
            store_result(name, text);
        }
        Value::Literal(literal) => {
            let literal_text = match literal {
                Literal::Bool(bool, _) => if *bool { 1 } else { 0 }.to_string(),
                Literal::Number(num, _) => num.to_string(),
                Literal::Unit(_) => "{}".into(),
            };
            store_result(literal_text, text);
        }
        Value::Op(op) => match op {
            Op::Builtin(name) => match name.as_str() {
                "int_print" => {
                    let a = load_slot(&instr.args[0], text, vals);
                    let fmt_ptr = vals.fresh();
                    let temp = vals.fresh();
                    text.pushln(format!(
                        "{fmt_ptr} = getelementptr [4 x i8], [4 x i8]* @.str, i32 0, i32 0"
                    ));
                    text.pushln(format!(
                        "{temp} = call i32 (i8*, ...) @printf(i8* {fmt_ptr}, i32 {a})"
                    ));
                    store_result("{}".into(), text);
                }
                _ => unreachable!("Builtin {name}"),
            },
            Op::Arith(arith) => {
                let args: Vec<_> = instr
                    .args
                    .iter()
                    .map(|arg| load_slot(arg, text, vals))
                    .collect();
                let temp = vals.fresh();
                let op_name = match arith {
                    Arith::Add => "add",
                    Arith::Sub => "sub",
                    Arith::Mul => "mul",
                    Arith::Div => {
                        if IntType::from_type(&instr.args[0].1).unwrap().is_signed() {
                            "sdiv"
                        } else {
                            "udiv"
                        }
                    }
                    Arith::ShiftLeft => "shl",
                    Arith::ShiftRight => "lshr",
                    Arith::BitAnd => "and",
                    Arith::BitOr => "or",
                    Arith::BitNot => todo!(),
                    Arith::BitXor => "xor",
                };

                let suffix = if let Some(arg) = args.get(1) {
                    format!(", {arg}")
                } else {
                    "".into()
                };

                text.pushln(format!(
                    "{temp} = {op_name} {result_type} {}{suffix}",
                    args[0]
                ));
                store_result(temp, text);
            }
            Op::Cmp(cmp_op) => {
                let a = load_slot(&instr.args[0], text, vals);
                let b = load_slot(&instr.args[1], text, vals);
                let cmp = vals.fresh();
                let bool = vals.fresh();
                let arg_type = emit_type(&instr.args[0].1);
                let signed = IntType::from_type(&instr.args[0].1).unwrap().is_signed();

                let op = match cmp_op {
                    Cmp::Lt => {
                        if signed {
                            "slt"
                        } else {
                            "ult"
                        }
                    }
                    Cmp::Le => {
                        if signed {
                            "sle"
                        } else {
                            "ule"
                        }
                    }
                    Cmp::Gt => {
                        if signed {
                            "sgt"
                        } else {
                            "ugt"
                        }
                    }
                    Cmp::Ge => {
                        if signed {
                            "sge"
                        } else {
                            "uge"
                        }
                    }
                    Cmp::Eq => "eq",
                    Cmp::Ne => "ne",
                };

                text.pushln(format!("{cmp} = icmp {op} {arg_type} {a}, {b}"));
                text.pushln(format!("{bool} = zext i1 {cmp} to i8"));
                store_result(bool, text);
            }
            Op::Logic(logic) => {
                let args: Vec<_> = instr
                    .args
                    .iter()
                    .map(|arg| load_slot(arg, text, vals))
                    .collect();
                let temp = vals.fresh();
                let op_name = match logic {
                    Logic::And => "and",
                    Logic::Or => "or",
                    Logic::Xor => "xor",
                    Logic::Not => "xor",
                };

                let suffix = if let Some(arg) = args.get(1) {
                    format!(", {arg}")
                } else {
                    "-1".into() // for bitwise not
                };

                text.pushln(format!(
                    "{temp} = {op_name} {result_type} {}{suffix}",
                    args[0]
                ));
                store_result(temp, text);
            }
        },
        Value::Call => {
            // list of (type, val) pairs
            let fp = load_slot(&instr.args[0], text, vals);
            let args = instr
                .args
                .iter()
                .skip(1)
                .map(|arg| format!("ptr {}", slot_name(&arg)))
                .chain(iter::once(format!("ptr {}", slot_name(&instr.result))));
            let temp = vals.fresh();
            text.pushln(format!(
                "{temp} = call {result_type} {fp}({})",
                str_list(args)
            ));
            store_result(temp, text);
        }
        Value::Ref => {
            store_result(slot_name(&instr.args[0]), text);
        }
        Value::FieldGet(_index, _witnesses) => todo!(),
        Value::FieldRef(index, _witnesses) => {
            let container_ptr = load_slot(&instr.args[0], text, vals);
            let field_ptr = vals.fresh();
            let TypeKind::Primitive(Prim::Ptr) = &instr.args[0].1.kind else {
                unreachable!()
            };
            let container_type = emit_type(&instr.args[0].1.children[0]);
            text.pushln(format!(
                            "{field_ptr} = getelementptr {container_type}, {container_type}* {container_ptr}, i32 0, i32 {index}"
                        ));
            store_result(field_ptr, text);
        }
        Value::PackStruct(_path, _name) => {
            let struct_slot = slot_name(&instr.result);
            for (i, arg) in instr.args.iter().enumerate() {
                let arg_slot = load_slot(arg, text, vals);
                let field_ptr = vals.fresh();
                text.pushln(format!(
                            "{field_ptr} = getelementptr {result_type}, {result_type}* {struct_slot}, i32 0, i32 {i}"
                        ));
                let arg_type = emit_type(&arg.1);
                text.pushln(format!(
                    "store {arg_type} {arg_slot}, {arg_type}* {field_ptr}"
                ));
            }
        }
        Value::Array(..) => {
            let array_slot = slot_name(&instr.result);
            for (i, arg) in instr.args.iter().enumerate() {
                let arg_slot = load_slot(arg, text, vals);
                let field_ptr = vals.fresh();
                text.pushln(format!(
                            "{field_ptr} = getelementptr {result_type}, {result_type}* {array_slot}, i32 0, i32 {i}"
                        ));
                let arg_type = emit_type(&arg.1);
                text.pushln(format!(
                    "store {arg_type} {arg_slot}, {arg_type}* {field_ptr}"
                ));
            }
        }
        Value::RefArray => {
            let array_slot = slot_name(&instr.args[0]);
            let array_ptr = vals.fresh();
            assert_eq!(instr.result.1.kind, TypeKind::Primitive(Prim::Ptr));
            let array_type = emit_type(&instr.args[0].1);
            text.pushln(format!(
                "{array_ptr} = getelementptr {array_type}, {array_type}* {array_slot}, i32 0, i32 0"
            ));
            store_result(array_ptr, text);
        }
        Value::IndexRef(_elem_witness) => {
            let array_ptr = load_slot(&instr.args[0], text, vals);
            let index = load_slot(&instr.args[1], text, vals);
            let elem_ptr = vals.fresh();
            let TypeKind::Primitive(Prim::Ptr) = &instr.args[0].1.kind else {
                unreachable!()
            };
            let elem_type = emit_type(&instr.args[0].1.children[0]);
            text.pushln(format!(
                "{elem_ptr} = getelementptr {elem_type}, {elem_type}* {array_ptr}, i32 {index}"
            ));
            store_result(elem_ptr, text);
        }
        Value::Store => todo!(),
        Value::Load => todo!(),
        Value::Unreachable => todo!(),
        Value::Undefined => todo!(),
    }
}
