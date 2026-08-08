use std::{
    collections::{HashSet, VecDeque},
    io::{self, Write},
    iter,
};

use crate::{
    ast::{Arith, Cmp, IntType, Literal, Logic, Prim, Type, TypeKind},
    emit::text::{LlvmVals, Text},
    ir::{Arg, BlockId, End, Func, Instr, Module, Op, Slot, Value, Witness},
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
        Cor(..) => unreachable!(),
    }
}

pub fn resolve_func(func_name: &str) -> String {
    format!("@\"{}\"", func_name)
}

/// Qualify a function name with its module path. Root-module functions keep
/// their bare name; nested modules (e.g. cor modules) get "module::name".
/// Used for both definitions and references so they stay in sync.
fn func_symbol(path: &[String], name: &str) -> String {
    if path.is_empty() {
        name.to_string()
    } else {
        format!("{}::{name}", path.join("::"))
    }
}

fn slot_name(slot: &Slot) -> String {
    format!("%{}", slot.0)
}

pub fn emit_prelude() -> String {
    let mut text = Text::default();
    text.pushln("declare void @llvm.memcpy.p0.p0.i64(ptr nocapture writeonly, ptr nocapture readonly, i64, i1 immarg)");
    text.pushln("attributes #0 = { noinline }");
    text.pushln("@.str = private unnamed_addr constant [4 x i8] c\"%d\\0A\\00\"");
    text.pushln("declare i32 @printf(i8*, ...)");
    text.finish()
}

pub fn emit_module(path: &[String], module: &Module) -> String {
    let mut text = Text::default();

    for func in module.funcs.values() {
        let name = if func.name == "main" {
            emit_entry_point(func, &mut text);
            "_main"
        } else {
            &func.name
        };
        emit_func(func, &func_symbol(path, name), &mut text);
    }

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
    println!("ON FUNCTION {func_name}");
    io::stdout().flush().unwrap();
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

fn arg_name(slot: &Arg) -> String {
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
            ..
        } => {
            let state = load_slot(slot, text, vals);
            let state_type = emit_type(&slot.1);

            let mut switch = format!(
                "switch {state_type} {state}, label %{} [",
                block_name(*default)
            );
            for (i, branch) in branches.iter().enumerate() {
                switch.push_str(&format!(
                    " {state_type} {i}, label %{}",
                    block_name(*branch)
                ));
            }
            switch.push_str(" ]");
            text.pushln(switch);

            let mut succs = branches.clone();
            succs.push(*default);
            succs
        }
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

fn witness_align(witness: &Witness, text: &mut Text, vals: &mut LlvmVals) -> String {
    match witness {
        Witness::Static { align, .. } => align.to_string(),
        Witness::Dynamic(slot) => {
            // Witness struct is packed as { i64 size, i64 align }, so align lives at byte offset 8
            let name = slot_name(slot);
            let ptr = vals.fresh();
            text.pushln(format!("{ptr} = getelementptr i8, ptr {name}, i64 8"));
            let align = vals.fresh();
            text.pushln(format!("{align} = load i64, ptr {ptr}"));
            align
        }
    }
}

/// Given the running byte offset into a container and the size and align of the next field,
/// return (start, next): where the field begins (offset + padding) and the offset after it.
/// Padding follows C struct layout: pad = (align - (offset % align)) % align.
fn next_field(
    offset: &str,
    size: &str,
    align: &str,
    text: &mut Text,
    vals: &mut LlvmVals,
) -> (String, String) {
    let rem = vals.fresh();
    text.pushln(format!("{rem} = urem i64 {offset}, {align}"));
    let sub = vals.fresh();
    text.pushln(format!("{sub} = sub i64 {align}, {rem}"));
    let pad = vals.fresh();
    text.pushln(format!("{pad} = urem i64 {sub}, {align}"));
    let start = vals.fresh();
    text.pushln(format!("{start} = add i64 {offset}, {pad}"));
    let next = vals.fresh();
    text.pushln(format!("{next} = add i64 {start}, {size}"));
    (start, next)
}

/// Byte offset of the index-th field within its container, per the container's field witnesses.
fn field_start(
    index: usize,
    witnesses: &[Witness],
    text: &mut Text,
    vals: &mut LlvmVals,
) -> String {
    let mut offset = "0".to_string();
    for (i, witness) in witnesses.iter().enumerate() {
        let size = witness_size(witness, text, vals);
        let align = witness_align(witness, text, vals);
        let (start, next) = next_field(&offset, &size, &align, text, vals);
        if i == index {
            return start;
        }
        offset = next;
    }
    unreachable!(
        "field index {index} out of bounds for {} fields",
        witnesses.len()
    );
}

/// Pack each arg slot into the container at C-struct-aligned byte offsets, copying each
/// whole arg slot in with a memcpy. Sizes and aligns are computed up front so that dynamic
/// witness loads are emitted before the writes they feed.
fn pack_fields<'a>(
    container: &str,
    args: &[Slot],
    witnesses: impl IntoIterator<Item = &'a Witness>,
    text: &mut Text,
    vals: &mut LlvmVals,
) {
    let fields: Vec<_> = witnesses
        .into_iter()
        .map(|witness| {
            (
                witness_size(witness, text, vals),
                witness_align(witness, text, vals),
            )
        })
        .collect();
    let mut offset = "0".to_string();
    for (arg, (size, align)) in args.iter().zip(&fields) {
        let (start, next) = next_field(&offset, &size, &align, text, vals);
        let field_ptr = vals.fresh();
        text.pushln(format!(
            "{field_ptr} = getelementptr i8, ptr {container}, i64 {start}"
        ));
        text.pushln(format!(
            "call void @llvm.memcpy.p0.p0.i64(ptr {field_ptr}, ptr {}, i64 {size}, i1 false)",
            slot_name(arg)
        ));
        offset = next;
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
    println!("{instr:?}");
    std::io::stdout().flush().unwrap();
    let store_result = |value: String, text: &mut Text| store_slot(&instr.result, value, text);
    match &instr.value {
        Value::Slot => {
            let src = slot_name(&instr.args[0]);
            let size = witness_size(&instr.result.2, text, vals);
            let result_name = slot_name(&instr.result);
            text.pushln(format!(
                "call void @llvm.memcpy.p0.p0.i64(ptr {result_name}, ptr {src}, i64 {size}, i1 false)"
            ));
        }
        Value::Arg(arg) => {
            let arg_name = arg_name(arg);
            let arg_size = witness_size(&arg.2, text, vals);
            let result_name = slot_name(&instr.result);

            text.pushln(format!(
                "call void @llvm.memcpy.p0.p0.i64(ptr {result_name}, ptr {arg_name}, i64 {arg_size}, i1 false)"
            ));
        }
        Value::Func(path, func_name) => {
            let name = resolve_func(&func_symbol(&path.path, func_name));
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
                "transmute" => {
                    // transmute: byte-copy the source slot into the result slot
                    let source = slot_name(&instr.args[0]);
                    let result_name = slot_name(&instr.result);
                    let size = witness_size(&instr.args[0].2, text, vals);
                    text.pushln(format!(
                        "call void @llvm.memcpy.p0.p0.i64(ptr {result_name}, ptr {source}, i64 {size}, i1 false)"
                    ));
                }
                _ => unreachable!("Builtin {name}"),
            },
            Op::Arith(Arith::Max) => {
                // clang 14 rejects the umax/smax instructions; lower to icmp + select
                let a = load_slot(&instr.args[0], text, vals);
                let b = load_slot(&instr.args[1], text, vals);
                let signed = IntType::from_type(&instr.args[0].1).unwrap().is_signed();
                let result_type = emit_type(&instr.result.1);
                let cond = vals.fresh();
                text.pushln(format!(
                    "{cond} = icmp {} {result_type} {a}, {b}",
                    if signed { "sgt" } else { "ugt" }
                ));
                let temp = vals.fresh();
                text.pushln(format!(
                    "{temp} = select i1 {cond}, {result_type} {a}, {result_type} {b}"
                ));
                store_result(temp, text);
            }
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
                    Arith::Urem => {
                        if IntType::from_type(&instr.args[0].1).unwrap().is_signed() {
                            "srem"
                        } else {
                            "urem"
                        }
                    }
                    Arith::ShiftLeft => "shl",
                    Arith::ShiftRight => "lshr",
                    Arith::BitAnd => "and",
                    Arith::BitOr => "or",
                    Arith::BitNot => todo!(),
                    Arith::BitXor => "xor",
                    Arith::Max => unreachable!(),
                };

                let suffix = if let Some(arg) = args.get(1) {
                    format!(", {arg}")
                } else {
                    "".into()
                };

                let result_type = emit_type(&instr.result.1);
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
                    // boolean not: Bool is i8 with values 0/1, so flip the low bit
                    ", 1".into()
                };

                let result_type = emit_type(&instr.result.1);
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
            text.pushln(format!("call void {fp}({})", str_list(args)));
        }
        Value::Ref => {
            store_result(slot_name(&instr.args[0]), text);
        }
        Value::FieldGet(index, witnesses) => {
            // container is by value: its slot holds the struct bytes, so the field lives
            // at a byte offset inside that slot
            let container_slot = slot_name(&instr.args[0]);
            let offset = field_start(*index, witnesses, text, vals);
            let field_ptr = vals.fresh();
            text.pushln(format!(
                "{field_ptr} = getelementptr i8, ptr {container_slot}, i64 {offset}"
            ));
            let size = witness_size(&instr.result.2, text, vals);
            let result_name = slot_name(&instr.result);
            text.pushln(format!(
                "call void @llvm.memcpy.p0.p0.i64(ptr {result_name}, ptr {field_ptr}, i64 {size}, i1 false)"
            ));
        }
        Value::FieldRef(index, witnesses) => {
            // container is a slot holding a ptr to the struct, so GEP off the loaded pointer
            let container_ptr = load_slot(&instr.args[0], text, vals);
            let offset = field_start(*index, witnesses, text, vals);
            let field_ptr = vals.fresh();
            text.pushln(format!(
                "{field_ptr} = getelementptr i8, ptr {container_ptr}, i64 {offset}"
            ));
            store_result(field_ptr, text);
        }
        Value::PackStruct(_path, _name) => {
            pack_fields(
                &slot_name(&instr.result),
                &instr.args,
                instr.args.iter().map(|arg| &arg.2),
                text,
                vals,
            );
        }
        Value::Array(_count, elem_witness) => {
            pack_fields(
                &slot_name(&instr.result),
                &instr.args,
                instr.args.iter().map(|_| elem_witness),
                text,
                vals,
            );
        }
        Value::RefArray => {
            // the array slot's address is the pointer to its first byte
            store_result(slot_name(&instr.args[0]), text);
        }
        Value::IndexRef(elem_witness) => {
            let array_ptr = load_slot(&instr.args[0], text, vals);
            let index = load_slot(&instr.args[1], text, vals);
            // scale the byte index by the element witness size, then byte-offset GEP
            let elem_size = witness_size(elem_witness, text, vals);
            let byte_index = vals.fresh();
            text.pushln(format!("{byte_index} = mul i64 {index}, {elem_size}"));
            let elem_ptr = vals.fresh();
            text.pushln(format!(
                "{elem_ptr} = getelementptr i8, ptr {array_ptr}, i64 {byte_index}"
            ));
            store_result(elem_ptr, text);
        }
        Value::Store => {
            let ptr = load_slot(&instr.args[0], text, vals);
            // byte-copy the whole value slot into the destination, sized by its
            // witness, like every other compound-value operation in this file
            let size = witness_size(&instr.args[1].2, text, vals);
            let val = slot_name(&instr.args[1]);
            text.pushln(format!(
                "call void @llvm.memcpy.p0.p0.i64(ptr {ptr}, ptr {val}, i64 {size}, i1 false)"
            ));
        }
        Value::Load => {
            let ptr = load_slot(&instr.args[0], text, vals);
            let size = witness_size(&instr.result.2, text, vals);
            let result_name = slot_name(&instr.result);
            text.pushln(format!(
                "call void @llvm.memcpy.p0.p0.i64(ptr {result_name}, ptr {ptr}, i64 {size}, i1 false)"
            ));
        }
        Value::Unreachable => {
            text.pushln("unreachable");
        }
        Value::Undefined => {
            let undef = match &instr.result.1.kind {
                TypeKind::Primitive(Prim::Unit) => "{}".into(),
                _ => "undef".into(),
            };
            store_result(undef, text);
        }
    }
}
