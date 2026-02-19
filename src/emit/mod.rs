use std::collections::{HashMap, HashSet};

use crate::{
    ast::{Literal, Type, TypeKind},
    emit::text::Text,
    ir::{BlockId, End, Func, Instr, Op, Slot, Struct, Value},
    typing::Spec,
};

mod text;

#[derive(Default)]
struct LlvmVals {
    next: usize,
}

impl LlvmVals {
    fn fresh(&mut self) -> String {
        let id = self.next;
        self.next += 1;
        format!("%var{id}")
    }
}

// calculate all the slots that need to be saved / restored over await points
// we don't do fine-grained tracking of which slot needs to be saved over which gap
fn saved_slots(func: &Func) -> HashSet<Slot> {
    let mut saved = HashSet::new();
    for block in func.blocks.values() {
        let mut defined: HashSet<Slot> = HashSet::new();
        for instr in &block.instrs {
            for arg in &instr.args {
                if !defined.contains(arg) {
                    saved.insert(arg.clone());
                }
            }
            defined.insert(instr.result.clone());
        }
    }
    saved
}

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
    if let TypeKind::Named(name) = &typ.kind {
        if let Some(prim) = primitive_name(name) {
            prim
        } else {
            format!("%\"{}\"", type_name(typ))
        }
    } else {
        type_name(typ)
    }
}

fn primitive_name(name: &str) -> Option<String> {
    let new_name = match name {
        "Int" => "i32",
        "Bool" => "i8",
        _ => return None,
    };
    Some(new_name.into())
}

fn type_name(typ: &Type) -> String {
    match &typ.kind {
        TypeKind::Func => {
            let [args @ .., result] = typ.children.as_slice() else {
                unreachable!()
            };
            format!(
                "{} ({})*",
                emit_type(result),
                str_list(args.iter().map(emit_type))
            )
        }
        TypeKind::Named(name) => {
            if let Some(name) = primitive_name(name) {
                name
            } else {
                format!("{name}[{}]", str_list(typ.children.iter().map(type_name)))
            }
        }
        TypeKind::Generic(_) | TypeKind::Unif(_) => unreachable!(),
    }
}

fn struct_name(struct_spec: &Spec) -> String {
    format!(
        "{}[{}]",
        struct_spec.struct_name,
        str_list(struct_spec.generics.iter().map(type_name)),
    )
}

fn resolve_struct(struct_spec: &Spec) -> String {
    format!("%\"{}\"", struct_name(struct_spec))
}

fn resolve_func(struct_spec: &Spec, func_name: &str) -> String {
    format!("@\"{}.{}\"", struct_name(struct_spec), func_name)
}

fn slot_name(slot: &Slot) -> String {
    format!("%{}", slot.0)
}

pub fn emit_program(program: &HashMap<Spec, Struct>) -> String {
    let mut text = Text::default();

    for (spec, strukt) in program {
        if ["Int", "Bool"].contains(&spec.struct_name.as_str()) {
            continue;
        }
        emit_struct_type_def(spec, strukt, &mut text);
    }

    for (spec, strukt) in program {
        for func in strukt.funcs.values() {
            emit_func(spec, func, &mut text);
        }
    }

    text.finish()
}

fn emit_struct_type_def(struct_spec: &Spec, strukt: &Struct, text: &mut Text) {
    text.pushln(format!(
        "{} = type {{ {} }}",
        resolve_struct(struct_spec),
        str_list(strukt.fields.values().map(emit_type))
    ));
}

// emits a "normal" function (not a cor)
fn emit_func(struct_spec: &Spec, func: &Func, text: &mut Text) {
    emit_func_prefix(struct_spec, func, text);
    let mut vals = LlvmVals::default();
    text.pushln(" {");
    text.pushln("entry:");
    text.inc();
    emit_slot_setup(func, text);
    text.pushln(format!("br label %{}", block_name(func.main)));
    text.dec();

    for (id, block) in &func.blocks {
        text.pushln(format!("{}:", block_name(*id)));
        text.inc();
        for instr in &block.instrs {
            emit_instr(instr, text, &mut vals);
        }
        emit_end(&block.end, text, &mut vals);
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
    text.pushln(format!(
        "{temp} = load {result_type}, {result_type}* {slot_name}",
    ));
    temp
}

// store an llvm val (or literal) into a slot
fn store_slot(slot: &Slot, val: impl AsRef<str>, text: &mut Text) {
    let slot_name = slot_name(slot);
    let result_type = emit_type(&slot.1);
    text.pushln(format!(
        "store {result_type} {}, {result_type}* {slot_name}",
        val.as_ref()
    ));
}

fn arg_name(slot: &Slot) -> String {
    format!("%arg_{}", slot.0)
}

fn emit_func_prefix(struct_spec: &Spec, func: &Func, text: &mut Text) {
    let func_name = resolve_func(struct_spec, &func.name);
    let result_type = emit_type(&func.result);
    text.push(format!(
        "define {result_type} {func_name}({})",
        str_list(
            func.args
                .iter()
                .map(|arg| format!("{} {}", emit_type(&arg.1), arg_name(arg)))
        )
    ))
}

// emit top-level alloca's for each slot, and store the function arguments into the appropriate slots
fn emit_slot_setup(func: &Func, text: &mut Text) {
    let mut slots: HashSet<_> = func
        .blocks
        .values()
        .flat_map(|block| {
            block
                .instrs
                .iter()
                .map(|instr| &instr.result)
                .collect::<Vec<_>>()
        })
        .collect();
    slots.extend(&func.args);

    for slot in slots {
        text.pushln(format!(
            "{} = alloca {}",
            slot_name(&slot),
            emit_type(&slot.1)
        ));
    }

    for arg in &func.args {
        store_slot(arg, arg_name(arg), text);
    }
}

fn emit_end(end: &End, text: &mut Text, vals: &mut LlvmVals) {
    match end {
        End::Jump(block_id, span) => {
            text.pushln(format!("br label %{}", block_name(*block_id)));
        }
        End::JumpIf {
            slot,
            then_branch,
            else_branch,
            span,
        } => {
            let cond = load_slot(slot, text, vals);
            let temp = vals.fresh();
            text.pushln(format!("{temp} = icmp sgt i8 {cond}, 0"));

            let then_label = block_name(*then_branch);
            let else_label = block_name(*else_branch);

            text.pushln(format!(
                "br i1 {temp}, label %{then_label}, label %{else_label}"
            ));
        }
        End::Await {
            cor_struct,
            result,
            then_branch,
            span,
        } => todo!(),
        End::Yield(block_id, span) => todo!(),
        End::Return(slot, span) => {
            let return_val = load_slot(slot, text, vals);
            let return_type = emit_type(&slot.1);
            text.pushln(format!("ret {return_type} {return_val}"));
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
fn emit_instr(instr: &Instr, text: &mut Text, vals: &mut LlvmVals) {
    let result_type = emit_type(&instr.result.1);
    let store_result = |value: String, text: &mut Text| store_slot(&instr.result, value, text);
    match &instr.value {
        Value::Slot => {
            let temp = load_slot(&instr.args[0], text, vals);
            store_result(temp, text);
        }
        Value::Func(struct_spec, func_name) => {
            let name = resolve_func(struct_spec, func_name);
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
                "int_add" => {
                    let a = load_slot(&instr.args[0], text, vals);
                    let b = load_slot(&instr.args[1], text, vals);
                    let temp = vals.fresh();
                    text.pushln(format!("{temp} = add {result_type} {a}, {b}"));
                    store_result(temp, text);
                }
                _ => unreachable!("Builtin {name}"),
            },
        },
        Value::Call => {
            // list of (type, val) pairs
            let args: Vec<(String, String)> = instr
                .args
                .iter()
                .map(|arg| {
                    let slot_type = emit_type(&arg.1);
                    let temp = load_slot(arg, text, vals);
                    (slot_type, temp)
                })
                .collect();
            let fp = &args[0].1;
            let temp = vals.fresh();
            text.pushln(format!(
                "{temp} = call {result_type} {fp}({})",
                str_list(args.iter().skip(1).map(|(typ, val)| format!("{typ} {val}")))
            ));
            store_result(temp, text);
        }
    }
}
