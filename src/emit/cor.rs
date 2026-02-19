/*
cors are lowered into a struct (called `cor`) with one function: `cor.poll`,
as well as the original cor function (called the constructor), which just builds a `cor` and returns it
*/

use std::collections::{HashMap, HashSet};

use crate::{
    ast::{Type, cor_name},
    emit::{
        block_name, emit_instr, emit_slot_setup, emit_type, load_slot, resolve_func, store_slot,
        str_list,
        text::{LlvmVals, Text},
    },
    ir::{BlockId, End, Func, Slot},
    typing::Spec,
};

// calculate all the slots that need to be saved / restored over await points
// we don't do fine-grained tracking of which slot needs to be saved over which gap
fn saved_slots(func: &Func) -> Vec<Slot> {
    let mut saved = Vec::new();
    for block in func.blocks.values() {
        let mut defined: HashSet<Slot> = HashSet::new();
        for instr in &block.instrs {
            for arg in &instr.args {
                if !defined.contains(arg) && !saved.contains(arg) {
                    saved.push(arg.clone());
                }
            }
            defined.insert(instr.result.clone());
        }
    }
    for arg in &func.args {
        if !saved.contains(arg) {
            saved.push(arg.clone());
        }
    }
    saved
}

// each basic block which ends in an `End::Yield` or `End::Await` gets a state number
fn build_block_state_maps(func: &Func) -> HashMap<BlockId, usize> {
    let mut state_map: HashMap<_, _> = func
        .blocks
        .values()
        .filter_map(|block| match &block.end {
            End::Return(..) | End::Jump(..) | End::JumpIf { .. } => None,
            End::Yield(id, _)
            | End::Await {
                then_branch: id, ..
            } => Some(*id),
        })
        .enumerate()
        .map(|(state, id)| (id, state + 1))
        .collect();
    state_map.insert(func.main, 0);
    state_map
}

fn cor_struct_name(struct_spec: &Spec, func: &Func) -> String {
    let result_type = Type::named(
        cor_name(&struct_spec.struct_name, &func.name),
        struct_spec.generics.clone(),
        func.result.span,
    );

    emit_type(&result_type)
}

pub fn emit_cor_struct(struct_spec: &Spec, func: &Func, text: &mut Text) {
    let slots = saved_slots(func);
    let struct_name = cor_struct_name(struct_spec, func);
    text.pushln(format!(
        "{struct_name} = type {{ i32, {} }}",
        str_list(slots.iter().map(|slot| emit_type(&slot.1)))
    ))
}

pub fn emit_constructor(struct_spec: &Spec, func: &Func, text: &mut Text) {
    let struct_name = cor_struct_name(struct_spec, func);

    let constructor_name = resolve_func(struct_spec, &func.name);

    text.pushln(format!(
        "define {struct_name} {constructor_name}({}) {{",
        str_list(
            func.args
                .iter()
                .map(|arg| format!("{} %{}", emit_type(&arg.1), arg.0))
        )
    ));
    text.pushln("entry:");
    text.inc();

    let mut vals = LlvmVals::default();
    let mut current_struct = String::from("undef");
    for (i, arg) in func.args.iter().enumerate() {
        let new_struct = vals.fresh();
        let arg_type = emit_type(&arg.1);
        text.pushln(format!(
            "{new_struct} = insertvalue {struct_name} {current_struct}, {arg_type} %{}, {}",
            arg.0,
            i + 1
        ));
        current_struct = new_struct;
    }

    text.pushln(format!("ret {struct_name} {current_struct}"));

    text.dec();
    text.pushln("}");
}

pub fn emit_poll(struct_spec: &Spec, func: &Func, text: &mut Text) {
    let cor_spec = Spec {
        struct_name: cor_name(&struct_spec.struct_name, &func.name),
        generics: struct_spec.generics.clone(),
    };
    let poll_name = resolve_func(&cor_spec, "poll");
    let cor_struct = cor_struct_name(struct_spec, func);
    let result_type = emit_type(&func.result);
    text.pushln(format!(
        "define i8 {poll_name}({cor_struct}* %cor, {result_type}* %result) {{"
    ));
    text.pushln("entry:");
    text.inc();

    emit_slot_setup(func, text);
    let saved_slots = saved_slots(func);

    let mut vals = LlvmVals::default();

    let mut field_ptrs = Vec::new();

    for (i, slot) in saved_slots.iter().enumerate() {
        let field_ptr = vals.fresh();
        text.pushln(format!(
            "{field_ptr} = getelementptr {cor_struct}, {cor_struct}* %cor, i32 0, i32 {}",
            i + 1
        ));
        let field = vals.fresh();
        let field_type = emit_type(&slot.1);
        text.pushln(format!(
            "{field} = load {field_type}, {field_type}* {field_ptr}"
        ));
        field_ptrs.push((field_ptr, slot));
        store_slot(slot, field, text);
    }

    let state_ptr = vals.fresh();
    text.pushln(format!(
        "{state_ptr} = getelementptr {cor_struct}, {cor_struct}* %cor, i32 0, i32 0",
    ));
    let state = vals.fresh();
    text.pushln(format!("{state} = load i32, i32* {state_ptr}"));

    let id_to_state = build_block_state_maps(func);

    text.pushln(format!("switch i32 {state}, label %bad_state ["));
    text.inc();
    for (id, state) in &id_to_state {
        text.pushln(format!("i32 {state}, label %{}", block_name(*id)))
    }
    text.dec();
    text.pushln("]");
    text.dec();

    text.pushln("bad_state:");
    text.inc();
    text.pushln("ret i8 0");
    text.dec();

    for (id, block) in &func.blocks {
        text.pushln(format!("{}:", block_name(*id)));
        text.inc();
        for instr in &block.instrs {
            emit_instr(instr, text, &mut vals);
        }
        emit_cor_end(
            &block.end,
            &id_to_state,
            &state_ptr,
            &field_ptrs,
            text,
            &mut vals,
        );
        text.dec();
    }

    text.pushln("}");
}

fn save_saved_slots(field_ptrs: &[(String, &Slot)], text: &mut Text, vals: &mut LlvmVals) {
    for (ptr_val, slot) in field_ptrs {
        let slot_val = load_slot(slot, text, vals);
        let slot_type = emit_type(&slot.1);
        text.pushln(format!(
            "store {slot_type} {slot_val}, {slot_type}* {ptr_val}"
        ));
    }
}

fn emit_cor_end(
    end: &End,
    id_to_state: &HashMap<BlockId, usize>,
    state_ptr: &str,
    field_ptrs: &[(String, &Slot)],
    text: &mut Text,
    vals: &mut LlvmVals,
) {
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
        End::Yield(block_id, span) => {
            let state = id_to_state[block_id];
            text.pushln(format!("store i32 {state}, i32* {state_ptr}"));
            save_saved_slots(field_ptrs, text, vals);
            text.pushln(format!("ret i8 0"));
        }
        End::Return(slot, span) => {
            let return_val = load_slot(slot, text, vals);
            let return_type = emit_type(&slot.1);
            text.pushln(format!(
                "store {return_type} {return_val}, {return_type}* %result"
            ));
            text.pushln(format!("ret i8 1"));
        }
    }
}
