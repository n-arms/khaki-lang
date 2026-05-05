/*
cors are lowered into a struct (called `cor`) with one function: `cor.poll`,
as well as the original cor function (called the constructor), which just builds a `cor` and returns it
*/

use std::collections::{HashMap, HashSet};

use crate::{
    emit::{
        block_name, emit_type, load_slot, slot_name,
        text::{LlvmVals, Text},
    },
    ir::{BlockId, End, Func, Slot, Value},
};

fn cor_struct_name(_func: &Func) -> String {
    todo!()
    /*
    let result_type = Type::named(
        cor_name(&struct_spec.struct_name, &func.name),
        struct_spec.generics.clone(),
        func.result.span,
    );

    emit_type(&result_type)
    */
}

pub fn emit_cor_struct(_func: &Func, _text: &mut Text) {
    /*
    let slots = saved_slots(func).saved_slots;
    let struct_name = cor_struct_name(struct_spec, func);
    let fields = iter::once("i32".into()).chain(slots.iter().map(|slot| emit_type(&slot.1)));
    text.pushln(format!("{struct_name} = type {{ {} }}", str_list(fields)))
    */
}

pub fn emit_constructor(_func: &Func, _text: &mut Text) {
    /*
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
    let final_struct = vals.fresh();
    text.pushln(format!(
        "{final_struct} = insertvalue {struct_name} {current_struct}, i32 0, 0"
    ));

    text.pushln(format!("ret {struct_name} {final_struct}"));

    text.dec();
    text.pushln("}");
    */
}

/*
fn emit_slot_setup(slots: &CorSlots, cor_struct: &str, text: &mut Text) {
    for slot in &slots.temp_slots {
        text.pushln(format!(
            "{} = alloca {}",
            slot_name(&slot),
            emit_type(&slot.1)
        ))
    }

    for (i, slot) in slots.saved_slots.iter().enumerate() {
        let name = slot_name(slot);
        text.pushln(format!(
            "{name} = getelementptr {cor_struct}, {cor_struct}* %cor, i32 0, i32 {}",
            i + 1
        ))
    }
}
*/
pub fn emit_poll(_func: &Func, _text: &mut Text) {
    /*
    let cor_spec = Spec {
        struct_name: cor_name(&struct_spec.struct_name, &func.name),
        generics: struct_spec.generics.clone(),
    };
    let poll_name = resolve_func(&cor_spec, "poll");
    let cor_struct = cor_struct_name(struct_spec, func);
    let result_type = emit_type(&func.result);
    text.pushln(format!(
        "define i8 {poll_name}({cor_struct}* %cor, {result_type}* %result) #0 {{"
    ));
    text.pushln("entry:");
    text.inc();

    let slots = saved_slots(func);
    emit_slot_setup(&slots, &cor_struct, text);

    let mut vals = LlvmVals::default();

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
        emit_cor_end(&block.end, &id_to_state, &state_ptr, text, &mut vals);
        text.dec();
    }

    text.pushln("}");
    */
}

fn emit_cor_end(
    end: &End,
    id_to_state: &HashMap<BlockId, usize>,
    state_ptr: &str,
    text: &mut Text,
    vals: &mut LlvmVals,
) {
    match end {
        End::Jump(block_id, _span) => {
            text.pushln(format!("br label %{}", block_name(*block_id)));
        }
        End::JumpIf {
            slot,
            then_branch,
            else_branch,
            span: _,
        } => {
            let cond = load_slot(slot, text, vals);
            let temp = vals.fresh();
            text.pushln(format!("{temp} = icmp ne i8 {cond}, 0"));

            let then_label = block_name(*then_branch);
            let else_label = block_name(*else_branch);

            text.pushln(format!(
                "br i1 {temp}, label %{then_label}, label %{else_label}"
            ));
        }
        End::Await {
            cor_struct: _,
            result: _,
            then_branch: _,
            span: _,
        } => {
            todo!()
            /*
            let TypeKind::Named(cor_name) = &cor_struct.1.kind else {
                unreachable!()
            };
            let poll_name = resolve_func(&cor_spec, "poll");
            let poll_result = vals.fresh();
            text.pushln(format!(
                "{poll_result} = call i8 {poll_name}({}* {}, {}* {})",
                emit_type(&cor_struct.1),
                slot_name(cor_struct),
                emit_type(&result.1),
                slot_name(result),
            ));
            let temp = vals.fresh();
            text.pushln(format!("{temp} = icmp ne i8 {poll_result}, 0"));
            let then_label = block_name(*then_branch);
            let yield_label = format!("await_{}", then_branch.0);
            text.pushln(format!(
                "br i1 {temp}, label %{then_label}, label %{yield_label}"
            ));
            text.dec();
            text.pushln(format!("{yield_label}:"));
            text.inc();
            text.pushln("ret i8 0");
            */
        }
        End::Yield(block_id, _span) => {
            let state = id_to_state[block_id];
            text.pushln(format!("store i32 {state}, i32* {state_ptr}"));
            text.pushln(format!("ret i8 0"));
        }
        End::Return(slot, _span) => {
            let return_val = load_slot(slot, text, vals);
            let return_type = emit_type(&slot.1);
            text.pushln(format!(
                "store {return_type} {return_val}, {return_type}* %result"
            ));
            text.pushln(format!("ret i8 1"));
        }
        End::Switch {
            slot,
            branches,
            default,
            span,
        } => todo!(),
    }
}
