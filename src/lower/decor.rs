//! Removes all coroutines, End::Await, and End::Yield calls from the IR.

use crate::{
    ast::{IntType, Literal, Path, Span, Struct, Type, TypeKind},
    ir::{BlockId, End, Func, Instr, Module, Slot, Value, Witness},
    lower::{
        Env, Global, bool_witness, builder::FuncBuilder, function_witness, integer_witness,
        lower_witness, pointer_witness, struct_witness, unit_witness,
    },
    ord_map::OrdMap,
};
use std::{
    collections::{HashMap, HashSet},
    iter,
};

pub fn decor(modules: &mut HashMap<Vec<String>, Module>, global: &Global) {
    let paths: Vec<_> = modules.keys().cloned().collect();
    for path in paths {
        let func_names: Vec<_> = modules.get(&path).unwrap().funcs.keys().cloned().collect();
        for func_name in func_names {
            let func = &modules.get(&path).unwrap().funcs[&func_name];
            if func.is_cor {
                let path = Path::new(path.clone(), func.result.span);
                let cor_path = path.clone().with(func.name.clone(), func.result.span);
                let generics = modules.get(&cor_path.path).unwrap().structs[&func.name]
                    .generics
                    .clone();
                let (poll, constructor, strukt) = build_coroutine(&path, func, global, &generics);
                modules
                    .get_mut(&path.path)
                    .unwrap()
                    .funcs
                    .insert(func_name.clone(), constructor);
                modules
                    .get_mut(&cor_path.path)
                    .unwrap()
                    .funcs
                    .insert("poll".into(), poll);
                modules
                    .get_mut(&cor_path.path)
                    .unwrap()
                    .structs
                    .insert(func_name, strukt);
            }
        }
    }
}

/// Enumerates the slots that need to be presevered over yield boundaries, generate the coroutine struct and the poll method.
/// Produce the constructor for the coroutine struct.
///
/// # Arguments
///
/// * `cor_path` - The path to the coroutine function, not its associated struct/poll.
/// * `cor` - The coroutine function.
/// * `global` - Global references.
/// * `generics` - The generics to the cor.
///
/// # Returns
///
/// Returns a tuple with (constructor, poll, struct) containing the funcs and struct to replace the old cor function with.
fn build_coroutine(
    cor_path: &Path,
    cor: &Func,
    global: &Global,
    generics: &[String],
) -> (Func, Func, Struct) {
    // Enumerates the slots that need to be saved
    let CorSlots { saved_slots, .. } = saved_slots(cor);
    let (state_map, states) = build_block_state_maps(cor);
    let saved_map: HashMap<Slot, usize> = saved_slots
        .iter()
        .enumerate()
        .map(|(id, slot)| (slot.clone(), id + 1))
        .collect();

    let start_block = cor.blocks.keys().map(|id| id.0 + 1).max().unwrap_or(0);
    let start_slot = cor
        .args
        .iter()
        .cloned()
        .chain(cor.blocks.values().flat_map(|block| {
            block
                .instrs
                .iter()
                .map(|instr| instr.result.clone())
                .chain(block.end.result_slots().into_iter().cloned())
        }))
        .map(|slot| (&slot.0[5..]).parse::<usize>().unwrap() + 1)
        .max()
        .unwrap_or(0);
    let mut fb = FuncBuilder::new_at(start_block, start_slot);

    let mut env = Env::default();
    for (name, slot) in generics.iter().zip(&cor.args) {
        env.set_var(name.clone(), slot.clone());
    }

    let span = cor.result.span;
    let cor_type = Type::named(
        cor_path.clone().with(cor.name.clone(), span),
        cor.name.clone(),
        generics
            .iter()
            .map(|name| Type::generic(name, span))
            .collect(),
        span,
    );
    let cor_slot = Slot(
        "cor".into(),
        Type::ptr(cor_type.clone(), span),
        pointer_witness(),
    );

    let mut fields = OrdMap::new();
    let mut cor_witnesses = Vec::new();

    let cor_struct_slots =
        iter::once(("state".into(), Type::int(IntType::usize(), cor.result.span))).chain(
            saved_slots
                .iter()
                .map(|slot| (slot.0.clone(), slot.1.clone())),
        );
    for (name, typ) in cor_struct_slots {
        let witness = decor_witness(
            lower_witness(&typ, &mut fb, &env, global),
            &mut fb,
            &saved_map,
            &cor_slot,
            &cor_witnesses,
            typ.span,
        );
        cor_witnesses.push(witness);
        fields.insert(name, typ.clone());
    }
    let result_slot = Slot(
        "result".into(),
        Type::ptr(cor.result.clone(), span),
        pointer_witness(),
    );
    let state_type = Type::int(IntType::usize(), span);
    let state_ptr = fb.instr(
        Type::ptr(state_type.clone(), span),
        pointer_witness(),
        Value::FieldRef(0, vec![integer_witness(&IntType::usize())]),
        vec![cor_slot.clone()],
        span,
    );
    let state = fb.instr(
        state_type.clone(),
        integer_witness(&IntType::usize()),
        Value::Load,
        vec![state_ptr.clone()],
        span,
    );
    let illegal_cor_block = fb.create_block();
    fb.end_block(End::Switch {
        slot: state,
        branches: states,
        default: illegal_cor_block,
        span,
    });
    fb.start_block(illegal_cor_block);
    let _ = fb.instr(
        Type::unit(span),
        unit_witness(),
        Value::Unreachable,
        vec![],
        span,
    );
    fb.end_block(End::Jump(illegal_cor_block, span));
    for (id, block) in cor.blocks.iter() {
        fb.start_block(*id);
        for instr in &block.instrs {
            let slot = decor_slot(
                &instr.result,
                &mut fb,
                &saved_map,
                &cor_slot,
                &cor_witnesses,
                span,
            );
            if let Value::Ref = instr.value {
                let ptr = decor_slot_ref(
                    &instr.args[0],
                    &mut fb,
                    &saved_map,
                    &cor_slot,
                    &cor_witnesses,
                    span,
                );
                fb.push(Instr {
                    result: slot.clone(),
                    value: Value::Slot,
                    args: vec![ptr],
                    span,
                });
            } else {
                let args = instr
                    .args
                    .iter()
                    .map(|arg| {
                        decor_slot(arg, &mut fb, &saved_map, &cor_slot, &cor_witnesses, span)
                    })
                    .collect();
                fb.push(Instr {
                    result: slot.clone(),
                    value: instr.value.clone(),
                    args,
                    span,
                });
            }

            if let Some(index) = saved_map.get(&slot) {
                let slot_ptr = fb.instr(
                    Type::ptr(slot.1.clone(), slot.1.span),
                    pointer_witness(),
                    Value::FieldRef(*index, cor_witnesses.clone()),
                    vec![cor_slot.clone()],
                    span,
                );
                let _ = fb.instr(
                    Type::unit(span),
                    unit_witness(),
                    Value::Store,
                    vec![slot_ptr, slot],
                    span,
                );
            }
        }
        match block.end.clone() {
            End::Jump(..) => {
                fb.end_block(block.end.clone());
            }
            End::JumpIf {
                slot,
                then_branch,
                else_branch,
                span,
            } => {
                let slot = decor_slot(&slot, &mut fb, &saved_map, &cor_slot, &cor_witnesses, span);
                fb.end_block(End::JumpIf {
                    slot,
                    then_branch,
                    else_branch,
                    span,
                });
            }
            End::Switch {
                slot,
                branches,
                default,
                span,
            } => {
                let slot = decor_slot(&slot, &mut fb, &saved_map, &cor_slot, &cor_witnesses, span);
                fb.end_block(End::Switch {
                    slot,
                    branches,
                    default,
                    span,
                });
            }
            End::Await {
                cor_struct,
                result,
                then_branch,
                span,
            } => {
                let cor_struct_ptr = decor_slot_ref(
                    &cor_struct,
                    &mut fb,
                    &saved_map,
                    &cor_slot,
                    &cor_witnesses,
                    span,
                );
                let next_state = state_map[&id];
                let TypeKind::Named(path, _) = cor_struct.1.kind.clone() else {
                    unreachable!()
                };
                let result_ref = decor_slot_ref(
                    &result,
                    &mut fb,
                    &saved_map,
                    &cor_slot,
                    &cor_witnesses,
                    span,
                );
                let cor_ptr_type = Type::ptr(cor_struct.1.clone(), span);
                let result_ptr_type = result_ref.1.clone();
                let func = fb.instr(
                    Type::func(
                        Vec::new(),
                        vec![cor_ptr_type, result_ptr_type],
                        Type::bool(span),
                        span,
                    ),
                    function_witness(),
                    Value::Func(path, "poll".into()),
                    vec![],
                    span,
                );
                let should_continue = fb.instr(
                    Type::bool(span),
                    bool_witness(),
                    Value::Call,
                    vec![func, cor_struct_ptr, result_ref],
                    span,
                );
                let yield_block = fb.create_block();
                fb.end_block(End::JumpIf {
                    slot: should_continue,
                    then_branch,
                    else_branch: yield_block,
                    span,
                });
                fb.start_block(yield_block);
                let next_state_slot = fb.instr(
                    state_type.clone(),
                    integer_witness(&IntType::usize()),
                    Value::Literal(Literal::Number(next_state.to_string(), span)),
                    vec![],
                    span,
                );
                let _ = fb.instr(
                    Type::unit(span),
                    unit_witness(),
                    Value::Store,
                    vec![state_ptr.clone(), next_state_slot],
                    span,
                );
                let false_slot = fb.instr(
                    Type::bool(span),
                    bool_witness(),
                    Value::Literal(Literal::Bool(false, span)),
                    vec![],
                    span,
                );
                fb.end_block(End::Return(false_slot, span));
            }
            End::Yield(block_id, span) => {
                let next_state = state_map[&block_id];
                let next_state_slot = fb.instr(
                    state_type.clone(),
                    integer_witness(&IntType::usize()),
                    Value::Literal(Literal::Number(next_state.to_string(), span)),
                    vec![],
                    span,
                );
                let _ = fb.instr(
                    Type::unit(span),
                    unit_witness(),
                    Value::Store,
                    vec![state_ptr.clone(), next_state_slot],
                    span,
                );
                let false_slot = fb.instr(
                    Type::bool(span),
                    bool_witness(),
                    Value::Literal(Literal::Bool(false, span)),
                    vec![],
                    span,
                );
                fb.end_block(End::Return(false_slot, span));
            }
            End::Return(slot, span) => {
                let slot = decor_slot(&slot, &mut fb, &saved_map, &cor_slot, &cor_witnesses, span);
                let _ = fb.instr(
                    Type::unit(span),
                    unit_witness(),
                    Value::Store,
                    vec![result_slot.clone(), slot.clone()],
                    span,
                );
                let true_slot = fb.instr(
                    Type::bool(span),
                    bool_witness(),
                    Value::Literal(Literal::Bool(true, span)),
                    vec![],
                    span,
                );
                fb.end_block(End::Return(true_slot, span));
            }
        }
    }
    let mut poll = fb.finish(
        "poll".into(),
        false,
        vec![cor_slot, result_slot],
        Type::bool(cor.result.span),
    );
    poll.main = BlockId(start_block);
    let mut fb = FuncBuilder::new();
    let cor_witness = struct_witness(cor_witnesses, &mut fb, cor.result.span);
    let first_state = state_map[&cor.main];
    let first_state_slot = fb.instr(
        Type::int(IntType::usize(), cor.result.span),
        integer_witness(&IntType::usize()),
        Value::Literal(Literal::Number(first_state.to_string(), cor.result.span)),
        vec![],
        cor.result.span,
    );
    let mut cor_struct_args = cor.args.clone();
    cor_struct_args.insert(0, first_state_slot);
    let constructed = fb.instr(
        cor_type.clone(),
        cor_witness,
        Value::PackStruct(
            cor_path.with(cor.name.clone(), cor.result.span),
            cor.name.clone(),
        ),
        cor_struct_args,
        cor.result.span,
    );
    fb.end_block(End::Return(constructed, cor.result.span));
    let constructor = fb.finish(cor.name.clone(), false, cor.args.clone(), cor_type);

    let strukt = Struct {
        name: cor.name.clone(),
        generics: generics.to_vec(),
        fields,
        span,
    };

    (constructor, poll, strukt)
}

/// Yield a reference to the given slot, either to the slot in the coroutine struct, or the temporary slot itself.
fn decor_slot_ref(
    slot: &Slot,
    fb: &mut FuncBuilder,
    saved_map: &HashMap<Slot, usize>,
    cor_ptr: &Slot,
    cor_witnesses: &[Witness],
    span: Span,
) -> Slot {
    let result_type = Type::ptr(slot.1.clone(), span);
    let result_witness = pointer_witness();
    if let Some(index) = saved_map.get(slot) {
        fb.instr(
            result_type,
            result_witness,
            Value::FieldRef(*index, cor_witnesses.to_vec()),
            vec![cor_ptr.clone()],
            span,
        )
    } else {
        fb.instr(
            result_type,
            result_witness,
            Value::Ref,
            vec![slot.clone()],
            span,
        )
    }
}

fn decor_slot(
    slot: &Slot,
    fb: &mut FuncBuilder,
    saved_map: &HashMap<Slot, usize>,
    cor_ptr: &Slot,
    cor_witnesses: &[Witness],
    span: Span,
) -> Slot {
    if let Some(index) = saved_map.get(slot) {
        let slot_pointer = fb.instr(
            Type::ptr(slot.1.clone(), span),
            pointer_witness(),
            Value::FieldRef(*index, cor_witnesses.to_vec()),
            vec![cor_ptr.clone()],
            span,
        );
        fb.instr(
            slot.1.clone(),
            slot.2.clone(),
            Value::Load,
            vec![slot_pointer],
            span,
        )
    } else {
        slot.clone()
    }
}

fn decor_witness(
    witness: Witness,
    fb: &mut FuncBuilder,
    saved_map: &HashMap<Slot, usize>,
    cor_ptr: &Slot,
    cor_witnesses: &[Witness],
    span: Span,
) -> Witness {
    match witness {
        Witness::Static { .. } => witness,
        Witness::Dynamic(slot) => Witness::Dynamic(Box::new(decor_slot(
            slot.as_ref(),
            fb,
            saved_map,
            cor_ptr,
            cor_witnesses,
            span,
        ))),
    }
}

struct CorSlots {
    saved_slots: Vec<Slot>,
    temp_slots: Vec<Slot>,
}

fn saved_slots(func: &Func) -> CorSlots {
    let mut saved = func.args.clone();
    let mut all = HashSet::new();
    for block in func.blocks.values() {
        let mut defined: HashSet<Slot> = HashSet::new();
        for instr in &block.instrs {
            for arg in &instr.args {
                if !defined.contains(arg) && !saved.contains(arg) {
                    saved.push(arg.clone());
                }
            }
            defined.insert(instr.result.clone());
            all.insert(&instr.result);

            if let Value::Ref = instr.value {
                if !saved.contains(&instr.args[0]) {
                    saved.push(instr.args[0].clone());
                }
            }
        }
        all.extend(block.end.result_slots());
    }

    let mut temp_slots = Vec::new();

    for slot in all {
        if !saved.contains(slot) {
            temp_slots.push(slot.clone());
        }
    }
    CorSlots {
        saved_slots: saved,
        temp_slots,
    }
}

/// Calculate all the slots that need to be saved / restored over await points
/// we don't do fine-grained tracking of which slot needs to be saved over which gap.

/// Each basic block which ends in an `End::Yield` or `End::Await` gets a state number.
fn build_block_state_maps(func: &Func) -> (HashMap<BlockId, usize>, Vec<BlockId>) {
    let mut state_map: HashMap<_, _> = func
        .blocks
        .iter()
        .filter_map(|(current_id, block)| match &block.end {
            End::Return(..) | End::Jump(..) | End::JumpIf { .. } | End::Switch { .. } => None,
            // yields resume at the next block
            End::Yield(next_id, _) => Some(*next_id),
            // awaits resume at the current block
            End::Await { .. } => Some(*current_id),
        })
        .enumerate()
        .map(|(state, id)| (id, state + 1))
        .collect();
    state_map.insert(func.main, 0);

    let state_vec = (0..state_map.len())
        .map(|state| {
            state_map
                .iter()
                .find_map(|(id, curr_state)| {
                    if state == *curr_state {
                        Some(*id)
                    } else {
                        None
                    }
                })
                .unwrap()
        })
        .collect();
    (state_map, state_vec)
}
