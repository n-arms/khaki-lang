use std::collections::HashMap;

use crate::{
    ast::{Span, Type},
    ir::{Arg, Block, BlockId, End, Func, Instr, Slot, Value, Witness},
};

pub struct FuncBuilder {
    next_id: usize,
    next_slot: usize,
    blocks: HashMap<BlockId, Block>,
    current: Option<BlockBuilder>,
}

#[derive(Debug)]
struct BlockBuilder {
    id: BlockId,
    instrs: Vec<Instr>,
}

impl FuncBuilder {
    pub fn new() -> Self {
        Self::new_at(0, 0)
    }

    pub fn new_at(first_id: usize, first_slot: usize) -> Self {
        Self {
            next_id: first_id + 1,
            next_slot: first_slot,
            blocks: HashMap::default(),
            current: Some(BlockBuilder {
                id: BlockId(first_id),
                instrs: Vec::new(),
            }),
        }
    }

    pub fn create_block(&mut self) -> BlockId {
        let id = self.next_id;
        self.next_id += 1;
        BlockId(id)
    }

    pub fn slot(&mut self, typ: Type, witness: Witness) -> Slot {
        let slot_id = self.next_slot;
        self.next_slot += 1;
        let slot_name = format!("slot_{slot_id}");
        Slot(slot_name, typ, witness)
    }

    pub fn start_block(&mut self, id: BlockId) {
        if let Some(current) = &self.current {
            panic!("Premature end to unfinished block {current:?}");
        }
        self.current = Some(BlockBuilder {
            id,
            instrs: Vec::new(),
        });
    }

    pub fn end_block(&mut self, end: End) {
        let Some(current) = self.current.take() else {
            panic!("Ending nonexistent block");
        };
        self.blocks.insert(
            current.id,
            Block {
                instrs: current.instrs,
                end,
            },
        );
    }

    pub fn instr(
        &mut self,
        result: Type,
        witness: Witness,
        value: Value,
        args: Vec<Slot>,
        span: Span,
    ) -> Slot {
        let slot = self.slot(result, witness);
        let Some(current) = self.current.as_mut() else {
            panic!("Pushing instr to nonexistent block");
        };
        current.instrs.push(Instr {
            result: slot.clone(),
            value,
            args,
            span,
        });
        slot
    }

    pub fn push(&mut self, instr: Instr) {
        let Some(current) = self.current.as_mut() else {
            panic!("Pushing instr to nonexistent block");
        };
        current.instrs.push(instr);
    }

    pub fn finish(mut self, name: String, is_cor: bool, args: Vec<Arg>, result: Type) -> Func {
        if let Some(current) = self.current.take() {
            panic!("Premature end to unfinished block {current:?}");
        };
        Func {
            name,
            is_cor,
            args,
            result,
            main: BlockId(0),
            blocks: self.blocks,
        }
    }
}
