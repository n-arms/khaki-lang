use std::collections::HashMap;

use crate::{
    ast::{Span, Type},
    ir::{Block, BlockId, End, Func, Instr, Slot, Value},
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
        Self {
            next_id: 1,
            next_slot: 0,
            blocks: HashMap::default(),
            current: Some(BlockBuilder {
                id: BlockId(0),
                instrs: Vec::new(),
            }),
        }
    }

    pub fn create_block(&mut self) -> BlockId {
        let id = self.next_id;
        self.next_id += 1;
        BlockId(id)
    }

    pub fn slot(&mut self, typ: Type) -> Slot {
        let slot_id = self.next_slot;
        self.next_slot += 1;
        let slot_name = format!("slot_{slot_id}");
        Slot(slot_name, typ)
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

    pub fn instr(&mut self, result: Type, value: Value, args: Vec<Slot>, span: Span) -> Slot {
        let slot = self.slot(result);
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

    pub fn finish(mut self, name: String, is_cor: bool, args: Vec<Slot>, result: Type) -> Func {
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
