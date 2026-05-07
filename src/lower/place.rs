use std::collections::HashMap;

use crate::ir::{Func, Slot, Value};

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Place {
    Stack,
    Reg,
}

#[derive(Default)]
pub struct PlaceMap {
    slots: HashMap<Slot, Place>,
}

impl PlaceMap {
    pub fn set_slot(&mut self, slot: Slot, place: Place) {
        self.slots.insert(slot, place);
    }

    pub fn get_slot(&self, slot: &Slot) -> Option<Place> {
        self.slots.get(slot).copied()
    }
}

pub fn get_places(func: &Func) -> PlaceMap {
    let mut map = PlaceMap::default();

    for arg in &func.args {
        if arg.2.is_static() {
            map.set_slot(arg.clone(), Place::Reg);
        } else {
            map.set_slot(arg.clone(), Place::Stack);
        }
    }

    for block in func.blocks.values() {
        for instr in &block.instrs {
            if instr.result.2.is_static() {
                map.set_slot(instr.result.clone(), Place::Reg);
            } else {
                map.set_slot(instr.result.clone(), Place::Stack);
            }
            if let Value::Ref = &instr.value {
                map.set_slot(instr.args[0].clone(), Place::Stack);
            }
        }
    }

    map
}
