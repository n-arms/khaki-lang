//! Witness table usage
//! Consider each of the following operations:
//! **Slot:** needs to have the witness of the result/arg to perform a copy
//! **Field:** needs to know the witnesses of all fields
//! **FieldRef:** same as above, but doesn't need the witness of the result
//! **Call:** need witnesses of the args and witnesses of the result

use std::collections::HashMap;

use crate::{
    ast::{Arith, Cmp, Literal, Logic, Path, Span, Struct, Type},
    ord_map::OrdMap,
};

#[derive(Clone, Debug)]
pub struct Module {
    pub structs: HashMap<String, Struct>,
    pub funcs: HashMap<String, Func>,
}

#[derive(Clone, Debug)]
pub struct Func {
    pub name: String,
    pub is_cor: bool,
    pub args: Vec<Slot>,
    pub result: Type,
    pub main: BlockId,
    pub blocks: HashMap<BlockId, Block>,
}

#[derive(Clone, Debug)]
pub struct Block {
    pub instrs: Vec<Instr>,
    pub end: End,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct BlockId(pub usize);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Slot(pub String, pub Type, pub Witness);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Witness {
    Static { size: usize, align: usize },
    Dynamic(Box<Slot>),
}

#[derive(Clone, Debug)]
pub struct Instr {
    pub result: Slot,
    pub value: Value,
    pub args: Vec<Slot>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum Value {
    Slot,
    Func(Path, String),
    Literal(Literal),
    Op(Op),
    Call,
    Ref,

    // Struct operations
    PackStruct(Path, String),
    // Given a pointer to a struct and witnesses for each field, make a pointer to the field
    FieldRef(usize, Vec<Witness>),
    FieldGet(usize, Vec<Witness>),

    // Array operations
    // Create an array of the given size
    Array(usize, Witness),
    // Take the reference of an array as a ptr
    RefArray,
    // Given a pointer, make a pointer that has indexed by the given amount
    IndexRef(Witness),
}

#[derive(Clone, Debug)]
pub enum Op {
    Builtin(String),
    Arith(Arith),
    Cmp(Cmp),
    Logic(Logic),
}

#[derive(Clone, Debug)]
pub enum End {
    Jump(BlockId, Span),
    JumpIf {
        slot: Slot,
        then_branch: BlockId,
        else_branch: BlockId,
        span: Span,
    },
    Await {
        cor_struct: Slot,
        result: Slot,
        then_branch: BlockId,
        span: Span,
    },
    Yield(BlockId, Span),
    Return(Slot, Span),
}

impl End {
    pub fn result_slots(&self) -> impl IntoIterator<Item = &Slot> {
        match self {
            End::Jump(..) | End::JumpIf { .. } | End::Yield(..) | End::Return(..) => None,
            End::Await { result, .. } => Some(result),
        }
    }
}
