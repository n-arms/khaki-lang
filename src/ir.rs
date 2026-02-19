use std::collections::HashMap;

use crate::{
    ast::{Literal, Span, Type},
    typing::Spec,
};

#[derive(Clone, Debug)]
pub struct Struct {
    pub name: Spec,
    pub fields: HashMap<String, Type>,
    pub funcs: HashMap<String, Func>,
}

#[derive(Clone, Debug)]
pub struct Func {
    pub name: String,
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
pub struct Slot(pub String, pub Type);

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
    Func(Spec, String),
    Literal(Literal),
    Op(Op),
    Call,
}

#[derive(Clone, Debug)]
pub enum Op {
    Builtin(String),
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
