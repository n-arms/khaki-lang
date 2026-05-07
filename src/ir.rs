//! Witness table usage
//! Consider each of the following operations:
//! **Slot:** needs to have the witness of the result/arg to perform a copy
//! **Field:** needs to know the witnesses of all fields
//! **FieldRef:** same as above, but doesn't need the witness of the result
//! **Call:** need witnesses of the args and witnesses of the result

use core::fmt;
use std::collections::HashMap;

use crate::ast::{Arith, Cmp, Literal, Logic, Path, Span, Struct, Type};

#[derive(Clone, Debug)]
pub struct Module {
    pub structs: HashMap<String, Struct>,
    pub funcs: HashMap<String, Func>,
}

#[derive(Clone)]
pub struct Func {
    pub name: String,
    pub is_cor: bool,
    pub args: Vec<Slot>,
    pub result: Type,
    pub main: BlockId,
    pub blocks: HashMap<BlockId, Block>,
}

#[derive(Clone)]
pub struct Block {
    pub instrs: Vec<Instr>,
    pub end: End,
}

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct BlockId(pub usize);

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Slot(pub String, pub Type, pub Witness);

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Witness {
    Static { size: usize, align: usize },
    Dynamic(Box<Slot>),
}

#[derive(Clone)]
pub struct Instr {
    pub result: Slot,
    pub value: Value,
    pub args: Vec<Slot>,
    pub span: Span,
}

#[derive(Clone)]
pub enum Value {
    Slot,
    Func(Path, String),
    Literal(Literal),
    Op(Op),
    Call,
    Ref,
    Store,
    Load,

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
    // Unreachable branch. Either traps or invokes UB depending on backend.
    Unreachable,
    // An undefined value. Reading from it is UB.
    Undefined,
}

#[derive(Clone, Debug)]
pub enum Op {
    Builtin(String),
    Arith(Arith),
    Cmp(Cmp),
    Logic(Logic),
}

#[derive(Clone)]
pub enum End {
    Jump(BlockId, Span),
    JumpIf {
        slot: Slot,
        then_branch: BlockId,
        else_branch: BlockId,
        span: Span,
    },
    Switch {
        slot: Slot,
        branches: Vec<BlockId>,
        default: BlockId,
        span: Span,
    },
    /// If a block ends with an End::Await, the whole block is rerun every time the await point is resumed.
    /// Awaits are normally lowered to a jump into an empty block that has an End::Await.
    Await {
        cor_struct: Slot,
        result: Slot,
        then_branch: BlockId,
        span: Span,
    },
    Yield(BlockId, Span),
    Return(Slot, Span),
}

impl Witness {
    pub fn is_static(&self) -> bool {
        if let Witness::Static { .. } = self {
            true
        } else {
            false
        }
    }
}

impl End {
    pub fn result_slots(&self) -> impl IntoIterator<Item = &Slot> {
        match self {
            End::Jump(..)
            | End::JumpIf { .. }
            | End::Switch { .. }
            | End::Yield(..)
            | End::Return(..) => None,
            End::Await { result, .. } => Some(result),
        }
    }
}

impl fmt::Debug for BlockId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "bb{}", self.0)
    }
}

impl fmt::Debug for Slot {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0) // Name only for argument positions
    }
}

impl Slot {
    fn fmt_full(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {:?} {:?}", self.0, self.1, self.2)
    }
}

impl fmt::Debug for Witness {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Witness::Static { size, align } => write!(f, "static({}, {})", size, align),
            Witness::Dynamic(s) => write!(f, "dyn({:?})", s),
        }
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Slot => write!(f, "slot"),
            Value::Func(p, s) => write!(f, "fn {:?}::{}", p, s),
            Value::Literal(l) => write!(f, "{:?}", l),
            Value::Op(o) => write!(f, "{:?}", o),
            Value::Call => write!(f, "call"),
            Value::Ref => write!(f, "ref"),
            Value::Store => write!(f, "store"),
            Value::Load => write!(f, "load"),
            Value::PackStruct(p, s) => write!(f, "pack {:?}::{}", p, s),
            Value::FieldRef(i, w) => write!(f, "field_ref.{} {:?}", i, w),
            Value::FieldGet(i, w) => write!(f, "field_get.{} {:?}", i, w),
            Value::Array(n, w) => write!(f, "array [{} x {:?}]", n, w),
            Value::RefArray => write!(f, "ref_array"),
            Value::IndexRef(w) => write!(f, "index_ref {:?}", w),
            Value::Unreachable => write!(f, "unreachable"),
            Value::Undefined => write!(f, "undefined"),
        }
    }
}

impl fmt::Debug for Instr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "    ")?;
        self.result.fmt_full(f)?;
        write!(f, " = {:?} {:?}", self.value, self.args)
    }
}

impl fmt::Debug for End {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "    ")?;
        match self {
            End::Jump(id, _) => write!(f, "jump {:?}", id),
            End::JumpIf {
                slot,
                then_branch,
                else_branch,
                ..
            } => write!(f, "br {:?}, {:?}, {:?}", slot, then_branch, else_branch),
            End::Switch {
                slot,
                branches,
                default,
                ..
            } => write!(
                f,
                "switch {:?}, {:?}, default {:?}",
                slot, branches, default
            ),
            End::Await {
                cor_struct,
                result,
                then_branch,
                ..
            } => {
                write!(f, "await ")?;
                result.fmt_full(f)?;
                write!(f, " from {:?}, then {:?}", cor_struct, then_branch)
            }
            End::Yield(id, _) => write!(f, "yield {:?}", id),
            End::Return(slot, _) => write!(f, "ret {:?}", slot),
        }
    }
}

impl fmt::Debug for Func {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let kind = if self.is_cor { "coro fn" } else { "fn" };
        write!(f, "{} {}(", kind, self.name)?;
        for (i, arg) in self.args.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            arg.fmt_full(f)?;
        }
        writeln!(f, ") -> {:?} {{", self.result)?;

        let mut ids: Vec<_> = self.blocks.keys().collect();
        ids.sort_by_key(|id| id.0);

        for id in ids {
            let block = &self.blocks[id];
            let entry = if *id == self.main { " (main)" } else { "" };
            writeln!(f, "  {:?}{}:", id, entry)?;
            for instr in &block.instrs {
                writeln!(f, "{:?}", instr)?;
            }
            writeln!(f, "{:?}", block.end)?;
        }
        write!(f, "}}")
    }
}
