#![cfg_attr(not(test), no_std)]
extern crate alloc;

pub mod compiler;

mod atom;
pub use atom::{
    Atom,
    JumpType,
    ReadWidth,
};

mod target;
pub use target::MatchTarget;

mod matcher;
pub use matcher::{
    execute,
    execute_with_stack,
    BinaryMatcher,
};

mod stack;
pub use stack::{
    HeapStack,
    Stack,
    StaticStack,
};

mod pattern;
pub use pattern::{
    BinaryPattern,
    GenericBinaryPattern,
};
