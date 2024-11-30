#![cfg_attr(not(test), no_std)]
extern crate alloc;

pub mod compiler;

mod atom;
pub use atom::{Atom, JumpType, ReadWidth};

mod target;
pub use target::MatchTarget;

mod matcher;
pub use matcher::BinaryMatcher;

mod pattern;
pub use pattern::{BinaryPattern, BorrowedBinaryPattern, OwnedBinaryPattern};
