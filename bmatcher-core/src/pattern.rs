use core::fmt::Debug;

use alloc::vec::Vec;

use crate::Atom;

/// A binary pattern is a structure used in matching processes and consists of two main components:
///
/// 1. Atoms  
///    A list of instructions or actions that define how the matcher should process the data at the current cursor.
///    
/// 2. Byte Sequence  
///    A sequence of bytes that the atoms can reference to match against actual input data.
pub trait BinaryPattern: Send + Sync + Debug {
    /// Retrieves the list of atoms within this binary pattern.
    ///
    /// The atoms define the actions or matching logic for the pattern.
    fn atoms(&self) -> &[Atom];

    /// Retrieves the byte sequence referenced by the atoms.
    ///
    /// This sequence represents the actual data to be matched against.
    fn byte_sequence(&self) -> &[u8];

    /// Returns an upper bound for the length of the save stack.
    ///
    /// # Note
    /// This is only an upper bound. The actual length used might be smaller.
    fn save_len(&self) -> usize {
        self.atoms()
            .iter()
            .filter(|atom| {
                matches!(
                    atom,
                    Atom::Read(_) | Atom::SaveCursor | Atom::SaveConstant(_)
                )
            })
            .count()
    }

    /// Returns an upper bound for the length of the cursor stack.
    ///
    /// # Note
    /// This is only an upper bound. The actual length used might be smaller.
    fn cursor_len(&self) -> usize {
        self.atoms()
            .iter()
            .filter(|atom| matches!(atom, Atom::CursorPush))
            .count()
    }
}

/// An implementation of the [BinaryPattern] interface that borrows the [Atom]s and byte sequence array.
///
/// This struct is primarily used alongside the [pattern!] macro to generate patterns at runtime.
#[derive(Debug, Clone, Copy)]
pub struct BorrowedBinaryPattern<'a> {
    atoms: &'a [Atom],
    byte_sequence: &'a [u8],
}

impl<'a> BorrowedBinaryPattern<'a> {
    pub const fn new(atoms: &'a [Atom], byte_sequence: &'a [u8]) -> Self {
        Self {
            atoms,
            byte_sequence,
        }
    }
}

impl BinaryPattern for BorrowedBinaryPattern<'_> {
    fn atoms(&self) -> &[Atom] {
        self.atoms
    }

    fn byte_sequence(&self) -> &[u8] {
        self.byte_sequence
    }
}

/// An implementation of the [BinaryPattern] interface that allocates a `Vec` for the [Atom]s and the byte sequence.
///
/// This struct is primarily used with [compiler::parse] to parse binary patterns at runtime.
#[derive(Debug, Default)]
pub struct OwnedBinaryPattern {
    atoms: Vec<Atom>,
    byte_sequence: Vec<u8>,
}

impl OwnedBinaryPattern {
    pub fn new(atoms: Vec<Atom>, byte_sequence: Vec<u8>) -> Self {
        Self {
            byte_sequence,
            atoms,
        }
    }
}

impl BinaryPattern for OwnedBinaryPattern {
    fn byte_sequence(&self) -> &[u8] {
        &self.byte_sequence
    }

    fn atoms(&self) -> &[Atom] {
        &self.atoms
    }
}
