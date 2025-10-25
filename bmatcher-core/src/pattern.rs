use alloc::borrow::Cow;
use core::fmt::Debug;

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

/// A flexible implementation of the [`BinaryPattern`] interface supporting both borrowed and owned data.
///
/// This struct uses [`std::borrow::Cow`] for both the [`Atom`] array and the byte sequence,
/// allowing it to either borrow data without allocation or own it when necessary.
/// This design makes it suitable for both compile-time defined and runtime-parsed patterns.
///
/// # Example
/// ```
/// # use bmatcher_core::*;
/// let atoms = &[Atom::ByteSequence { seq_start: 0x00, seq_end: 0x01 }];
/// let bytes = &[0x90];
///
/// // Borrowed (no allocation)
/// let borrowed = GenericBinaryPattern::new(atoms, bytes);
///
/// // Owned (allocating)
/// let owned = GenericBinaryPattern::new(vec![Atom::ByteSequence { seq_start: 0x00, seq_end: 0x01 }], vec![0x90]);
#[derive(Debug, Clone)]
pub struct GenericBinaryPattern<'a> {
    atoms: Cow<'a, [Atom]>,
    byte_sequence: Cow<'a, [u8]>,
}

impl<'a> GenericBinaryPattern<'a> {
    /// Creates a new [`GenericBinaryPattern`] from borrowed or owned data.
    ///
    /// This constructor accepts any type that can be converted into a [`Cow`],
    /// such as slices or vectors.  
    /// Borrowed inputs (`&[Atom]`, `&[u8]`) avoid allocation, while owned inputs
    /// (`Vec<Atom>`, `Vec<u8>`) store the data internally.
    pub fn new(atoms: impl Into<Cow<'a, [Atom]>>, byte_sequence: impl Into<Cow<'a, [u8]>>) -> Self {
        Self {
            atoms: atoms.into(),
            byte_sequence: byte_sequence.into(),
        }
    }

    /// Creates a borrowed [`GenericBinaryPattern`] from static references.
    ///
    /// This constructor is `const` and does not perform any heap allocation.
    /// It is ideal for defining patterns that reference constant or static data.
    pub const fn new_const(atoms: &'a [Atom], byte_sequence: &'a [u8]) -> Self {
        Self {
            atoms: Cow::Borrowed(atoms),
            byte_sequence: Cow::Borrowed(byte_sequence),
        }
    }

    /// Converts the pattern into an owned version with a `'static` lifetime.
    ///
    /// Any borrowed data is cloned into owned memory, ensuring that the returned
    /// pattern no longer depends on the original lifetimes.  
    /// This is useful when the pattern needs to outlive temporary references.
    pub fn into_owned(self) -> GenericBinaryPattern<'static> {
        GenericBinaryPattern {
            atoms: self.atoms.into_owned().into(),
            byte_sequence: self.byte_sequence.into_owned().into(),
        }
    }
}

impl BinaryPattern for GenericBinaryPattern<'_> {
    fn atoms(&self) -> &[Atom] {
        self.atoms.as_ref()
    }

    fn byte_sequence(&self) -> &[u8] {
        self.byte_sequence.as_ref()
    }
}
