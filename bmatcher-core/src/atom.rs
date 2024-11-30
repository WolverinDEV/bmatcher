#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum JumpType {
    RelByte,
    RelDWord,
    AbsQWord,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum ReadWidth {
    Byte,
    Word,
    DWord,
}

/// An atom represents a single operation that the matcher should perform.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Atom {
    /// Match a sequence of bytes from the sequence array.
    ByteSequence { seq_start: usize, seq_end: usize },

    /// Skip a fixed number of bytes.
    WildcardFixed(usize),
    /// Skip a variable number of bytes.
    WildcardRange { min: usize, max: usize },

    /// Jump to the relative / absolute based on the binary data the current cursor location.
    Jump(JumpType),

    /// Read the data value of a specified size at the current cursors location and save it to the save stack.
    /// This also advances the data cursor by the specified size.
    Read(ReadWidth),

    /// Match any one of the two subexpressions
    /// and then continue where we left of.
    Branch {
        /// Length of the left subpattern
        left_len: usize,

        /// Length of the right subpattern
        right_len: usize,
    },

    /// Push the cursor location to the cursor stack
    CursorPush,
    /// Pop the cursor location from the cursor stack and advance by X bytes
    CursorPop { advance: usize },

    /// Save the current cursor position to the save stack
    SaveCursor,
    /// Save the constant to the save stack.
    /// This can be usefull to save which branch has been taken.    
    SaveConstant(u32),
}
