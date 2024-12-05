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
    ByteSequence { seq_start: u16, seq_end: u16 },

    /// Skip a fixed number of bytes.
    WildcardFixed(u16),
    /// Skip a variable number of bytes.
    WildcardRange { min: u16, max: u16 },

    /// Jump to the relative / absolute based on the binary data the current cursor location.
    Jump(JumpType),

    /// Read the data value of a specified size at the current cursors location and save it to the save stack.
    /// This also advances the data cursor by the specified size.
    Read(ReadWidth),

    /// Match any one of the two subexpressions
    /// and then continue where we left of.
    Branch {
        /// Length of the left subpattern
        left_len: u16,

        /// Length of the right subpattern
        right_len: u16,
    },

    /// Push the cursor location to the cursor stack
    CursorPush,
    /// Pop the cursor location from the cursor stack and advance by X bytes
    CursorPop { advance: u16 },

    /// Save the current cursor position to the save stack
    SaveCursor,
    /// Save the constant to the save stack.
    /// This can be usefull to save which branch has been taken.    
    SaveConstant(u32),
}
