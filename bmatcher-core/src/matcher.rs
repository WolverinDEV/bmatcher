use crate::{
    Atom,
    BinaryPattern,
    HeapStack,
    JumpType,
    MatchTarget,
    ReadWidth,
    Stack,
    StaticStack,
};

/// The `BinaryMatcher` is responsible for searching a [BinaryPattern] within a [MatchTarget].
///
/// Use [`BinaryMatcher::next_match`] to iterate through matches of the specified pattern.
pub struct BinaryMatcher<
    'a,
    S: Stack<u32> = StaticStack<0x10, u32>,
    C: Stack<usize> = StaticStack<0x10, usize>,
> {
    pattern_atoms: &'a [Atom],
    pattern_byte_sequence: &'a [u8],

    target: &'a dyn MatchTarget,

    match_offset: usize,

    save_stack: S,
    cursor_stack: C,
}

impl<'a> BinaryMatcher<'a> {
    /// Create a new BinaryMatcher instance with a statically allocated stack.
    /// The default stack size is 0x10 for the save and cursor stack.
    pub fn new(pattern: &'a dyn BinaryPattern, target: &'a dyn MatchTarget) -> Self {
        Self::new_with_stack(
            pattern,
            target,
            StaticStack::<0x10, u32>::new(),
            StaticStack::<0x10, usize>::new(),
        )
    }

    /// Create a new BinaryMatcher instance with a heap allocated save and cursor stack
    pub fn new_heap_stack(
        pattern: &'a dyn BinaryPattern,
        target: &'a dyn MatchTarget,
    ) -> BinaryMatcher<'a, HeapStack<u32>, HeapStack<usize>> {
        BinaryMatcher::new_with_stack(
            pattern,
            target,
            HeapStack::<u32>::new(),
            HeapStack::<usize>::new(),
        )
    }
}

impl<'a, S: Stack<u32>, C: Stack<usize>> BinaryMatcher<'a, S, C> {
    /// Create a new binary matcher and supply the save and cursor stacks on your own
    pub fn new_with_stack(
        pattern: &'a dyn BinaryPattern,
        target: &'a dyn MatchTarget,
        mut save_stack: S,
        cursor_stack: C,
    ) -> Self {
        save_stack.truncate(0);
        save_stack.push_value(0x00);

        Self {
            pattern_atoms: pattern.atoms(),
            pattern_byte_sequence: pattern.byte_sequence(),

            target,

            save_stack,
            cursor_stack,

            match_offset: 0,
        }
    }

    /// Matches the current atom and updates the data & atom cursor.
    /// Returns false if the atom does not match.
    fn match_atoms(&mut self, mut data_cursor: usize, atoms: &[Atom]) -> Option<usize> {
        let mut atom_cursor = 0;
        while atom_cursor < atoms.len() {
            match atoms[atom_cursor] {
                Atom::ByteSequence { seq_start, seq_end } => {
                    let expected_bytes =
                        &self.pattern_byte_sequence[seq_start as usize..seq_end as usize];
                    let actual_bytes = self.target.subrange(data_cursor, expected_bytes.len())?;

                    if expected_bytes
                        .iter()
                        .zip(actual_bytes.iter())
                        .any(|(expected, data)| *expected != *data)
                    {
                        return None;
                    }

                    atom_cursor += 1;
                    data_cursor += expected_bytes.len();
                }
                Atom::ByteSequenceMasked {
                    seq_start,
                    mask_start,
                    len,
                } => {
                    let target_bytes = &self.pattern_byte_sequence
                        [seq_start as usize..seq_start as usize + len as usize];

                    let target_mask = &self.pattern_byte_sequence
                        [mask_start as usize..mask_start as usize + len as usize];

                    let actual_bytes = self.target.subrange(data_cursor, target_mask.len())?;

                    let target_bytes = target_bytes
                        .iter()
                        .zip(target_mask)
                        .map(|(value, mask)| *value & *mask);
                    let actual_bytes = actual_bytes
                        .iter()
                        .zip(target_mask)
                        .map(|(value, mask)| *value & *mask);

                    if target_bytes
                        .zip(actual_bytes)
                        .any(|(expected, data)| expected != data)
                    {
                        return None;
                    }

                    atom_cursor += 1;
                    data_cursor += len as usize;
                }
                Atom::WildcardFixed(length) => {
                    atom_cursor += 1;
                    data_cursor += length as usize;
                }
                Atom::WildcardRange { min, max } => {
                    let save_stack_size = self.save_stack.len();
                    let cursor_stack_size = self.cursor_stack.len();

                    for offset in min..=max {
                        self.save_stack.truncate(save_stack_size);
                        self.cursor_stack.truncate(cursor_stack_size);
                        if let Some(data_cursor) = self
                            .match_atoms(data_cursor + offset as usize, &atoms[atom_cursor + 1..])
                        {
                            /* match :) */
                            return Some(data_cursor);
                        }
                    }

                    return None;
                }

                Atom::CursorPush => {
                    if !self.cursor_stack.push_value(data_cursor) {
                        /* TODO: Return error instead of abort search */
                        return None;
                    }

                    atom_cursor += 1;
                }
                Atom::CursorPop { advance } => {
                    let Some(value) = self.cursor_stack.pop_value() else {
                        /* TODO: Return error instead of abort search */
                        return None;
                    };

                    data_cursor = value + advance as usize;
                    atom_cursor += 1;
                }

                Atom::Branch {
                    left_len,
                    right_len,
                } => {
                    let save_stack_size = self.save_stack.len();
                    let cursor_stack_size = self.cursor_stack.len();

                    data_cursor = if let Some(data_cursor) = self.match_atoms(
                        data_cursor,
                        &atoms[atom_cursor + 1..atom_cursor + 1 + left_len as usize],
                    ) {
                        /* match for left hand side */
                        data_cursor
                    } else {
                        /* restore stack state and try right hand side */
                        self.save_stack.truncate(save_stack_size);
                        self.cursor_stack.truncate(cursor_stack_size);

                        self.match_atoms(
                            data_cursor,
                            &atoms[atom_cursor + 1 + left_len as usize
                                ..atom_cursor + 1 + left_len as usize + right_len as usize],
                        )?
                    };

                    atom_cursor += 1 + left_len as usize + right_len as usize;
                }

                Atom::Jump(mode) => {
                    data_cursor = match mode {
                        JumpType::RelByte => {
                            let value = self.target.subrange(data_cursor, 1)?;
                            (data_cursor + 1).wrapping_add_signed(value[0] as i8 as isize)
                        }
                        JumpType::RelDWord => {
                            let value = self.target.subrange(data_cursor, 4)?;
                            let value = i32::from_le_bytes(value.try_into().unwrap());
                            (data_cursor + 4).wrapping_add_signed(value as isize)
                        }
                        JumpType::AbsQWord => {
                            let value = self.target.subrange(data_cursor, 8)?;
                            let value = u64::from_le_bytes(value.try_into().unwrap());
                            self.target.translate_absolute_address(value)?
                        }
                    };
                    atom_cursor += 1;
                }

                Atom::Read(width) => {
                    let (value, width) = match width {
                        ReadWidth::Byte => {
                            let value = self.target.subrange(data_cursor, 1)?;
                            (value[0] as u32, 1)
                        }
                        ReadWidth::Word => {
                            let value = self.target.subrange(data_cursor, 2)?;
                            (u16::from_le_bytes(value.try_into().unwrap()) as u32, 2)
                        }
                        ReadWidth::DWord => {
                            let value = self.target.subrange(data_cursor, 4)?;
                            (u32::from_le_bytes(value.try_into().unwrap()), 4)
                        }
                    };
                    if !self.save_stack.push_value(value) {
                        /* TODO: Return error instead of abort search */
                        return None;
                    }

                    atom_cursor += 1;
                    data_cursor += width;
                }

                Atom::SaveCursor => {
                    if !self.save_stack.push_value(data_cursor as u32) {
                        /* TODO: Return error instead of abort search */
                        return None;
                    }
                    atom_cursor += 1;
                }
                Atom::SaveConstant(value) => {
                    if !self.save_stack.push_value(value) {
                        /* TODO: Return error instead of abort search */
                        return None;
                    }
                    atom_cursor += 1;
                }
            }
        }

        Some(data_cursor)
    }

    /// Finds the next match for the associated [BinaryPattern] within the [MatchTarget].
    ///
    /// # Returns
    /// - `Some(&[u32])` containing the saved stack.
    ///    The first element of the saved stack represents the start of the matched location.  
    ///    Subsequent elements can be pushed using the `Atom::SaveCursor` atom or the `'` command within the binary pattern.
    /// - `None` if no further matches are available.
    pub fn next_match(&mut self) -> Option<&[u32]> {
        for match_offset in self.match_offset..self.target.match_length() {
            self.save_stack.truncate(1);
            self.cursor_stack.truncate(0);

            if self.match_atoms(match_offset, self.pattern_atoms).is_none() {
                continue;
            }

            self.match_offset = match_offset + 1;

            let save_stack = self.save_stack.stack_mut();
            save_stack[0] = match_offset as u32;
            return Some(save_stack);
        }

        None
    }
}

#[cfg(test)]
mod test {
    use super::BinaryMatcher;
    use crate::{
        compiler::parse_pattern,
        BinaryPattern,
    };

    const DATA: &[u8] = &[
        0xCA, 0x70, 0x11, 0xB5, 0xA, 0x9D, 0x91, 0x83, 0xC4, 0x5A, 0xFC, 0xC7, 0x31, 0x26, 0xC3,
        0x48, 0x3D, 0x6C, 0x16, 0xD7, 0x15, 0x91, 0xDB, 0xC4, 0x21, 0x2, 0x31, 0x4D, 0xE9, 0xD5,
        0x52, 0xFB, 0xB7, 0x31, 0x91, 0x45, 0x35, 0xC7, 0xDA, 0xA9, 0x77, 0xFC, 0x9C, 0x3E, 0x65,
        0x19, 0xF2, 0x5A, 0x68, 0x99, 0x21, 0xC, 0xED, 0xDC, 0x21, 0x8C, 0xA2, 0x7B, 0xBA, 0xC0,
        0x9A, 0x94, 0x99, 0x9B, 0xB2, 0xB7, 0x69, 0x2D, 0x17, 0xA9, 0x85, 0x2C, 0xD7, 0x42, 0x43,
        0x91, 0xF6, 0x6E, 0x34, 0xBC, 0x2F, 0xF7, 0xAE, 0xAA, 0xAE, 0xBF, 0x4, 0xE5, 0xD5, 0x9B,
        0x13, 0x60, 0x17, 0x31, 0x87, 0xEF, 0xF1, 0x24, 0x43, 0xB4, 0x60, 0xBC, 0x9F, 0x16, 0x86,
        0x39, 0x3D, 0x9E, 0x1, 0x68, 0x74, 0x8D, 0xD3, 0xC8, 0x6, 0x25, 0x88, 0xB0, 0x95, 0x99,
        0xB4, 0x5D, 0xBE, 0x8B, 0xD3, 0x26, 0xCB, 0x3C,
    ];

    fn test_single(pattern: &str, data: &[u8], result: Option<&[u32]>) {
        let pattern = parse_pattern(pattern).unwrap();
        println!("Atoms: {:?}", pattern.atoms());

        let mut matcher = BinaryMatcher::new(&pattern, &data);
        assert_eq!(matcher.next_match(), result);
    }

    #[test]
    fn test_simple() {
        test_single("B7 69 2D", DATA, Some(&[0x41]));
        test_single("B7 69 ' 2D", DATA, Some(&[0x41, 0x43]));
        test_single("' B7 69 ' 2D", DATA, Some(&[0x41, 0x41, 0x43]));
        test_single("B7 69 3D", DATA, None);
    }

    #[test]
    fn test_binary_mask() {
        test_single("B7682D & FFFEFF", DATA, Some(&[0x41]));
    }

    #[test]
    fn test_range() {
        test_single("B7 69 2D [0-3] 85 2C '", DATA, Some(&[0x41, 0x48]));
        test_single("B7 69 2D [0-1] 85 2C '", DATA, None);
    }

    #[test]
    fn test_branch() {
        test_single("B7 (69 | 70) 2D", DATA, Some(&[0x41]));
        test_single("B7 (70 | 69) 2D", DATA, Some(&[0x41]));

        /* optional 0x70 */
        test_single("B7 (70 | ) 69 2D", DATA, Some(&[0x41]));
    }

    #[test]
    fn test_jmp() {
        test_single(
            "EB % FF",
            &[0x00, 0xEB, 0x01, 0xEE, 0xFF, 0xEE],
            Some(&[0x01]),
        );
        test_single("EB % EE", &[0x00, 0xEB, 0x01, 0xEE, 0xFF, 0xEE], None);
        test_single(
            "EB % EE",
            &[0x00, 0xEB, 0x00, 0xEE, 0xFF, 0xEE],
            Some(&[0x01]),
        );

        test_single(
            "E9 $ EE",
            &[0x00, 0xE9, 0x01, 0x00, 0x00, 0x00, 0xEE, 0xFF],
            None,
        );

        test_single(
            "E9 $ EE '",
            &[0x00, 0xE9, 0x01, 0x00, 0x00, 0x00, 0xEE, 0xEE],
            Some(&[0x01, 0x08]),
        );

        test_single(
            "E9 $ { EE FF } '",
            &[0x00, 0xE9, 0x01, 0x00, 0x00, 0x00, 0xEE, 0xEE, 0xFF],
            Some(&[0x01, 0x06]),
        );
    }
}
