use alloc::vec::Vec;
use core::num::ParseIntError;

use super::{
    Lexer,
    PositionedError,
    Token,
};
use crate::{
    Atom,
    GenericBinaryPattern,
    JumpType,
    ReadWidth,
};

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ParseError {
    UnexpectedToken,
    UnexpectedEnd,

    MaskByteLenMismatch,

    BinaryValueInvalid,
    BinaryValueIncomplete,

    GroupNotClosed,
    BlockNotClosed,

    RangeBoundInvalid(ParseIntError),
    RangeEndMustBeGraterThenStart,

    SequenceTooLarge,
}

#[derive(Debug, Clone, Copy)]
enum ByteSegment {
    Value(u8),
    Whildcard,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Radix {
    Hex,
    Bin,
}

impl Radix {
    pub fn from_prefix(value: &str) -> Option<Self> {
        if value.starts_with("0b") {
            Some(Self::Bin)
        } else if value.starts_with("0x") {
            Some(Self::Hex)
        } else {
            None
        }
    }

    pub fn prefix(&self) -> &'static str {
        match self {
            Self::Bin => "0b",
            Self::Hex => "0x",
        }
    }

    pub fn segment_bit_width(&self) -> usize {
        match self {
            Self::Bin => 1,
            Self::Hex => 4,
        }
    }
}

pub struct PatternParser<'a> {
    lexer: Lexer<'a>,
    peeked_token: Option<Token<'a>>,

    byte_sequence: Vec<u8>,
    atoms: Vec<Atom>,
}

impl<'a> PatternParser<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            lexer: Lexer::new(input),
            peeked_token: None,

            atoms: Vec::with_capacity(128),
            byte_sequence: Vec::with_capacity(256),
        }
    }

    fn peek_token(&mut self) -> Option<&Token<'a>> {
        if self.peeked_token.is_none() {
            self.peeked_token = self.lexer.next_token();
        }

        self.peeked_token.as_ref()
    }

    fn pop_token(&mut self) -> Result<Token<'a>, PositionedError<ParseError>> {
        if let Some(token) = self.peeked_token.take() {
            Ok(token)
        } else if let Some(token) = self.lexer.next_token() {
            Ok(token)
        } else {
            Err(PositionedError::new(
                self.lexer.token_range(),
                ParseError::UnexpectedEnd,
            ))
        }
    }

    fn bytes_commit(
        &mut self,
        sequence_start: usize,
    ) -> Result<(u16, u16), PositionedError<ParseError>> {
        assert!(sequence_start <= self.byte_sequence.len());

        let bytes_end = self.byte_sequence.len();
        if sequence_start > u16::MAX as usize || bytes_end > u16::MAX as usize {
            return Err(PositionedError::new(
                self.lexer.token_range(),
                ParseError::SequenceTooLarge,
            ));
        }

        Ok((sequence_start as u16, bytes_end as u16))
    }

    pub fn parse(mut self) -> Result<GenericBinaryPattern<'static>, PositionedError<ParseError>> {
        /* parse until the end :) */
        let _ = self.parse_until(|_| false)?;
        Ok(GenericBinaryPattern::new(self.atoms, self.byte_sequence))
    }

    fn parse_until(
        &mut self,
        matcher: impl Fn(&Token<'a>) -> bool,
    ) -> Result<bool, PositionedError<ParseError>> {
        while let Some(token) = self.peek_token() {
            if matcher(token) {
                return Ok(true);
            }

            match token {
                Token::Text(_) => self.parse_bytes()?,
                Token::Whildcard => self.parse_wildcard()?,

                Token::PositionSave => self.parse_position_save()?,

                Token::JumpRel1 => self.parse_jump()?,
                Token::JumpRel4 => self.parse_jump()?,
                Token::JumpAbs64 => self.parse_jump()?,

                Token::Read1 => self.parse_read()?,
                Token::Read2 => self.parse_read()?,
                Token::Read4 => self.parse_read()?,

                Token::GroupOpen => self.parse_group()?,
                Token::RangeOpen => self.parse_range()?,

                _ => {
                    return Err(PositionedError::new(
                        self.lexer.token_range(),
                        ParseError::UnexpectedToken,
                    ))
                }
            }
        }

        Ok(false)
    }

    fn parse_bytes_with_radix(&mut self, radix: Radix) -> Result<(), PositionedError<ParseError>> {
        let Token::Text(value) = self.pop_token()? else {
            return Err(PositionedError::new(
                self.lexer.token_range(),
                ParseError::UnexpectedToken,
            ));
        };

        let radix_prefix = radix.prefix();
        let (chars_token_index, chars) = if value.starts_with(radix_prefix) {
            (
                self.lexer.token_range().start + radix_prefix.len(),
                value[radix_prefix.len()..].char_indices(),
            )
        } else {
            (self.lexer.token_range().start, value.char_indices())
        };

        let mut byte_segments = Vec::new();
        for (char_index, c) in chars {
            if c == '?' {
                byte_segments.push(ByteSegment::Whildcard);
                continue;
            }

            let Some(value) = c.to_digit(1 << radix.segment_bit_width()) else {
                return Err(PositionedError::new(
                    chars_token_index + char_index..chars_token_index + char_index + 1,
                    ParseError::BinaryValueInvalid,
                ));
            };

            byte_segments.push(ByteSegment::Value(value as u8));
        }

        let byte_chunk_size = 8 / radix.segment_bit_width();
        if byte_segments.len() % byte_chunk_size > 0 {
            return Err(PositionedError::new(
                self.lexer.token_range(),
                ParseError::BinaryValueIncomplete,
            ));
        }

        let bytes_start = self.byte_sequence.len();
        for chunk in byte_segments.chunks_exact(byte_chunk_size) {
            let mut byte = 0;
            for segment in chunk {
                byte <<= radix.segment_bit_width();
                if let ByteSegment::Value(bits) = *segment {
                    byte |= bits;
                }
            }

            self.byte_sequence.push(byte);
        }

        let (seq_start, seq_end) = self.bytes_commit(bytes_start)?;
        if byte_segments
            .iter()
            .any(|seg| matches!(seg, ByteSegment::Whildcard))
        {
            let bytes_mask_start = self.byte_sequence.len();
            for chunk in byte_segments.chunks_exact(byte_chunk_size) {
                let mut byte = 0;
                for segment in chunk {
                    byte <<= radix.segment_bit_width();
                    match segment {
                        ByteSegment::Value(_) => {
                            byte |= (1 << radix.segment_bit_width()) - 1;
                        }
                        ByteSegment::Whildcard => {
                            byte |= 0;
                        }
                    }
                }

                self.byte_sequence.push(byte);
            }

            let (mask_start, _mask_end) = self.bytes_commit(bytes_mask_start)?;
            self.atoms.push(Atom::ByteSequenceMasked {
                seq_start,
                mask_start,
                len: (byte_segments.len() / byte_chunk_size) as u16,
            });
        } else {
            self.atoms.push(Atom::ByteSequence { seq_start, seq_end });
        }
        Ok(())
    }

    fn parse_bytes(&mut self) -> Result<(), PositionedError<ParseError>> {
        let Some(Token::Text(value)) = self.peek_token() else {
            return Err(PositionedError::new(
                self.lexer.token_range(),
                ParseError::UnexpectedToken,
            ));
        };

        let radix = Radix::from_prefix(&value).unwrap_or(Radix::Hex);
        self.parse_bytes_with_radix(radix)
    }

    fn parse_position_save(&mut self) -> Result<(), PositionedError<ParseError>> {
        let Token::PositionSave = self.pop_token()? else {
            return Err(PositionedError::new(
                self.lexer.token_range(),
                ParseError::UnexpectedToken,
            ));
        };

        self.atoms.push(Atom::SaveCursor);
        Ok(())
    }

    fn parse_wildcard(&mut self) -> Result<(), PositionedError<ParseError>> {
        let Token::Whildcard = self.pop_token()? else {
            return Err(PositionedError::new(
                self.lexer.token_range(),
                ParseError::UnexpectedToken,
            ));
        };

        self.atoms.push(Atom::WildcardFixed(1));
        Ok(())
    }

    fn parse_read(&mut self) -> Result<(), PositionedError<ParseError>> {
        let read_width = match self.pop_token()? {
            Token::Read1 => ReadWidth::Byte,
            Token::Read2 => ReadWidth::Word,
            Token::Read4 => ReadWidth::DWord,
            _ => {
                return Err(PositionedError::new(
                    self.lexer.token_range(),
                    ParseError::UnexpectedToken,
                ))
            }
        };

        self.atoms.push(Atom::Read(read_width));
        Ok(())
    }

    fn parse_jump(&mut self) -> Result<(), PositionedError<ParseError>> {
        let (jump_type, width) = match self.pop_token()? {
            Token::JumpRel1 => (JumpType::RelByte, 1),
            Token::JumpRel4 => (JumpType::RelDWord, 4),
            Token::JumpAbs64 => (JumpType::AbsQWord, 8),
            _ => {
                return Err(PositionedError::new(
                    self.lexer.token_range(),
                    ParseError::UnexpectedToken,
                ))
            }
        };

        if matches!(self.peek_token(), Some(Token::BlockOpen)) {
            let _ = self.pop_token()?;
            self.atoms.push(Atom::CursorPush);
            self.atoms.push(Atom::Jump(jump_type));

            let block_start = self.lexer.token_range();
            if !self.parse_until(|token| matches!(token, Token::BlockClose))? {
                return Err(PositionedError::new(
                    block_start,
                    ParseError::BlockNotClosed,
                ));
            }

            self.atoms.push(Atom::CursorPop { advance: width });
            let _ = self.pop_token()?;
        } else {
            self.atoms.push(Atom::Jump(jump_type));
        }

        Ok(())
    }

    fn parse_group(&mut self) -> Result<(), PositionedError<ParseError>> {
        let Token::GroupOpen = self.pop_token()? else {
            return Err(PositionedError::new(
                self.lexer.token_range(),
                ParseError::UnexpectedToken,
            ));
        };

        let group_start = self.lexer.token_range();
        let mut branch_atoms = Vec::with_capacity(8);
        loop {
            let branch_atom_index = self.atoms.len();
            self.atoms.push(Atom::Branch {
                left_len: 0,
                right_len: 0,
            });

            if !self.parse_until(|token| matches!(token, Token::GroupClose | Token::GroupPipe))? {
                return Err(PositionedError::new(
                    group_start,
                    ParseError::GroupNotClosed,
                ));
            }

            if matches!(self.pop_token()?, Token::GroupClose) {
                /* group end, no more entries */
                self.atoms.remove(branch_atom_index);
                break;
            }

            let left_branch_len = self.atoms.len() - branch_atom_index - 1;
            if left_branch_len > u16::MAX as usize {
                return Err(PositionedError::new(
                    self.lexer.token_range(),
                    ParseError::SequenceTooLarge,
                ));
            }

            if let Atom::Branch { left_len, .. } = &mut self.atoms[branch_atom_index] {
                *left_len = left_branch_len as u16;
            } else {
                unreachable!("atom should be a branch");
            }

            branch_atoms.push(branch_atom_index);
        }

        let atom_count = self.atoms.len();
        for branch_atom_index in branch_atoms {
            if let Atom::Branch {
                left_len,
                right_len,
            } = &mut self.atoms[branch_atom_index]
            {
                let right_branch_len = atom_count - *left_len as usize - branch_atom_index - 1;
                if right_branch_len > u16::MAX as usize {
                    return Err(PositionedError::new(
                        self.lexer.token_range(),
                        ParseError::SequenceTooLarge,
                    ));
                }

                *right_len = right_branch_len as u16;
            } else {
                unreachable!("atom should be a branch");
            }
        }

        Ok(())
    }

    fn parse_range(&mut self) -> Result<(), PositionedError<ParseError>> {
        let Token::RangeOpen = self.pop_token()? else {
            return Err(PositionedError::new(
                self.lexer.token_range(),
                ParseError::UnexpectedToken,
            ));
        };

        let Token::Text(range_start) = self.pop_token()? else {
            return Err(PositionedError::new(
                self.lexer.token_range(),
                ParseError::UnexpectedToken,
            ));
        };

        let range_start = range_start.parse::<u16>().map_err(|err| {
            PositionedError::new(self.lexer.token_range(), ParseError::RangeBoundInvalid(err))
        })?;

        match self.pop_token()? {
            Token::RangeClose => {
                self.atoms.push(Atom::WildcardFixed(range_start));
                Ok(())
            }
            Token::RangeSeperator => {
                let Token::Text(range_end) = self.pop_token()? else {
                    return Err(PositionedError::new(
                        self.lexer.token_range(),
                        ParseError::UnexpectedToken,
                    ));
                };

                let range_end = range_end.parse::<u16>().map_err(|err| {
                    PositionedError::new(
                        self.lexer.token_range(),
                        ParseError::RangeBoundInvalid(err),
                    )
                })?;

                if range_end <= range_start {
                    return Err(PositionedError::new(
                        self.lexer.token_range(),
                        ParseError::RangeEndMustBeGraterThenStart,
                    ));
                }

                self.atoms.push(Atom::WildcardRange {
                    min: range_start,
                    max: range_end,
                });
                if !matches!(self.pop_token()?, Token::RangeClose) {
                    Err(PositionedError::new(
                        self.lexer.token_range(),
                        ParseError::UnexpectedToken,
                    ))
                } else {
                    Ok(())
                }
            }
            _ => Err(PositionedError::new(
                self.lexer.token_range(),
                ParseError::UnexpectedToken,
            )),
        }
    }
}

/// Parse the given string as pattern.
pub fn parse_pattern(
    pattern: &str,
) -> Result<GenericBinaryPattern<'static>, PositionedError<ParseError>> {
    let parser = PatternParser::new(pattern);
    parser.parse()
}

#[cfg(test)]
mod test {
    use super::PatternParser;
    use crate::{
        compiler::{
            parser::ParseError,
            PositionedError,
        },
        pattern::BinaryPattern,
        Atom,
        JumpType,
    };

    #[test]
    fn test_byte_sequence_bin() {
        {
            let parser = PatternParser::new("0b10011100");
            let result = parser.parse().unwrap();
            assert_eq!(
                result.atoms(),
                &[Atom::ByteSequence {
                    seq_start: 0,
                    seq_end: 1
                },]
            );
            assert_eq!(result.byte_sequence(), &[0b10011100]);
        }

        {
            let parser = PatternParser::new("0b1001110011110000");
            let result = parser.parse().unwrap();
            assert_eq!(
                result.atoms(),
                &[Atom::ByteSequence {
                    seq_start: 0,
                    seq_end: 2
                },]
            );
            assert_eq!(result.byte_sequence(), &[0b10011100, 0b11110000]);
        }
    }

    #[test]
    fn test_byte_sequence_hex() {
        {
            let parser = PatternParser::new("FF 00 12");
            let result = parser.parse().unwrap();
            assert_eq!(
                result.atoms(),
                &[
                    Atom::ByteSequence {
                        seq_start: 0,
                        seq_end: 1
                    },
                    Atom::ByteSequence {
                        seq_start: 1,
                        seq_end: 2
                    },
                    Atom::ByteSequence {
                        seq_start: 2,
                        seq_end: 3
                    }
                ]
            );
            assert_eq!(result.byte_sequence(), &[0xFF, 0x00, 0x12]);
        }

        {
            let parser = PatternParser::new("FF00 12");
            let result = parser.parse().unwrap();
            assert_eq!(
                result.atoms(),
                &[
                    Atom::ByteSequence {
                        seq_start: 0,
                        seq_end: 2
                    },
                    Atom::ByteSequence {
                        seq_start: 2,
                        seq_end: 3
                    }
                ]
            );
            assert_eq!(result.byte_sequence(), &[0xFF, 0x00, 0x12]);
        }

        {
            let parser = PatternParser::new("0xDEADBEEF");
            let result = parser.parse().unwrap();
            assert_eq!(
                result.atoms(),
                &[Atom::ByteSequence {
                    seq_start: 0,
                    seq_end: 4
                },]
            );
            assert_eq!(result.byte_sequence(), &[0xDE, 0xAD, 0xBE, 0xEF]);
        }

        {
            let parser = PatternParser::new("FF0");
            let result = parser.parse().unwrap_err();
            assert_eq!(
                &result,
                &PositionedError::new(0..3, ParseError::BinaryValueIncomplete)
            );
        }

        {
            let parser = PatternParser::new("FX");
            let result = parser.parse().unwrap_err();
            assert_eq!(
                &result,
                &PositionedError::new(1..2, ParseError::BinaryValueInvalid)
            );
        }
    }

    #[test]
    fn test_byte_sequence_mask_hex() {
        {
            let parser = PatternParser::new("A?");
            let result = parser.parse().unwrap();
            assert_eq!(
                result.atoms(),
                &[Atom::ByteSequenceMasked {
                    seq_start: 0,
                    mask_start: 1,
                    len: 1
                }]
            );
            assert_eq!(result.byte_sequence(), &[0xA0, 0xF0]);
        }

        {
            let parser = PatternParser::new("F?E?");
            let result = parser.parse().unwrap();
            assert_eq!(
                result.atoms(),
                &[Atom::ByteSequenceMasked {
                    seq_start: 0,
                    mask_start: 2,
                    len: 2
                }]
            );
            assert_eq!(result.byte_sequence(), &[0xF0, 0xE0, 0xF0, 0xF0]);
        }
    }

    #[test]
    fn test_byte_sequence_mask_bin() {
        {
            let parser = PatternParser::new("0b100??001");
            let result = parser.parse().unwrap();
            assert_eq!(
                result.atoms(),
                &[Atom::ByteSequenceMasked {
                    seq_start: 0,
                    mask_start: 1,
                    len: 1
                }]
            );
            assert_eq!(result.byte_sequence(), &[0x81, 0xE7]);
        }

        {
            let parser = PatternParser::new("0b100??001?????111");
            let result = parser.parse().unwrap();
            assert_eq!(
                result.atoms(),
                &[Atom::ByteSequenceMasked {
                    seq_start: 0,
                    mask_start: 2,
                    len: 2
                }]
            );
            assert_eq!(result.byte_sequence(), &[0x81, 0x07, 0xE7, 0x07]);
        }
    }

    #[test]
    fn test_byte_wildcard() {
        {
            let parser = PatternParser::new("?");
            let result = parser.parse().unwrap();
            assert_eq!(result.atoms(), &[Atom::WildcardFixed(1),]);
            assert_eq!(result.byte_sequence(), &[]);
        }

        {
            let parser = PatternParser::new("AB ? CD");
            let result = parser.parse().unwrap();
            assert_eq!(
                result.atoms(),
                &[
                    Atom::ByteSequence {
                        seq_start: 0x00,
                        seq_end: 0x01
                    },
                    Atom::WildcardFixed(1),
                    Atom::ByteSequence {
                        seq_start: 0x01,
                        seq_end: 0x02
                    }
                ]
            );
            assert_eq!(result.byte_sequence(), &[0xAB, 0xCD]);
        }

        /* double wildcards are being interpreted as nibbles */
        {
            let parser = PatternParser::new("AB ?? CD");
            let result = parser.parse().unwrap();
            assert_eq!(
                result.atoms(),
                &[
                    Atom::ByteSequence {
                        seq_start: 0x00,
                        seq_end: 0x01
                    },
                    Atom::ByteSequenceMasked {
                        seq_start: 0x01,
                        mask_start: 0x02,
                        len: 0x01
                    },
                    Atom::ByteSequence {
                        seq_start: 0x03,
                        seq_end: 0x04
                    }
                ]
            );
            assert_eq!(result.byte_sequence(), &[0xAB, 0x00, 0x00, 0xCD]);
        }

        /* this is just invalid */
        {
            let parser = PatternParser::new("AB ??? CD");
            let result = parser.parse().unwrap_err();
            assert_eq!(
                &result,
                &PositionedError::new(3..6, ParseError::BinaryValueIncomplete)
            );
        }
    }

    #[test]
    fn test_byte_wildcard_nl() {
        let parser = PatternParser::new("?\n?");
        let result = parser.parse().unwrap();
        assert_eq!(
            result.atoms(),
            &[Atom::WildcardFixed(1), Atom::WildcardFixed(1),]
        );
        assert_eq!(result.byte_sequence(), &[]);
    }

    #[test]
    fn test_jump() {
        {
            let parser = PatternParser::new("%$* FF * % $");
            let result = parser.parse().unwrap();
            assert_eq!(
                result.atoms(),
                &[
                    Atom::Jump(JumpType::RelByte),
                    Atom::Jump(JumpType::RelDWord),
                    Atom::Jump(JumpType::AbsQWord),
                    Atom::ByteSequence {
                        seq_start: 0,
                        seq_end: 1
                    },
                    Atom::Jump(JumpType::AbsQWord),
                    Atom::Jump(JumpType::RelByte),
                    Atom::Jump(JumpType::RelDWord),
                ]
            );
            assert_eq!(result.byte_sequence(), &[0xFF]);
        }
    }

    #[test]
    fn test_jump_block() {
        {
            let parser = PatternParser::new("$ { FE }");
            let result = parser.parse().unwrap();
            assert_eq!(
                result.atoms(),
                &[
                    Atom::CursorPush,
                    Atom::Jump(JumpType::RelDWord),
                    Atom::ByteSequence {
                        seq_start: 0,
                        seq_end: 1
                    },
                    Atom::CursorPop { advance: 4 },
                ]
            );
            assert_eq!(result.byte_sequence(), &[0xFE]);
        }

        {
            let parser = PatternParser::new("$ { FE $ { FF } }");
            let result = parser.parse().unwrap();
            assert_eq!(
                result.atoms(),
                &[
                    Atom::CursorPush,
                    Atom::Jump(JumpType::RelDWord),
                    Atom::ByteSequence {
                        seq_start: 0,
                        seq_end: 1
                    },
                    Atom::CursorPush,
                    Atom::Jump(JumpType::RelDWord),
                    Atom::ByteSequence {
                        seq_start: 1,
                        seq_end: 2
                    },
                    Atom::CursorPop { advance: 4 },
                    Atom::CursorPop { advance: 4 },
                ]
            );
            assert_eq!(result.byte_sequence(), &[0xFE, 0xFF]);
        }

        {
            let parser = PatternParser::new("$ { FE");
            let result = parser.parse().unwrap_err();
            assert_eq!(
                &result,
                &PositionedError::new(2..3, ParseError::BlockNotClosed)
            );
        }
    }

    #[test]
    fn test_group() {
        {
            /* empty group */
            let parser = PatternParser::new("()");
            let result = parser.parse().unwrap();
            assert_eq!(result.atoms(), &[]);
            assert_eq!(result.byte_sequence(), &[]);
        }

        {
            /* single entry group -> just ignores the group */
            let parser = PatternParser::new("( FF00 )");
            let result = parser.parse().unwrap();
            assert_eq!(
                result.atoms(),
                &[Atom::ByteSequence {
                    seq_start: 0,
                    seq_end: 2
                }]
            );
            assert_eq!(result.byte_sequence(), &[0xFF, 0x00]);
        }

        {
            /* group with two entries */
            let parser = PatternParser::new("( 01 | 02 03 )");
            let result = parser.parse().unwrap();
            assert_eq!(
                result.atoms(),
                &[
                    Atom::Branch {
                        left_len: 1,
                        right_len: 2
                    },
                    Atom::ByteSequence {
                        seq_start: 0,
                        seq_end: 1
                    },
                    Atom::ByteSequence {
                        seq_start: 1,
                        seq_end: 2
                    },
                    Atom::ByteSequence {
                        seq_start: 2,
                        seq_end: 3
                    }
                ]
            );
            assert_eq!(result.byte_sequence(), &[0x01, 0x02, 0x03]);
        }

        {
            /* group with tree entries */
            let parser = PatternParser::new("( 01 | 02 03 | FF )");
            let result = parser.parse().unwrap();
            assert_eq!(
                result.atoms(),
                &[
                    Atom::Branch {
                        left_len: 1,
                        right_len: 4
                    },
                    Atom::ByteSequence {
                        seq_start: 0,
                        seq_end: 1
                    },
                    Atom::Branch {
                        left_len: 2,
                        right_len: 1
                    },
                    Atom::ByteSequence {
                        seq_start: 1,
                        seq_end: 2
                    },
                    Atom::ByteSequence {
                        seq_start: 2,
                        seq_end: 3
                    },
                    Atom::ByteSequence {
                        seq_start: 3,
                        seq_end: 4
                    }
                ]
            );
            assert_eq!(result.byte_sequence(), &[0x01, 0x02, 0x03, 0xFF]);
        }

        {
            /* nested group (right) */
            let parser = PatternParser::new("( 01 | ( 02 | 03 ) )");
            let result = parser.parse().unwrap();
            assert_eq!(
                result.atoms(),
                &[
                    Atom::Branch {
                        left_len: 1,
                        right_len: 3
                    },
                    Atom::ByteSequence {
                        seq_start: 0,
                        seq_end: 1
                    },
                    Atom::Branch {
                        left_len: 1,
                        right_len: 1
                    },
                    Atom::ByteSequence {
                        seq_start: 1,
                        seq_end: 2
                    },
                    Atom::ByteSequence {
                        seq_start: 2,
                        seq_end: 3
                    }
                ]
            );
            assert_eq!(result.byte_sequence(), &[0x01, 0x02, 0x03]);
        }

        {
            /* nested group (left) */
            let parser = PatternParser::new("( (01 | 02) | 03 )");
            let result = parser.parse().unwrap();
            assert_eq!(
                result.atoms(),
                &[
                    Atom::Branch {
                        left_len: 3,
                        right_len: 1
                    },
                    Atom::Branch {
                        left_len: 1,
                        right_len: 1
                    },
                    Atom::ByteSequence {
                        seq_start: 0,
                        seq_end: 1
                    },
                    Atom::ByteSequence {
                        seq_start: 1,
                        seq_end: 2
                    },
                    Atom::ByteSequence {
                        seq_start: 2,
                        seq_end: 3
                    },
                ]
            );
            assert_eq!(result.byte_sequence(), &[0x01, 0x02, 0x03]);
        }

        {
            /* unclosed group */
            let parser = PatternParser::new("(");
            let result = parser.parse().unwrap_err();
            assert_eq!(
                &result,
                &PositionedError::new(0..1, ParseError::GroupNotClosed)
            );
        }

        {
            /* unclosed group with contents */
            let parser = PatternParser::new("( FF 00");
            let result = parser.parse().unwrap_err();
            assert_eq!(
                &result,
                &PositionedError::new(0..1, ParseError::GroupNotClosed)
            );
        }

        {
            /* unclosed group with pipe */
            let parser = PatternParser::new(" (|");
            let result = parser.parse().unwrap_err();
            assert_eq!(
                &result,
                &PositionedError::new(1..2, ParseError::GroupNotClosed)
            );
        }
    }

    #[test]
    fn test_range() {
        {
            /* widecard fixed */
            let parser = PatternParser::new("[0] [123]");
            let result = parser.parse().unwrap();
            assert_eq!(
                result.atoms(),
                &[Atom::WildcardFixed(0), Atom::WildcardFixed(123)]
            );
            assert_eq!(result.byte_sequence(), &[]);
        }

        {
            /* widecard range */
            let parser = PatternParser::new("[0-10] [123- 999]");
            let result = parser.parse().unwrap();
            assert_eq!(
                result.atoms(),
                &[
                    Atom::WildcardRange { min: 0, max: 10 },
                    Atom::WildcardRange { min: 123, max: 999 }
                ]
            );
            assert_eq!(result.byte_sequence(), &[]);
        }

        {
            /* widecard range error */
            let parser = PatternParser::new("[0-]");
            let result = parser.parse().unwrap_err();
            assert_eq!(
                &result,
                &PositionedError::new(3..4, ParseError::UnexpectedToken)
            );
        }

        {
            /* widecard range error */
            let parser = PatternParser::new("[FF 0-3]");
            let result = parser.parse().unwrap_err();
            assert_eq!(*result.position(), 1..3);
            assert!(matches!(result.inner(), ParseError::RangeBoundInvalid(_)));
        }

        {
            /* not closed */
            let parser = PatternParser::new("[0-3");
            let result = parser.parse().unwrap_err();
            assert_eq!(
                &result,
                &PositionedError::new(3..4, ParseError::UnexpectedEnd)
            );
        }

        {
            /* end less then start */
            let parser = PatternParser::new("[3-0]");
            let result = parser.parse().unwrap_err();
            assert_eq!(
                &result,
                &PositionedError::new(3..4, ParseError::RangeEndMustBeGraterThenStart)
            );
        }
    }
}
