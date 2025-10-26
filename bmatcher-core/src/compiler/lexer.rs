use core::{
    ops::Range,
    str::CharIndices,
};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub enum Token<'a> {
    /// A text value without any spaces (basically the fallback if no other token matched)
    Text(&'a str),

    /// A one byte wildcard token: "?"
    Whildcard,

    /// A range open token: "["
    RangeOpen,
    /// A range token: "-"
    RangeSeperator,
    /// A range open token: "]"
    RangeClose,

    /// A block open token: "{"
    BlockOpen,
    /// A block open token: "}"
    BlockClose,

    /// Group open token: "("
    GroupOpen,
    /// Group pipe token: "|"
    GroupPipe,
    /// Group close token: ")"
    GroupClose,

    /// One byte relative jump: %
    JumpRel1,
    /// Four byte relative jump: $
    JumpRel4,
    /// Absolute jump to target address: *
    JumpAbs64,

    /// Read 1 byte: r1
    Read1,
    /// Read 2 bytes: r2
    Read2,
    /// Read 4 bytes: r4
    Read4,

    /// Position save token: '
    PositionSave,
}

pub struct Lexer<'a> {
    input: &'a str,
    iter: CharIndices<'a>,

    token_start: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            input,
            iter: input.char_indices(),

            token_start: 0,
        }
    }

    pub fn token_range(&self) -> Range<usize> {
        self.token_start..self.iter.offset()
    }

    /// Get the token from a specific char if the character is causing a new token
    fn char_to_token(&self, value: char) -> Option<Token<'a>> {
        Some(match value {
            '{' => Token::BlockOpen,
            '}' => Token::BlockClose,

            '(' => Token::GroupOpen,
            '|' => Token::GroupPipe,
            ')' => Token::GroupClose,

            '[' => Token::RangeOpen,
            '-' => Token::RangeSeperator,
            ']' => Token::RangeClose,

            '%' => Token::JumpRel1,
            '$' => Token::JumpRel4,
            '*' => Token::JumpAbs64,

            '\'' => Token::PositionSave,

            _ => return None,
        })
    }

    pub fn next_token(&mut self) -> Option<Token<'a>> {
        loop {
            let Some((token_pos, token)) = self.iter.next() else {
                /* end reached */
                return None;
            };

            if token.is_whitespace() {
                /* we ignore all whitespaces */
                continue;
            }

            self.token_start = token_pos;
            if token == '/' {
                /* check for the beginning of a comment block */
                let next_token = self.iter.clone().next().map(|(_, v)| v);
                if matches!(next_token, Some('/')) {
                    /* single line comment */
                    loop {
                        let Some((_position, token)) = self.iter.next() else {
                            break;
                        };

                        if token == '\n' {
                            break;
                        }
                    }

                    continue;
                } else if matches!(next_token, Some('*')) {
                    /* multi line comment */
                    let mut star_index = 0;
                    loop {
                        let Some((position, token)) = self.iter.next() else {
                            break;
                        };

                        if token == '/' && star_index + 1 == position {
                            /* multi line comment end */
                            break;
                        }

                        if token == '*' {
                            star_index = position;
                            continue;
                        }
                    }

                    continue;
                }
            }

            if token == 'r' {
                let width = self.iter.clone().next().map_or('_', |(_, v)| v);
                let width = match width {
                    '1' => Some(Token::Read1),
                    '2' => Some(Token::Read2),
                    '4' => Some(Token::Read4),
                    _ => None,
                };

                if let Some(width) = width {
                    self.iter.next();
                    return Some(width);
                }
            }

            if token == '?' {
                let next = self.iter.clone().next().map_or(' ', |(_, v)| v);
                if next == ' ' {
                    return Some(Token::Whildcard);
                }
            }

            if let Some(token) = self.char_to_token(token) {
                return Some(token);
            }

            let mut iter_local = self.iter.clone();
            let token_end = loop {
                let Some((position, token)) = iter_local.next() else {
                    break iter_local.offset();
                };

                if token.is_whitespace() || self.char_to_token(token).is_some() {
                    /* whitespace and tokens always break the text token */
                    break position;
                }

                self.iter.next();
            };

            return Some(Token::Text(&self.input[token_pos..token_end]));
        }
    }
}

#[cfg(test)]
mod test {
    use std::iter;

    use super::{
        Lexer,
        Token,
    };

    fn execut_tests(test_cases: &[(&str, &[Token<'_>])]) {
        for (input, expected_output) in test_cases {
            let mut lexer = Lexer::new(input);
            let result = iter::from_fn(|| lexer.next_token()).collect::<Vec<_>>();

            assert_eq!(result.as_slice(), *expected_output);
        }
    }

    #[test]
    fn lexer_text() {
        self::execut_tests(&[
            ("0", &[Token::Text("0")]),
            ("00", &[Token::Text("00")]),
            ("12 3", &[Token::Text("12"), Token::Text("3")]),
            ("1213", &[Token::Text("1213")]),
            (
                "12[13",
                &[Token::Text("12"), Token::RangeOpen, Token::Text("13")],
            ),
        ]);
    }

    #[test]
    fn lexer_comments() {
        self::execut_tests(&[
            ("// Hello World", &[]),
            ("00 // Hello World", &[Token::Text("00")]),
            (
                "00 // Hello World
                01",
                &[Token::Text("00"), Token::Text("01")],
            ),
            (
                "00 /* Hello World
                does this work? */ 01",
                &[Token::Text("00"), Token::Text("01")],
            ),
        ]);
    }

    #[test]
    fn lexer_full_patterns() {
        self::execut_tests(&[(
            "
                    // Inst 1
                    49 83 ? 71
                    
                    // Inst 2
                    0F 86 $ { FE }
            ",
            &[
                Token::Text("49"),
                Token::Text("83"),
                Token::Whildcard,
                Token::Text("71"),
                Token::Text("0F"),
                Token::Text("86"),
                Token::JumpRel4,
                Token::BlockOpen,
                Token::Text("FE"),
                Token::BlockClose,
            ],
        )]);
    }
}
