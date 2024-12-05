use alloc::vec::Vec;

use crate::{
    pattern::{
        BinaryPattern,
        OwnedBinaryPattern,
    },
    Atom,
};

struct Branch {
    /// Index of the branch atom
    atom_index: usize,

    /// Index of the right branch
    right_index: usize,

    /// Index of when the branch ends
    end_index: usize,
}

struct Optimizer {
    atoms: Vec<Atom>,
    bytes: Vec<u8>,

    branches: Vec<Branch>,
}

impl Optimizer {
    pub fn new(pattern: &dyn BinaryPattern) -> Self {
        let mut branches = Vec::with_capacity(8);

        for (index, atom) in pattern.atoms().iter().enumerate() {
            let Atom::Branch {
                left_len,
                right_len,
            } = atom
            else {
                continue;
            };

            branches.push(Branch {
                atom_index: index,
                right_index: index + 1 + *left_len as usize,
                end_index: index + 1 + *left_len as usize + *right_len as usize,
            });
        }

        Self {
            atoms: pattern.atoms().to_vec(),
            bytes: pattern.byte_sequence().to_vec(),
            branches,
        }
    }

    pub fn optimize(mut self) -> OwnedBinaryPattern {
        while self.atoms.len() > 1 {
            if self.join_byte_sequences() {
                continue;
            }

            if self.join_wildcards_fixed() {
                continue;
            }

            if self.join_wildcards_ranged() {
                continue;
            }

            if self.join_wildcards() {
                continue;
            }

            /* finished optimizing */
            break;
        }

        self.fixup_branches();

        OwnedBinaryPattern::new(self.atoms, self.bytes)
    }

    fn fixup_branches(&mut self) {
        for branch in self.branches.iter() {
            self.atoms[branch.atom_index] = Atom::Branch {
                left_len: (branch.right_index - branch.atom_index - 1) as u16,
                right_len: (branch.end_index - branch.right_index) as u16,
            };
        }
    }

    fn iter_merge_pair(&mut self, merger: impl Fn(&Atom, &Atom) -> Option<Atom>) -> bool {
        let mut updated = false;
        let mut index = 0;
        while index + 1 < self.atoms.len() {
            let is_across_branch = self
                .branches
                .iter()
                .any(|branch| index + 1 == branch.right_index || index + 1 == branch.end_index);
            if is_across_branch {
                index += 1;
                continue;
            }

            let left = &self.atoms[index];
            let right = &self.atoms[index + 1];

            if let Some(new) = merger(left, right) {
                self.atoms[index] = new;
                self.atoms.remove(index + 1);

                for branch in self.branches.iter_mut() {
                    if branch.atom_index > index {
                        branch.atom_index -= 1;
                    }

                    if branch.right_index > index {
                        branch.right_index -= 1;
                    }

                    if branch.end_index > index {
                        branch.end_index -= 1;
                    }
                }

                updated = true;
            } else {
                index += 1;
            }
        }

        updated
    }

    fn join_byte_sequences(&mut self) -> bool {
        self.iter_merge_pair(|left, right| {
            let Atom::ByteSequence {
                seq_start: left_start,
                seq_end: left_end,
            } = *left
            else {
                return None;
            };

            let Atom::ByteSequence {
                seq_start: right_start,
                seq_end: right_end,
            } = *right
            else {
                return None;
            };

            if left_end == right_start {
                Some(Atom::ByteSequence {
                    seq_start: left_start,
                    seq_end: right_end,
                })
            } else {
                None
            }
        })
    }

    fn join_wildcards_fixed(&mut self) -> bool {
        self.iter_merge_pair(|left, right| {
            let Atom::WildcardFixed(left_value) = *left else {
                return None;
            };
            let Atom::WildcardFixed(right_value) = *right else {
                return None;
            };
            Some(Atom::WildcardFixed(left_value + right_value))
        })
    }

    fn join_wildcards_ranged(&mut self) -> bool {
        self.iter_merge_pair(|left, right| {
            let Atom::WildcardRange {
                min: left_min,
                max: left_max,
            } = *left
            else {
                return None;
            };

            let Atom::WildcardRange {
                min: right_min,
                max: right_max,
            } = *right
            else {
                return None;
            };

            Some(Atom::WildcardRange {
                min: left_min + right_min,
                max: left_max + right_max,
            })
        })
    }

    fn join_wildcards(&mut self) -> bool {
        self.iter_merge_pair(|left, right| {
            if let Atom::WildcardFixed(fixed) = *left {
                let Atom::WildcardRange { min, max } = *right else {
                    return None;
                };

                Some(Atom::WildcardRange {
                    min: min + fixed,
                    max: max + fixed,
                })
            } else if let Atom::WildcardRange { min, max } = *left {
                let Atom::WildcardFixed(fixed) = *right else {
                    return None;
                };

                Some(Atom::WildcardRange {
                    min: min + fixed,
                    max: max + fixed,
                })
            } else {
                None
            }
        })
    }
}

/// Optimize a [BinaryPattern] to increase matching performance.  
/// The following optimazations are performed:  
/// - Join byte sequence matches
/// - Join ranged wildcards
/// - Join fixed wildcards
/// - Join fixed and ranged wildcards
pub fn optimize_pattern(pattern: &dyn BinaryPattern) -> OwnedBinaryPattern {
    let optimizer = Optimizer::new(pattern);
    optimizer.optimize()
}

#[cfg(test)]
mod test {
    use crate::{
        pattern::{
            BinaryPattern,
            OwnedBinaryPattern,
        },
        Atom,
    };

    fn test_optimize(input: &[Atom], expected: &[Atom]) {
        println!("Testing: {:?}", input);
        let result = super::optimize_pattern(&OwnedBinaryPattern::new(input.to_vec(), vec![]));
        assert_eq!(result.atoms(), expected);
        println!(" -> success");
    }

    #[test]
    fn test_empty() {
        test_optimize(&[], &[]);
        test_optimize(&[Atom::WildcardFixed(1)], &[Atom::WildcardFixed(1)]);
    }

    #[test]
    fn test_byte_sequence() {
        test_optimize(
            &[
                Atom::ByteSequence {
                    seq_start: 0,
                    seq_end: 1,
                },
                Atom::ByteSequence {
                    seq_start: 1,
                    seq_end: 3,
                },
            ],
            &[Atom::ByteSequence {
                seq_start: 0,
                seq_end: 3,
            }],
        );

        test_optimize(
            &[
                Atom::ByteSequence {
                    seq_start: 0,
                    seq_end: 1,
                },
                Atom::ByteSequence {
                    seq_start: 2,
                    seq_end: 4,
                },
            ],
            &[
                Atom::ByteSequence {
                    seq_start: 0,
                    seq_end: 1,
                },
                Atom::ByteSequence {
                    seq_start: 2,
                    seq_end: 4,
                },
            ],
        );

        test_optimize(
            &[
                Atom::ByteSequence {
                    seq_start: 2,
                    seq_end: 4,
                },
                Atom::ByteSequence {
                    seq_start: 4,
                    seq_end: 5,
                },
            ],
            &[Atom::ByteSequence {
                seq_start: 2,
                seq_end: 5,
            }],
        );

        test_optimize(
            &[
                Atom::ByteSequence {
                    seq_start: 2,
                    seq_end: 4,
                },
                Atom::ByteSequence {
                    seq_start: 4,
                    seq_end: 5,
                },
                Atom::ByteSequence {
                    seq_start: 5,
                    seq_end: 8,
                },
            ],
            &[Atom::ByteSequence {
                seq_start: 2,
                seq_end: 8,
            }],
        );

        test_optimize(
            &[
                Atom::ByteSequence {
                    seq_start: 0,
                    seq_end: 1,
                },
                Atom::CursorPush,
                Atom::ByteSequence {
                    seq_start: 1,
                    seq_end: 3,
                },
            ],
            &[
                Atom::ByteSequence {
                    seq_start: 0,
                    seq_end: 1,
                },
                Atom::CursorPush,
                Atom::ByteSequence {
                    seq_start: 1,
                    seq_end: 3,
                },
            ],
        );
    }

    #[test]
    fn test_wildcard_ranges() {
        test_optimize(
            &[
                Atom::WildcardRange { min: 0, max: 10 },
                Atom::WildcardRange { min: 5, max: 7 },
            ],
            &[Atom::WildcardRange { min: 5, max: 17 }],
        );

        test_optimize(
            &[
                Atom::WildcardRange { min: 0, max: 10 },
                Atom::WildcardRange { min: 5, max: 7 },
                Atom::WildcardRange { min: 2, max: 3 },
            ],
            &[Atom::WildcardRange { min: 7, max: 20 }],
        );
    }

    #[test]
    fn test_wildcard_fixed() {
        test_optimize(
            &[Atom::WildcardFixed(8), Atom::WildcardFixed(2)],
            &[Atom::WildcardFixed(10)],
        );

        test_optimize(
            &[
                Atom::WildcardFixed(8),
                Atom::WildcardFixed(2),
                Atom::WildcardFixed(8),
            ],
            &[Atom::WildcardFixed(18)],
        );
    }

    #[test]
    fn test_wildcard_fixed_ranges() {
        test_optimize(
            &[
                Atom::WildcardFixed(8),
                Atom::WildcardRange { min: 2, max: 10 },
            ],
            &[Atom::WildcardRange { min: 10, max: 18 }],
        );
        test_optimize(
            &[
                Atom::WildcardRange { min: 2, max: 10 },
                Atom::WildcardFixed(8),
            ],
            &[Atom::WildcardRange { min: 10, max: 18 }],
        );

        test_optimize(
            &[
                Atom::WildcardFixed(8),
                Atom::WildcardRange { min: 2, max: 10 },
                Atom::WildcardFixed(2),
            ],
            &[Atom::WildcardRange { min: 12, max: 20 }],
        );

        test_optimize(
            &[
                Atom::WildcardRange { min: 2, max: 10 },
                Atom::WildcardFixed(8),
                Atom::WildcardRange { min: 2, max: 10 },
            ],
            &[Atom::WildcardRange { min: 12, max: 28 }],
        );
    }

    #[test]
    fn test_merge_keep_branch() {
        test_optimize(
            &[
                Atom::Branch {
                    left_len: 2,
                    right_len: 1,
                },
                Atom::WildcardFixed(8),
                Atom::WildcardFixed(8),
                Atom::WildcardFixed(8),
            ],
            &[
                Atom::Branch {
                    left_len: 1,
                    right_len: 1,
                },
                Atom::WildcardFixed(16),
                Atom::WildcardFixed(8),
            ],
        );

        /* nested right branch */
        test_optimize(
            &[
                Atom::Branch {
                    left_len: 2,
                    right_len: 4,
                },
                Atom::WildcardFixed(8),
                Atom::WildcardFixed(8),
                Atom::Branch {
                    left_len: 2,
                    right_len: 1,
                },
                Atom::WildcardFixed(8),
                Atom::WildcardFixed(8),
                Atom::WildcardFixed(8),
            ],
            &[
                Atom::Branch {
                    left_len: 1,
                    right_len: 3,
                },
                Atom::WildcardFixed(16),
                Atom::Branch {
                    left_len: 1,
                    right_len: 1,
                },
                Atom::WildcardFixed(16),
                Atom::WildcardFixed(8),
            ],
        );

        /* nested left branch */
        test_optimize(
            &[
                Atom::Branch {
                    left_len: 4,
                    right_len: 2,
                },
                // outer left
                Atom::Branch {
                    left_len: 1,
                    right_len: 2,
                },
                // inner left
                Atom::WildcardFixed(8),
                // inner right
                Atom::WildcardFixed(8),
                Atom::WildcardFixed(8),
                // outer right
                Atom::WildcardFixed(8),
                Atom::WildcardFixed(8),
            ],
            &[
                Atom::Branch {
                    left_len: 3,
                    right_len: 1,
                },
                Atom::Branch {
                    left_len: 1,
                    right_len: 1,
                },
                Atom::WildcardFixed(8),
                Atom::WildcardFixed(16),
                Atom::WildcardFixed(16),
            ],
        );
    }
}
