use core::{
    fmt::Debug,
    ops::Range,
};

/// A PositionedError representing an error that is associated with a specific position in the given pattern.
#[derive(Debug, PartialEq)]
pub struct PositionedError<E: Debug + PartialEq> {
    position: Range<usize>,
    inner: E,
}

impl<E: Debug + PartialEq> PositionedError<E> {
    pub fn new(position: Range<usize>, inner: E) -> Self {
        Self { position, inner }
    }

    pub fn position(&self) -> &Range<usize> {
        &self.position
    }

    pub fn inner(&self) -> &E {
        &self.inner
    }
}
