/// A trait for targets that can be matched against a binary pattern.
///
/// Implementing this trait allows matching against data sources that may not be
/// continuous (e.g., fragmented data or packed representations), offering greater flexibility
/// compared to directly using a `u8` slice.
///
/// By default a `u8` slice does implement this trait.
pub trait MatchTarget {
    /// Returns the total length of the data which will be scaned when matching a binary pattern.
    fn match_length(&self) -> usize;

    /// Retrieves a subrange of the data, starting at the specified offset and spanning the given number of bytes.
    ///
    /// # Parameters
    /// - `offset`: The starting position within the data.
    /// - `byte_count`: The number of bytes to include in the subrange.
    ///
    /// # Returns
    /// An `Option` containing a slice of the data if the range is valid; otherwise, `None`.
    fn subrange(&self, offset: usize, byte_count: usize) -> Option<&[u8]>;

    /// Translates an absolute address to an index within the matchable data.
    ///
    /// # Parameters
    /// - `address`: The absolute address to translate.
    ///
    /// # Returns
    /// An `Option` containing the translated index if the address is valid; otherwise, `None`.
    fn translate_absolute_address(&self, address: u64) -> Option<usize>;
}

impl MatchTarget for &[u8] {
    fn match_length(&self) -> usize {
        self.len()
    }

    fn subrange(&self, offset: usize, byte_count: usize) -> Option<&[u8]> {
        if offset + byte_count > self.len() {
            return None;
        }

        Some(&self[offset..offset + byte_count])
    }

    fn translate_absolute_address(&self, address: u64) -> Option<usize> {
        let own_address = self.as_ptr() as u64;
        if own_address < address || address >= own_address + self.len() as u64 {
            None
        } else {
            Some((address - own_address) as usize)
        }
    }
}
