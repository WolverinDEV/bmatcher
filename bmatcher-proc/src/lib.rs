use proc_macro::TokenStream;
use syn::parse_macro_input;

extern crate proc_macro;

mod macro_pattern;

/// Parse a binary pattern and generate an instance of [bmatcher::BorrowedBinaryPattern] at compile time.  
/// An exhausive overview of the pattern syntax and operads can be found here: [bpattern::doc_pattern_syntax].
///
/// # Example
/// ```rust,ignore
/// static MY_PATTERN: &dyn BinaryPattern = &pattern!("01 02 ? 03 [4]");
/// ```
#[proc_macro]
pub fn pattern(item: TokenStream) -> TokenStream {
    let item = parse_macro_input!(item);

    macro_pattern::pattern(item)
        .unwrap_or_else(|e| e.to_compile_error())
        .into()
}
