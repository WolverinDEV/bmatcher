mod lexer;
use lexer::{Lexer, Token};

mod parser;
pub use parser::{parse_pattern, ParseError};

mod optimizer;
pub use optimizer::optimize_pattern;
