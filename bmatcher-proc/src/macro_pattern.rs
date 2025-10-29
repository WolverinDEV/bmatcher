use std::io::{
    self,
    Cursor,
    Write,
};

use bmatcher_core::{
    compiler,
    Atom,
    BinaryPattern,
    JumpType,
    ReadWidth,
};
use proc_macro2::TokenStream;
use quote::ToTokens;
use syn::{
    parse2,
    Error,
    Expr,
    LitStr,
    Result,
};

fn emit_atom(output: &mut dyn Write, atom: &Atom) -> io::Result<()> {
    match atom {
        Atom::ByteSequence { seq_start, seq_end } => {
            write!(
                output,
                "bmatcher::Atom::ByteSequence{{ seq_start: 0x{:X}, seq_end: 0x{:X} }}",
                seq_start, seq_end
            )?;
        }

        Atom::ByteSequenceMasked {
            seq_start,
            mask_start,
            len,
        } => {
            write!(
                output,
                "bmatcher::Atom::ByteSequenceMasked{{ seq_start: 0x{:X}, mask_start: 0x{:X}, len: 0x{:X} }}",
                seq_start, mask_start, len
            )?;
        }

        Atom::WildcardFixed(value) => {
            write!(output, "bmatcher::Atom::WildcardFixed(0x{:X})", value)?;
        }
        Atom::WildcardRange { min, max } => {
            write!(
                output,
                "bmatcher::Atom::WildcardRange{{ min: 0x{:X}, max: 0x{:X} }}",
                min, max
            )?;
        }

        Atom::Jump(value) => {
            let inner = match value {
                JumpType::RelByte => "bmatcher::JumpType::RelByte",
                JumpType::RelDWord => "bmatcher::JumpType::RelDWord",
                JumpType::AbsQWord => "bmatcher::JumpType::AbsQWord",
            };
            write!(output, "bmatcher::Atom::Jump({})", inner)?;
        }
        Atom::Read(value) => {
            let inner = match value {
                ReadWidth::Byte => "bmatcher::ReadWidth::Byte",
                ReadWidth::Word => "bmatcher::ReadWidth::Word",
                ReadWidth::DWord => "bmatcher::ReadWidth::DWord",
            };
            write!(output, "bmatcher::Atom::Read({})", inner)?;
        }

        Atom::Branch {
            left_len,
            right_len,
        } => {
            write!(
                output,
                "bmatcher::Atom::Branch{{ left_len: 0x{:X}, right_len: 0x{:X} }}",
                left_len, right_len
            )?;
        }

        Atom::CursorPush => {
            write!(output, "bmatcher::Atom::CursorPush",)?;
        }
        Atom::CursorPop { advance } => {
            write!(
                output,
                "bmatcher::Atom::CursorPop{{ advance: 0x{:X} }}",
                advance
            )?;
        }

        Atom::SaveCursor => {
            write!(output, "bmatcher::Atom::SaveCursor",)?;
        }
        Atom::SaveConstant(value) => {
            write!(output, "bmatcher::Atom::SaveConstant(0x{:X})", value)?;
        }
    }

    Ok(())
}

fn emit_atoms(output: &mut dyn Write, atoms: &[Atom]) -> io::Result<()> {
    write!(output, "&[")?;

    if !atoms.is_empty() {
        self::emit_atom(output, &atoms[0])?;
    }

    if atoms.len() > 1 {
        for atom in &atoms[1..] {
            write!(output, ", ")?;
            self::emit_atom(output, atom)?;
        }
    }

    write!(output, "]")?;
    Ok(())
}

fn emit_byte_sequence(output: &mut dyn Write, byte_sequence: &[u8]) -> io::Result<()> {
    write!(output, "&[")?;

    if !byte_sequence.is_empty() {
        write!(output, "0x{:X}", byte_sequence[0])?;
    }

    if byte_sequence.len() > 1 {
        for byte in &byte_sequence[1..] {
            write!(output, ", 0x{:X}", byte)?;
        }
    }

    write!(output, "]")?;
    Ok(())
}

fn pattern_to_const_str(pattern: &dyn BinaryPattern) -> io::Result<String> {
    let mut result_buffer = Vec::<u8>::with_capacity(1024);
    {
        let mut writer = Cursor::new(&mut result_buffer);

        write!(&mut writer, "bmatcher::GenericBinaryPattern::new_const(")?;
        emit_atoms(&mut writer, pattern.atoms())?;
        write!(&mut writer, ", ")?;
        emit_byte_sequence(&mut writer, pattern.byte_sequence())?;
        write!(&mut writer, ")")?;
    }

    Ok(String::from_utf8(result_buffer).unwrap())
}

pub fn pattern(item: TokenStream) -> Result<TokenStream> {
    let pattern_str = parse2::<LitStr>(item)?;
    let pattern = match compiler::parse_pattern(&pattern_str.value()) {
        Ok(result) => result,
        Err(error) => {
            let error_span = pattern_str
                .token()
                .subspan(error.position().start + 1..error.position().end + 1)
                .unwrap_or(pattern_str.span());
            return Err(Error::new(error_span, format!("{}", error.inner())));
        }
    };

    let pattern = compiler::optimize_pattern(&pattern);
    let pattern = pattern_to_const_str(&pattern).unwrap();
    let pattern = syn::parse_str::<Expr>(&pattern)?;
    Ok(pattern.to_token_stream())
}
