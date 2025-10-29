use pelite::{
    image::IMAGE_SECTION_HEADER,
    pe32::{
        self,
        Pe as Pe32,
    },
    pe64::{
        self,
        headers::SectionHeader,
        Pe as Pe64,
    },
    PeFile,
    PeView,
    Wrap,
};

use crate::{
    MatchHint,
    MatchTarget,
};

trait AbstractPe {
    fn ape_size_of_image(&self) -> usize;
    fn ape_section_headers(&self) -> &[SectionHeader];
    fn ape_section_bytes(&self, section_header: &IMAGE_SECTION_HEADER) -> pelite::Result<&[u8]>;
    fn ape_slice(&self, rva: u32, min_size: usize, align: usize) -> pelite::Result<&[u8]>;
}

impl AbstractPe for PeView<'_> {
    fn ape_size_of_image(&self) -> usize {
        match self.optional_header() {
            Wrap::T32(inner) => inner.SizeOfImage as usize,
            Wrap::T64(inner) => inner.SizeOfImage as usize,
        }
    }

    fn ape_section_headers(&self) -> &[SectionHeader] {
        self.section_headers().as_slice()
    }

    fn ape_section_bytes(&self, section_header: &IMAGE_SECTION_HEADER) -> pelite::Result<&[u8]> {
        self.get_section_bytes(section_header)
    }

    fn ape_slice(&self, rva: u32, min_size: usize, align: usize) -> pelite::Result<&[u8]> {
        self.slice(rva, min_size, align)
    }
}

impl AbstractPe for PeFile<'_> {
    fn ape_size_of_image(&self) -> usize {
        match self.optional_header() {
            Wrap::T32(inner) => inner.SizeOfImage as usize,
            Wrap::T64(inner) => inner.SizeOfImage as usize,
        }
    }

    fn ape_section_headers(&self) -> &[SectionHeader] {
        self.section_headers().as_slice()
    }

    fn ape_section_bytes(&self, section_header: &IMAGE_SECTION_HEADER) -> pelite::Result<&[u8]> {
        self.get_section_bytes(section_header)
    }

    fn ape_slice(&self, rva: u32, min_size: usize, align: usize) -> pelite::Result<&[u8]> {
        self.slice(rva, min_size, align)
    }
}

impl AbstractPe for pe32::PeView<'_> {
    fn ape_size_of_image(&self) -> usize {
        self.optional_header().SizeOfImage as usize
    }

    fn ape_section_headers(&self) -> &[SectionHeader] {
        self.section_headers().as_slice()
    }

    fn ape_section_bytes(&self, section_header: &IMAGE_SECTION_HEADER) -> pelite::Result<&[u8]> {
        self.get_section_bytes(section_header)
    }

    fn ape_slice(&self, rva: u32, min_size: usize, align: usize) -> pelite::Result<&[u8]> {
        self.slice(rva, min_size, align)
    }
}

impl AbstractPe for pe32::PeFile<'_> {
    fn ape_size_of_image(&self) -> usize {
        self.optional_header().SizeOfImage as usize
    }

    fn ape_section_headers(&self) -> &[SectionHeader] {
        self.section_headers().as_slice()
    }

    fn ape_section_bytes(&self, section_header: &IMAGE_SECTION_HEADER) -> pelite::Result<&[u8]> {
        self.get_section_bytes(section_header)
    }

    fn ape_slice(&self, rva: u32, min_size: usize, align: usize) -> pelite::Result<&[u8]> {
        self.slice(rva, min_size, align)
    }
}

impl AbstractPe for &dyn pe32::PeObject<'_> {
    fn ape_size_of_image(&self) -> usize {
        self.optional_header().SizeOfImage as usize
    }

    fn ape_section_headers(&self) -> &[SectionHeader] {
        self.section_headers().as_slice()
    }

    fn ape_section_bytes(&self, section_header: &IMAGE_SECTION_HEADER) -> pelite::Result<&[u8]> {
        self.get_section_bytes(section_header)
    }

    fn ape_slice(&self, rva: u32, min_size: usize, align: usize) -> pelite::Result<&[u8]> {
        self.slice(rva, min_size, align)
    }
}

impl AbstractPe for pe64::PeView<'_> {
    fn ape_size_of_image(&self) -> usize {
        self.optional_header().SizeOfImage as usize
    }

    fn ape_section_headers(&self) -> &[SectionHeader] {
        self.section_headers().as_slice()
    }

    fn ape_section_bytes(&self, section_header: &IMAGE_SECTION_HEADER) -> pelite::Result<&[u8]> {
        self.get_section_bytes(section_header)
    }

    fn ape_slice(&self, rva: u32, min_size: usize, align: usize) -> pelite::Result<&[u8]> {
        self.slice(rva, min_size, align)
    }
}

impl AbstractPe for pe64::PeFile<'_> {
    fn ape_size_of_image(&self) -> usize {
        self.optional_header().SizeOfImage as usize
    }

    fn ape_section_headers(&self) -> &[SectionHeader] {
        self.section_headers().as_slice()
    }

    fn ape_section_bytes(&self, section_header: &IMAGE_SECTION_HEADER) -> pelite::Result<&[u8]> {
        self.get_section_bytes(section_header)
    }

    fn ape_slice(&self, rva: u32, min_size: usize, align: usize) -> pelite::Result<&[u8]> {
        self.slice(rva, min_size, align)
    }
}

impl AbstractPe for &dyn pe64::PeObject<'_> {
    fn ape_size_of_image(&self) -> usize {
        self.optional_header().SizeOfImage as usize
    }

    fn ape_section_headers(&self) -> &[SectionHeader] {
        self.section_headers().as_slice()
    }

    fn ape_section_bytes(&self, section_header: &IMAGE_SECTION_HEADER) -> pelite::Result<&[u8]> {
        self.get_section_bytes(section_header)
    }

    fn ape_slice(&self, rva: u32, min_size: usize, align: usize) -> pelite::Result<&[u8]> {
        self.slice(rva, min_size, align)
    }
}

fn match_hint<P: AbstractPe>(target: &P, offset: usize, byte_sequence: &[u8]) -> MatchHint {
    let sections = target.ape_section_headers();
    let sections = sections
        .iter()
        .skip_while(|section| offset >= (section.VirtualAddress + section.VirtualSize) as usize);

    for section in sections {
        let section_offset = if offset < section.VirtualAddress as usize {
            0
        } else {
            offset - section.VirtualAddress as usize
        };

        let Ok(section_bytes) = target.ape_section_bytes(&section) else {
            continue;
        };

        if section_offset >= section_bytes.len() {
            continue;
        }

        match (&section_bytes[section_offset..]).match_hint(0, byte_sequence) {
            MatchHint::MaybeMatch(offset) => {
                return MatchHint::MaybeMatch(
                    offset + section_offset + section.VirtualAddress as usize,
                )
            }
            MatchHint::NoMatches => continue,
            MatchHint::Unsupported => unreachable!(),
        }
    }

    MatchHint::NoMatches
}

fn subrange<P: AbstractPe>(target: &P, offset: usize, byte_count: usize) -> Option<&[u8]> {
    Some(&target.ape_slice(offset as u32, byte_count, 1).ok()?[..byte_count])
}

impl<P: AbstractPe> MatchTarget for P {
    fn match_length(&self) -> usize {
        self.ape_size_of_image()
    }

    fn match_hint(&self, offset: usize, byte_sequence: &[u8]) -> MatchHint {
        self::match_hint(self, offset, byte_sequence)
    }

    fn subrange(&self, offset: usize, byte_count: usize) -> Option<&[u8]> {
        self::subrange(self, offset, byte_count)
    }

    fn translate_absolute_address(&self, _address: u64) -> Option<usize> {
        /* not supported */
        None
    }
}

#[cfg(test)]
mod test {
    use pelite::pe64::PeFile;

    use crate::{
        MatchHint,
        MatchTarget,
    };

    #[test]
    fn pe_object_target() {
        let file_data = include_bytes!("../resources/hvloader.dll").to_vec();
        let target = PeFile::from_bytes(&file_data).unwrap();

        const SEC_TEXT_HDR: &[u8] = &[
            0xCC, 0xCC, 0xCC, 0xCC, 0xCC, 0xCC, 0xCC, 0xCC, 0x48, 0x89, 0x5C, 0x24, 0x08, 0x48,
            0x89, 0x6C, 0x24, 0x10, 0x48, 0x89, 0x74, 0x24, 0x18, 0x57, 0x48, 0x83, 0xEC, 0x20,
            0x48, 0x8B, 0x1D, 0xBD,
        ];

        let match_hint = target.match_hint(0x00, SEC_TEXT_HDR);
        assert_eq!(match_hint, MatchHint::MaybeMatch(0x1000));

        let match_hint = target.match_hint(0x8FE, SEC_TEXT_HDR);
        assert_eq!(match_hint, MatchHint::MaybeMatch(0x1000));

        let match_hint = target.match_hint(0x1001, SEC_TEXT_HDR);
        assert_eq!(match_hint, MatchHint::NoMatches);

        const SEC_DATA_SOMEWHERE: &[u8] = &[
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x56, 0x6D, 0x78, 0x42, 0x6F,
            0x6F, 0x74, 0x49, 0x6E, 0x66, 0x6F, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x80, 0xDA,
        ];

        let match_hint = target.match_hint(0x00, SEC_DATA_SOMEWHERE);
        assert_eq!(match_hint, MatchHint::MaybeMatch(0x12090));

        let match_hint = target.match_hint(0x1000, SEC_DATA_SOMEWHERE);
        assert_eq!(match_hint, MatchHint::MaybeMatch(0x12090));

        let match_hint = target.match_hint(0x1001, SEC_DATA_SOMEWHERE);
        assert_eq!(match_hint, MatchHint::MaybeMatch(0x12090));

        let match_hint = target.match_hint(0x12090, SEC_DATA_SOMEWHERE);
        assert_eq!(match_hint, MatchHint::MaybeMatch(0x12090));

        let match_hint = target.match_hint(0x12091, SEC_DATA_SOMEWHERE);
        assert_eq!(match_hint, MatchHint::NoMatches);

        const SEC_PDATA_END: &[u8] = &[
            0xF0, 0xEB, 0x00, 0x00, 0xF7, 0xEB, 0x00, 0x00, 0x0C, 0x0E, 0x01, 0x00, 0x00, 0xEC,
            0x00, 0x00, 0x6A, 0xEC, 0x00, 0x00, 0x14, 0x0E, 0x01, 0x00, 0x70, 0xEC, 0x00, 0x00,
            0x74, 0xEC, 0x00, 0x00, 0x2C, 0x0E, 0x01, 0x00, 0x80, 0xEC, 0x00, 0x00, 0x84, 0xEC,
            0x00, 0x00, 0x30, 0x0E, 0x01, 0x00,
        ];

        let match_hint = target.match_hint(0x00, SEC_PDATA_END);
        assert_eq!(match_hint, MatchHint::MaybeMatch(0x56690));

        let match_hint = target.match_hint(0x56691, SEC_PDATA_END);
        assert_eq!(match_hint, MatchHint::NoMatches);
    }
}
